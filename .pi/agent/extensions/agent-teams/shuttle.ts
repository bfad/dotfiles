/**
 * Shuttle pane management for agent teams.
 *
 * Shuttle is a macOS workspace manager built on Ghostty.  It organises
 * terminals into workspaces → sessions → panes → tabs.
 *
 * Layout matches tmux/zellij:
 *   - Lead occupies the left ~⅓ of the terminal.
 *   - Teammates share the right ~⅔, stacking vertically.
 *
 * Implementation:
 *   - First teammate: `pane split right` from the lead, then
 *     `pane resize --ratio 0.33` on the parent container.
 *   - Additional teammates: `pane split down` from an existing teammate
 *     pane → stacking vertically in the right column.
 *
 * Pane IDs use Shuttle's hierarchical handles:
 *   workspace:W/session:S/pane:P
 *
 * Each split creates a new pane with a tab inside it.  We track the
 * **tab** handle as the "paneId" stored in the team config, since tabs
 * are what we send commands to and close.  Closing the last tab in a
 * split pane collapses the split automatically.
 *
 * Key CLI commands:
 *   shuttle pane split right|down --pane <pane> --json
 *   shuttle pane list --session <session> --json
 *   shuttle tab list --session <session> --json
 *   shuttle tab close --tab <tab> --json
 *   shuttle tab send --tab <tab> --text <text> --submit --json
 *   shuttle tab mark-attention --tab <tab> --message <text> --json
 */

import { execSync } from "node:child_process";
import type { PaneCreationObserver, PaneManager } from "./pane-manager.js";
import { shellEscape } from "./shared-utils.js";

// ---------------------------------------------------------------------------
// Detection
// ---------------------------------------------------------------------------

/** True when the process is running inside a Shuttle-managed terminal. */
export function isInShuttle(): boolean {
	return !!process.env.SHUTTLE_SESSION_ID;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Run a shuttle command with --json and parse the result. */
function shuttleJson(cmd: string): any {
	const raw = execSync(`shuttle ${cmd} --json`, { encoding: "utf-8" });
	return JSON.parse(raw);
}

/** Get the current session handle from the environment. */
function getSessionId(): string {
	const id = process.env.SHUTTLE_SESSION_ID;
	if (!id) throw new Error("SHUTTLE_SESSION_ID not set — are we running inside Shuttle?");
	return id;
}

/** Snapshot current tab IDs in the session. */
function snapshotTabIds(): Set<string> {
	const result = shuttleJson(`tab list --session ${getSessionId()}`);
	const tabs: any[] = result.data?.items ?? [];
	return new Set(tabs.map((t: any) => t.id));
}

/**
 * Extract the session handle from a tab handle.
 * workspace:W/session:S/tab:T → workspace:W/session:S
 */
function sessionFromTab(tabHandle: string): string {
	const parts = tabHandle.split("/");
	return parts.filter((p) => !p.startsWith("tab:")).join("/");
}

/**
 * Find which pane a tab lives in.
 * Returns the pane handle (workspace:W/session:S/pane:P).
 */
function paneForTab(tabHandle: string): string | null {
	const sessionId = sessionFromTab(tabHandle);
	const tabResult = shuttleJson(`tab list --session ${sessionId}`);
	const tabs: any[] = tabResult.data?.items ?? [];
	const tab = tabs.find((t: any) => t.id === tabHandle);
	if (!tab) return null;

	const paneResult = shuttleJson(`pane list --session ${sessionId}`);
	const panes: any[] = paneResult.data?.items ?? [];
	const pane = panes.find((p: any) => p.raw_id === tab.pane_id);
	return pane?.id ?? null;
}

/**
 * Rebalance teammate panes so they share equal vertical space.
 * Shuttle uses nested binary splits, so for N teammates the k-th
 * container (1-indexed from top) needs ratio 1/(N-k+1).
 */
function rebalanceTeammatePanes(sessionId: string): void {
	try {
		const result = shuttleJson(`pane list --session ${sessionId}`);
		const panes: any[] = result.data?.items ?? [];
		// Find vertical split containers (split_direction === "down")
		const verticalSplits = panes
			.filter((p: any) => p.split_direction === "down")
			.sort((a: any, b: any) => a.pane_number - b.pane_number);
		if (verticalSplits.length === 0) return;
		// N teammates = number of vertical splits + 1
		const n = verticalSplits.length + 1;
		for (let k = 0; k < verticalSplits.length; k++) {
			const ratio = 1 / (n - k);
			shuttleJson(`pane resize --pane ${verticalSplits[k].id} --ratio ${ratio}`);
		}
	} catch {
		/* best-effort — uneven layout still works */
	}
}

/** Focus a tab, returning keyboard input to it. */
function focusTab(tabId: string): void {
	try {
		const sessionHandle = sessionFromTab(tabId);
		execSync(
			`shuttle tab focus --tab ${tabId} --session ${sessionHandle} --json`,
			{ encoding: "utf-8" },
		);
	} catch {
		/* best-effort — user can click to refocus */
	}
}

/**
 * Block until the shell in a newly-created tab is ready to accept input.
 * Uses `shuttle tab wait` to watch for the `%` prompt character (zsh) or
 * `$` (bash).  Falls back to a simple sleep if the wait times out.
 */
function waitForShellReady(tabId: string): void {
	try {
		execSync(
			`shuttle tab wait --tab ${tabId} --text '%' --mode screen --timeout-ms 5000 --json`,
			{ encoding: "utf-8" },
		);
		return;
	} catch {
		/* zsh prompt not found — try bash */
	}
	try {
		execSync(
			`shuttle tab wait --tab ${tabId} --text '$' --mode screen --timeout-ms 2000 --json`,
			{ encoding: "utf-8" },
		);
		return;
	} catch {
		/* fall through to sleep */
	}
	// Last resort: blind wait
	execSync("sleep 1");
}

// ---------------------------------------------------------------------------
// PaneManager implementation
// ---------------------------------------------------------------------------

/**
 * Return the tab handle of the current tab.
 * Shuttle sets $SHUTTLE_TAB_ID automatically (e.g. workspace:63/session:1/tab:1).
 */
export function getCurrentPaneId(): string {
	const id = process.env.SHUTTLE_TAB_ID;
	if (!id) throw new Error("SHUTTLE_TAB_ID not set — are we running inside Shuttle?");
	return id;
}

/**
 * Split the lead pane to the right.  The new (right) pane runs `command`.
 * Returns the new tab's handle (used as paneId in team config).
 */
export function splitForFirstTeammate(
	leadPaneId: string,
	command: string,
	_name?: string,
	observer?: PaneCreationObserver,
): string {
	// leadPaneId is actually a tab handle — find its pane
	const paneHandle = paneForTab(leadPaneId) ?? process.env.SHUTTLE_PANE_ID;
	if (!paneHandle) throw new Error("Cannot determine lead's Shuttle pane handle");

	// Snapshot tabs before split
	const beforeTabs = snapshotTabIds();

	// Split right from the lead pane
	let result: any;
	try {
		result = shuttleJson(`pane split right --pane ${paneHandle}`);
	} catch (error) {
		observer?.onPaneCreationUncertain();
		throw error;
	}

	// Find the new tab (the one that wasn't there before)
	const allTabs: any[] = result.data?.tabs ?? [];
	const newTab = allTabs.find((t: any) => !beforeTabs.has(t.id));
	const tabId: string | undefined = newTab?.id;
	if (!tabId) {
		observer?.onPaneCreationUncertain();
		throw new Error(`Failed to create split: ${JSON.stringify(result)}`);
	}
	observer?.onPaneCreated(tabId);

	// Resize the parent split container so the lead gets ~⅓ and teammates ~⅔,
	// matching the tmux/zellij layout.
	const panes: any[] = result.data?.panes ?? [];
	const parentContainer = panes.find((p: any) => p.split_direction === "right");
	if (parentContainer?.id) {
		try {
			shuttleJson(`pane resize --pane ${parentContainer.id} --ratio 0.33`);
		} catch {
			/* best-effort — layout still works at default 50/50 */
		}
	}

	// Wait for the shell in the new tab to be ready before sending the
	// command.  Without this, the text arrives before zsh finishes
	// initializing and the command never executes.
	waitForShellReady(tabId);

	// Send the command to the new tab.
	// Append "; exit" so the shell (and thus the pane) closes when pi exits,
	// matching tmux/zellij behavior where the pane dies with its process.
	execSync(
		`shuttle tab send --tab ${tabId} --text ${shellEscape(command + "; exit")} --submit --json`,
		{ encoding: "utf-8" },
	);

	// Rebalance teammate panes to equal height and return focus to the lead.
	rebalanceTeammatePanes(getSessionId());
	focusTab(leadPaneId);

	return tabId;
}

/**
 * Split an existing teammate's pane downward to stack a new teammate below.
 * Returns the new tab's handle.
 */
export function splitForAdditionalTeammate(
	existingTeammatePaneId: string,
	command: string,
	_name?: string,
	observer?: PaneCreationObserver,
): string {
	// existingTeammatePaneId is a tab handle — find its pane
	const paneHandle = paneForTab(existingTeammatePaneId);
	if (!paneHandle) throw new Error(`Cannot find pane for tab ${existingTeammatePaneId}`);

	// Snapshot tabs before split
	const beforeTabs = snapshotTabIds();

	// Split down from the existing teammate's pane
	let result: any;
	try {
		result = shuttleJson(`pane split down --pane ${paneHandle}`);
	} catch (error) {
		observer?.onPaneCreationUncertain();
		throw error;
	}

	// Find the new tab
	const allTabs: any[] = result.data?.tabs ?? [];
	const newTab = allTabs.find((t: any) => !beforeTabs.has(t.id));
	const tabId: string | undefined = newTab?.id;
	if (!tabId) {
		observer?.onPaneCreationUncertain();
		throw new Error(`Failed to create split: ${JSON.stringify(result)}`);
	}
	observer?.onPaneCreated(tabId);

	// Wait for the shell to be ready before sending the command.
	waitForShellReady(tabId);

	// Send the command (with "; exit" to close the pane when pi exits)
	execSync(
		`shuttle tab send --tab ${tabId} --text ${shellEscape(command + "; exit")} --submit --json`,
		{ encoding: "utf-8" },
	);

	// Rebalance teammate panes to equal height and return focus to the lead.
	rebalanceTeammatePanes(getSessionId());
	const leadTabId = process.env.SHUTTLE_TAB_ID;
	if (leadTabId) focusTab(leadTabId);

	return tabId;
}

/**
 * Set a title on a tab.  Shuttle tabs pick up their title from the
 * terminal (pi sets it automatically), so this is a no-op.  We avoid
 * using mark-attention here since that's reserved for user-actionable
 * notifications, not cosmetic labels.
 */
export function setPaneTitle(_paneId: string, _title: string): void {
	/* no-op — pi sets the terminal title which Shuttle picks up */
}

/** Close a Shuttle tab and clean up its parent pane/split if now empty. */
export function killPane(paneId: string): void {
	if (!paneId || paneId === "none") return;
	// Look up the underlying pane handle while the tab still exists.
	let paneHandle: string | null = null;
	try {
		paneHandle = paneForTab(paneId);
	} catch {
		/* tab may already be gone */
	}

	// Close the tab.
	try {
		execSync(`shuttle tab close --tab ${paneId} --json`, { encoding: "utf-8" });
	} catch {
		/* tab may already be gone */
	}

	// Best-effort: close the pane/split if it has no remaining tabs.
	// Shuttle should auto-collapse empty panes, but if it doesn't we
	// clean up explicitly to avoid orphaned split containers.
	if (paneHandle) {
		try {
			const sessionHandle = sessionFromTab(paneId);
			const tabResult = shuttleJson(`tab list --session ${sessionHandle}`);
			const remainingTabs: any[] = tabResult.data?.items ?? [];
			const paneResult = shuttleJson(`pane list --session ${sessionHandle}`);
			const panes: any[] = paneResult.data?.items ?? [];
			const pane = panes.find((p: any) => p.id === paneHandle);
			if (pane) {
				const tabsInPane = remainingTabs.filter((t: any) => t.pane_id === pane.raw_id);
				if (tabsInPane.length === 0) {
					execSync(`shuttle pane close --pane ${paneHandle} --json`, { encoding: "utf-8" });
				}
			}
		} catch {
			/* pane may have already collapsed — this is fine */
		}
	}
}

/**
 * Check whether a tab is still alive by listing tabs in its session.
 * Unlike tmux (where panes are destroyed when the process exits),
 * Shuttle tabs persist after the shell exits.  We must check exit
 * status — not just existence — to avoid reporting dead tabs as alive.
 */
export function isPaneAlive(paneId: string): boolean {
	if (!paneId || paneId === "none") return false;
	try {
		const sessionHandle = sessionFromTab(paneId);
		const result = shuttleJson(`tab list --session ${sessionHandle}`);
		const tabs: any[] = result.data?.items ?? [];
		const tab = tabs.find((t: any) => t.id === paneId);
		if (!tab) return false;
		// Shuttle tabs linger after the process exits with runtime_status
		// still set to "idle" until the Shuttle fix lands (adding "exited").
		// Check for the exited status so dead tabs aren't reported alive.
		if (tab.runtime_status === "exited") return false;
		return true;
	} catch {
		return false;
	}
}

/** Capture the last N lines of terminal content from a tab via `tab read`. */
export function capturePaneContent(paneId: string, lines: number): string | null {
	if (!paneId || paneId === "none") return null;
	try {
		const result = shuttleJson(`tab read --tab ${paneId} --mode screen --lines ${lines}`);
		return result.data?.text ?? null;
	} catch {
		return null;
	}
}

// ---------------------------------------------------------------------------
// Export
// ---------------------------------------------------------------------------

/** PaneManager backed by Shuttle. */
export const shuttleManager: PaneManager = {
	kind: "shuttle",
	getCurrentPaneId,
	splitForFirstTeammate,
	splitForAdditionalTeammate,
	setPaneTitle,
	killPane,
	isPaneAlive,
	capturePaneContent,
};
