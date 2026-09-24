/**
 * tmux pane management for agent teams.
 *
 * Layout:
 *   - Lead occupies the left ~1/3 of the terminal.
 *   - Teammates share the right ~2/3, stacking vertically.
 *
 * On first teammate spawn the current (lead) pane is split horizontally.
 * Subsequent teammates split the right column vertically.
 */

import { execSync } from "node:child_process";
import type { PaneLocation, PaneManager } from "./pane-manager.js";
import { shellEscape } from "./shared-utils.js";

/** True when the process is running inside a tmux session. */
export function isInTmux(): boolean {
	return !!process.env.TMUX;
}

/**
 * Return the pane id (%N) of the pane this process is running in.
 *
 * Prefer $TMUX_PANE — tmux sets this per-process to the pane the process
 * is running in (e.g. "%5").  This is correct even when the user has
 * navigated to a different tmux window.  `tmux display-message -p` would
 * return the *client's active pane* instead, which may be in another window.
 */
export function getCurrentPaneId(): string {
	if (process.env.TMUX_PANE) return process.env.TMUX_PANE;
	return execSync("tmux display-message -p '#{pane_id}'", { encoding: "utf-8" }).trim();
}

/**
 * Split the lead pane horizontally.  The new (right) pane gets ~67 % of the
 * width and runs `command`.  Returns the new pane id.
 */
export function splitForFirstTeammate(leadPaneId: string, command: string, _name?: string): string {
	return execSync(
		`tmux split-window -h -d -l 67% -t ${leadPaneId} -P -F '#{pane_id}' ${shellEscape(command)}`,
		{ encoding: "utf-8" },
	).trim();
}

/**
 * Split an existing teammate pane vertically so the new teammate stacks
 * below it.  Returns the new pane id.
 */
export function splitForAdditionalTeammate(existingTeammatePaneId: string, command: string, _name?: string): string {
	return execSync(
		`tmux split-window -v -d -t ${existingTeammatePaneId} -P -F '#{pane_id}' ${shellEscape(command)}`,
		{ encoding: "utf-8" },
	).trim();
}

/** Set a human-readable title on a pane (visible in tmux status). */
export function setPaneTitle(paneId: string, title: string): void {
	try {
		execSync(`tmux select-pane -t ${paneId} -T ${shellEscape(title)}`);
	} catch {
		/* ignore – not fatal */
	}
}

/** Kill a tmux pane by id. */
export function killPane(paneId: string): void {
	try {
		execSync(`tmux kill-pane -t ${paneId}`);
	} catch {
		/* pane may already be dead */
	}
}

/**
 * Check whether a pane is still alive.
 *
 * Uses `tmux list-panes -a` to enumerate every pane on the server and
 * exact-match on `#{pane_id}`. This is the correct primitive for pane
 * existence checks.
 *
 * Why not `tmux display-message -t <pane> -p '...'`?
 *   Its `-t` target resolution is forgiving: when the target doesn't
 *   resolve to a real pane, tmux silently falls back to the client's
 *   active pane and returns exit 0 with whatever the format evaluates
 *   to there. That makes `display-message` useless for liveness — it
 *   always reports "alive" for stale or bogus pane IDs. The resulting
 *   bug: `team_status` reported dead teammates as "✓ active" because
 *   their `%N` no longer existed but `display-message` still said OK.
 *
 * The `#{pane_dead}` flag catches the `remain-on-exit on` case, where
 * tmux keeps a pane visible after its command has exited. Without the
 * flag check, a zombie pane would be reported as alive.
 */
export function isPaneAlive(paneId: string): boolean {
	// Guard against sentinel values — "none" is used for members without a tmux pane (e.g. vscode)
	if (!paneId || paneId === "none") return false;
	try {
		const out = execSync(`tmux list-panes -a -F '#{pane_id} #{pane_dead}'`, {
			encoding: "utf-8",
		});
		for (const line of out.split("\n")) {
			const [id, dead] = line.split(" ");
			if (id === paneId) return dead === "0";
		}
		return false;
	} catch {
		return false;
	}
}

/** Return the tmux session/window containing a live pane, or null. */
export function getPaneLocation(paneId: string): PaneLocation | null {
	if (!paneId || paneId === "none") return null;
	try {
		const out = execSync(`tmux list-panes -a -F '#{pane_id} #{pane_dead} #{session_id} #{window_id}'`, {
			encoding: "utf-8",
		});
		for (const line of out.split("\n")) {
			const [id, dead, sessionId, windowId] = line.split(" ");
			if (id === paneId && dead === "0" && sessionId && windowId) return { sessionId, windowId };
		}
		return null;
	} catch {
		return null;
	}
}

// ---------------------------------------------------------------------------
// PaneManager implementation
// ---------------------------------------------------------------------------

/** Capture the last N lines of a tmux pane's visible content. */
export function capturePaneContent(paneId: string, lines: number): string | null {
	if (!paneId || paneId === "none") return null;
	try {
		return execSync(`tmux capture-pane -t ${paneId} -p -S -${lines}`, { encoding: "utf-8" });
	} catch {
		return null;
	}
}

/** PaneManager backed by tmux. */
export const tmuxManager: PaneManager = {
	kind: "tmux",
	getCurrentPaneId,
	splitForFirstTeammate,
	splitForAdditionalTeammate,
	setPaneTitle,
	killPane,
	isPaneAlive,
	getPaneLocation,
	capturePaneContent,
};
