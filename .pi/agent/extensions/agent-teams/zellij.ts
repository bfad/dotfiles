/**
 * Zellij pane management for agent teams.
 *
 * Requires Zellij ≥ 0.44.0 for --pane-id targeting and list-panes support.
 * Requires Zellij ≥ 0.44.1 for --tab-id on new-pane (gracefully degrades
 * to using the focused tab on older versions).
 *
 * Layout mirrors the tmux approach:
 *   - Lead occupies the left portion of the terminal.
 *   - Teammates share the right portion, stacking vertically (using -d down).
 *
 * Pane IDs in Zellij are integers exposed via $ZELLIJ_PANE_ID.  The CLI
 * accepts them as "terminal_N" (or just "N" which defaults to terminal).
 * We store them as "terminal_N" strings for consistency.
 *
 * Key CLI commands used (all require ≥ 0.44.0):
 *   zellij action new-pane -d right|down [--tab-id N] -n <name> -- <cmd>
 *   zellij action close-pane --pane-id terminal_N
 *   zellij action rename-pane --pane-id terminal_N "<title>"
 *   zellij action list-panes --json
 *   zellij action current-tab-info
 *   zellij action focus-pane-id terminal_N
 */

import { execSync, spawn } from "node:child_process";
import type { PaneManager } from "./pane-manager.js";
import { shellEscape } from "./shared-utils.js";

/** Minimum Zellij version required for --pane-id and list-panes support. */
const MIN_ZELLIJ_VERSION = "0.44.0";

/** Minimum Zellij version required for --tab-id on new-pane. */
const MIN_ZELLIJ_TAB_ID_VERSION = "0.44.1";

const CLOSE_PANE_AFTER_PROCESS_EXIT_SCRIPT = `
const { execFileSync } = require("node:child_process");
const [parentPidText, paneId] = process.argv.slice(1);
const parentPid = Number(parentPidText);
function waitForParentExit() {
  try {
    process.kill(parentPid, 0);
    setTimeout(waitForParentExit, 50);
  } catch {
    try {
      execFileSync("zellij", ["action", "close-pane", "--pane-id", paneId], { stdio: "ignore" });
    } catch {}
  }
}
waitForParentExit();
`;

/** True when the process is running inside a Zellij session. */
export function isInZellij(): boolean {
	return !!process.env.ZELLIJ;
}

/**
 * Parse a semver-like version string into comparable parts.
 * Returns [major, minor, patch].
 */
function parseVersion(v: string): [number, number, number] {
	const parts = v.split(".").map(Number);
	return [parts[0] ?? 0, parts[1] ?? 0, parts[2] ?? 0];
}

/** True if version `a` is greater than or equal to version `b`. */
function versionGte(a: string, b: string): boolean {
	const [aMaj, aMin, aPat] = parseVersion(a);
	const [bMaj, bMin, bPat] = parseVersion(b);
	if (aMaj !== bMaj) return aMaj > bMaj;
	if (aMin !== bMin) return aMin > bMin;
	return aPat >= bPat;
}

/**
 * Get the installed Zellij version string (e.g. "0.44.0").
 * Returns null if zellij is not found or the version can't be parsed.
 */
export function getZellijVersion(): string | null {
	try {
		const output = execSync("zellij --version", { encoding: "utf-8" }).trim();
		// Output format: "zellij 0.44.0"
		const match = output.match(/(\d+\.\d+\.\d+)/);
		return match ? match[1] : null;
	} catch {
		return null;
	}
}

/**
 * Check that the running Zellij version meets the minimum requirement.
 * Throws a descriptive error if not.
 */
export function checkZellijVersion(): void {
	const version = getZellijVersion();
	if (!version) {
		throw new Error("Could not determine Zellij version. Is zellij installed?");
	}
	if (!versionGte(version, MIN_ZELLIJ_VERSION)) {
		throw new Error(
			`Zellij ${version} is too old. Agent teams requires Zellij ≥ ${MIN_ZELLIJ_VERSION} ` +
			`for --pane-id targeting and list-panes support. Please upgrade: brew upgrade zellij`,
		);
	}
}

/**
 * Check whether the running Zellij version supports `--tab-id` on `new-pane`.
 * This flag was added in 0.44.1.  On 0.44.0 we gracefully degrade by omitting
 * the flag (the pane opens in the currently focused tab instead).
 *
 * The result is cached after the first call since the version won't change
 * during a session.
 */
let _supportsTabId: boolean | null = null;
export function supportsTabIdOnNewPane(): boolean {
	if (_supportsTabId !== null) return _supportsTabId;
	const version = getZellijVersion();
	_supportsTabId = !!version && versionGte(version, MIN_ZELLIJ_TAB_ID_VERSION);
	return _supportsTabId;
}

/** Reset the --tab-id support cache (for testing). */
export function _resetTabIdCache(): void {
	_supportsTabId = null;
}

/**
 * Return the pane id of the currently focused pane.
 * Zellij sets $ZELLIJ_PANE_ID automatically as a bare integer.
 * We prefix it with "terminal_" for CLI use.
 */
export function getCurrentPaneId(): string {
	const id = process.env.ZELLIJ_PANE_ID;
	if (!id) throw new Error("ZELLIJ_PANE_ID not set — are we running inside Zellij?");
	return `terminal_${id}`;
}

/**
 * Split the lead pane to create the first teammate (right column).
 *
 * On Zellij ≥ 0.44.1 we use `--tab-id` to target the lead's tab explicitly
 * so the split always happens there, even if the user has switched to a
 * different tab since the session started.
 *
 * On older versions (0.44.0) we omit `--tab-id` and the pane opens in the
 * currently focused tab — not ideal but functional.
 *
 * We do NOT pass `-c` (close-on-exit) — panes should persist after the
 * command exits (matching tmux behavior) so the user can review output.
 */
export function splitForFirstTeammate(leadPaneId: string, command: string, name?: string): string {
	const nameFlag = name ? `-n ${shellEscape(name)}` : "";
	const tabFlag = getTabIdFlag(leadPaneId);
	// Snapshot the active tab BEFORE creating the pane, so we know
	// whether the user is on the lead's tab or somewhere else.
	const userOnSameTab = isUserOnPaneTab(leadPaneId);
	// Zellij's `new-pane --` exec's the command directly (no shell).
	// Use `bash -l -c` (login shell) so PATH includes nix-store and
	// homebrew paths.  Plain `bash -c` gives a minimal PATH in zellij.
	const paneId = execSync(
		`zellij action new-pane -d right ${tabFlag} ${nameFlag} -- bash -l -c ${shellEscape(command)}`,
		{ encoding: "utf-8" },
	).trim();
	// Zellij has no `-d` (detach) equivalent for new-pane, so it steals
	// focus on the same tab.  Refocus the lead — but only if the user
	// was on this tab (cross-tab new-pane doesn't steal focus).
	if (userOnSameTab) focusPane(leadPaneId);
	return normalizePaneId(paneId);
}

/**
 * Split an existing teammate pane vertically (stack below).
 *
 * On Zellij ≥ 0.44.1 we use `--tab-id` to target the teammate's tab
 * explicitly so the split always happens there regardless of current focus.
 * On older versions we omit it (pane opens in the focused tab).
 * No `-c` flag — panes persist after exit.
 */
export function splitForAdditionalTeammate(existingPaneId: string, command: string, name?: string): string {
	const nameFlag = name ? `-n ${shellEscape(name)}` : "";
	const tabFlag = getTabIdFlag(existingPaneId);
	const leadPaneId = getCurrentPaneId();
	// Snapshot the active tab BEFORE any focus manipulation.
	const userOnSameTab = isUserOnPaneTab(leadPaneId);
	// Focus the existing teammate pane first so that `-d down` splits
	// below it (not below the lead, which may have been refocused after
	// the previous spawn).  Only do this if we're on the same tab —
	// cross-tab focus-pane-id would yank the user.
	if (userOnSameTab) focusPane(existingPaneId);
	// Zellij's `new-pane --` exec's the command directly (no shell).
	// Use `bash -l -c` (login shell) so PATH includes nix-store and
	// homebrew paths.  Plain `bash -c` gives a minimal PATH in zellij.
	const paneId = execSync(
		`zellij action new-pane -d down ${tabFlag} ${nameFlag} -- bash -l -c ${shellEscape(command)}`,
		{ encoding: "utf-8" },
	).trim();
	// Refocus the lead — but only if the user was on this tab.
	if (userOnSameTab) focusPane(leadPaneId);
	return normalizePaneId(paneId);
}

/** Set a human-readable title on a pane (visible on the pane frame). */
export function setPaneTitle(paneId: string, title: string): void {
	try {
		execSync(`zellij action rename-pane --pane-id ${paneId} ${shellEscape(title)}`);
	} catch {
		/* ignore – not fatal */
	}
}

/**
 * Focus a specific pane by id.
 * Useful when you need to jump to a known pane (e.g. after multiple
 * splits where "previous" wouldn't be correct).
 */
export function focusPane(paneId: string): void {
	try {
		execSync(`zellij action focus-pane-id ${paneId}`);
	} catch {
		/* ignore – best effort */
	}
}

/**
 * Check whether the user is currently viewing the same tab as the given pane.
 * Uses `current-tab-info` which returns the ACTUALLY active tab (unlike
 * `is_focused` in list-panes which is per-tab and misleading).
 * Returns false on any error (safe default — skips focus manipulation).
 */
export function isUserOnPaneTab(paneId: string): boolean {
	try {
		const tabInfo = execSync("zellij action current-tab-info", { encoding: "utf-8" });
		const activeTabMatch = tabInfo.match(/^id:\s*(\d+)/m);
		if (!activeTabMatch) return false;
		const activeTabId = parseInt(activeTabMatch[1], 10);

		const json = execSync("zellij action list-panes --json", { encoding: "utf-8" });
		const panes: ZellijPaneInfo[] = JSON.parse(json);
		const numericId = extractNumericId(paneId);
		const pane = panes.find((p) => p.id === numericId && !p.is_plugin);
		if (!pane) return false;

		return activeTabId === pane.tab_id;
	} catch {
		return false;
	}
}

/** Kill a Zellij pane by id. */
export function killPane(paneId: string): void {
	try {
		execSync(`zellij action close-pane --pane-id ${paneId}`);
	} catch {
		/* pane may already be gone */
	}
}

/**
 * Close a held pane after the current Pi process exits. A detached helper
 * waits for the parent PID so every session_shutdown handler can finish first.
 */
export function closePaneAfterProcessExit(paneId: string): void {
	try {
		const helper = spawn(
			process.execPath,
			["-e", CLOSE_PANE_AFTER_PROCESS_EXIT_SCRIPT, String(process.pid), paneId],
			{ detached: true, stdio: "ignore" },
		);
		helper.on("error", () => {});
		helper.unref();
	} catch {
		/* best effort — graceful Pi shutdown must still proceed */
	}
}

/**
 * Check whether a pane is still alive by querying the pane list.
 * Parses `zellij action list-panes --json` and checks that the pane
 * exists and has not exited.
 */
export function isPaneAlive(paneId: string): boolean {
	try {
		const json = execSync("zellij action list-panes --json", { encoding: "utf-8" });
		const panes: ZellijPaneInfo[] = JSON.parse(json);
		const numericId = extractNumericId(paneId);
		const pane = panes.find((p) => p.id === numericId && !p.is_plugin);
		return !!pane && !pane.exited;
	} catch {
		return false;
	}
}

// ---------------------------------------------------------------------------
// PaneManager implementation
// ---------------------------------------------------------------------------

/** Capture pane content — not yet implemented for Zellij. */
export function capturePaneContent(_paneId: string, _lines: number): string | null {
	return null;
}

/** PaneManager backed by Zellij. */
export const zellijManager: PaneManager = {
	kind: "zellij",
	getCurrentPaneId,
	splitForFirstTeammate,
	splitForAdditionalTeammate,
	setPaneTitle,
	killPane,
	closePaneAfterProcessExit,
	isPaneAlive,
	capturePaneContent,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Look up the tab_id for a given pane by querying `list-panes --json`.
 * Returns `--tab-id N` if the running Zellij version supports it (≥ 0.44.1)
 * and the pane is found, or an empty string as a fallback (which lets zellij
 * use the currently focused tab).
 */
function getTabIdFlag(paneId: string): string {
	if (!supportsTabIdOnNewPane()) return "";
	try {
		const json = execSync("zellij action list-panes --json", { encoding: "utf-8" });
		const panes: ZellijPaneInfo[] = JSON.parse(json);
		const numericId = extractNumericId(paneId);
		const pane = panes.find((p) => p.id === numericId && !p.is_plugin);
		if (pane) return `--tab-id ${pane.tab_id}`;
	} catch {
		/* best-effort — fall back to default tab */
	}
	return "";
}

/**
 * Normalize a pane ID returned by Zellij.  The CLI may return just the
 * numeric ID or "terminal_N" — we always store "terminal_N".
 */
function normalizePaneId(raw: string): string {
	const trimmed = raw.trim();
	if (trimmed.startsWith("terminal_") || trimmed.startsWith("plugin_")) {
		return trimmed;
	}
	// Bare numeric ID — prefix with terminal_
	if (/^\d+$/.test(trimmed)) {
		return `terminal_${trimmed}`;
	}
	// Unexpected format — return as-is
	return trimmed;
}

/** Extract the numeric portion of a pane ID like "terminal_3" → 3. */
function extractNumericId(paneId: string): number {
	return parseInt(paneId.replace(/^terminal_/, ""), 10);
}

// ---------------------------------------------------------------------------
// Types for Zellij JSON output
// ---------------------------------------------------------------------------

/** Shape of a pane entry from `zellij action list-panes --json`. */
interface ZellijPaneInfo {
	id: number;
	is_plugin: boolean;
	is_focused: boolean;
	is_floating: boolean;
	title: string;
	exited: boolean;
	exit_status: number | null;
	is_held: boolean;
	pane_x: number;
	pane_y: number;
	pane_rows: number;
	pane_columns: number;
	tab_id: number;
}
