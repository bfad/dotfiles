/**
 * iTerm2 pane management for agent teams via AppleScript.
 *
 * Supports three layout modes:
 *   - "split"  — split the lead's pane (left/right, then stack vertically)
 *   - "tab"    — create a new tab for teammates (stack vertically within it)
 *   - "window" — create a new window for teammates (stack vertically within it)
 *
 * In "split" mode the first split direction is configurable via
 * `itermSplitDirection`: "auto" (default — side-by-side on landscape windows,
 * top/bottom on portrait/square windows), "right", or "down". Additional
 * teammates always stack top/bottom, which suits both orientations.
 *
 * Pane IDs are iTerm2 session unique IDs (UUIDs).
 */

import { execSync } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { PaneCreationObserver, PaneManager } from "./pane-manager.js";
import { createSpawnScript, shellEscape } from "./shared-utils.js";

export type ITermLayout = "split" | "tab" | "window";

export type ITermSplitDirection = "auto" | "right" | "down";

/**
 * Window aspect ratio (width/height) at or above which "auto" picks a
 * side-by-side first split. Below it (portrait or square-ish windows) the
 * lead pane is split top/bottom instead, which preserves usable line width.
 */
const AUTO_SPLIT_ASPECT_THRESHOLD = 1.4;

/** True when the process is running inside iTerm2 (and NOT inside tmux/zellij). */
export function isInITerm(): boolean {
	// Only use iTerm2 native panes when NOT already inside a multiplexer.
	if (process.env.TMUX || process.env.ZELLIJ) return false;
	return !!(process.env.ITERM_SESSION_ID || process.env.TERM_PROGRAM?.toLowerCase() === "iterm.app");
}

// ---------------------------------------------------------------------------
// Config file: ~/.pi/teams/config.json
// ---------------------------------------------------------------------------

const TEAMS_CONFIG_PATH = path.join(os.homedir(), ".pi", "teams", "config.json");

/**
 * Load the iTerm2 layout from ~/.pi/teams/config.json.
 *
 * Example config:
 * ```json
 * { "itermLayout": "tab" }
 * ```
 *
 * Valid values: "split" (default), "tab", "window".
 */
export function loadITermLayout(): ITermLayout {
	try {
		const raw = JSON.parse(fs.readFileSync(TEAMS_CONFIG_PATH, "utf-8"));
		const value = raw?.itermLayout?.toLowerCase();
		if (value === "tab" || value === "window" || value === "split") return value;
	} catch {
		/* file missing or invalid — use default */
	}
	return "split";
}

/**
 * Load the first-split direction for "split" layout from ~/.pi/teams/config.json.
 *
 * Example config:
 * ```json
 * { "itermSplitDirection": "down" }
 * ```
 *
 * Valid values: "auto" (default), "right", "down".
 */
export function loadITermSplitDirection(): ITermSplitDirection {
	try {
		const raw = JSON.parse(fs.readFileSync(TEAMS_CONFIG_PATH, "utf-8"));
		const value = raw?.itermSplitDirection?.toLowerCase();
		if (value === "auto" || value === "right" || value === "down") return value;
	} catch {
		/* file missing or invalid — use default */
	}
	return "auto";
}

/**
 * Parse an AppleScript window bounds string ("x1, y1, x2, y2") into a
 * width/height aspect ratio. Returns null when the input is malformed or
 * describes a degenerate rectangle.
 */
export function parseBoundsAspectRatio(raw: string): number | null {
	const parts = raw.split(",").map((part) => Number.parseFloat(part.trim()));
	if (parts.length !== 4 || parts.some((n) => !Number.isFinite(n))) return null;
	const width = parts[2] - parts[0];
	const height = parts[3] - parts[1];
	if (width <= 0 || height <= 0) return null;
	return width / height;
}

/**
 * Map a window aspect ratio to the first-split direction for "auto" mode.
 *
 * Landscape windows (aspect >= AUTO_SPLIT_ASPECT_THRESHOLD) split side-by-side
 * (iTerm2 "vertically"); portrait/square windows split top/bottom (iTerm2
 * "horizontally"). Unknown aspect (null/non-finite/non-positive) falls back to
 * side-by-side, matching the historical default.
 */
export function resolveAutoSplitDirection(aspect: number | null): "vertically" | "horizontally" {
	if (aspect === null || !Number.isFinite(aspect) || aspect <= 0) return "vertically";
	return aspect >= AUTO_SPLIT_ASPECT_THRESHOLD ? "vertically" : "horizontally";
}

// ---------------------------------------------------------------------------
// AppleScript helpers
// ---------------------------------------------------------------------------

function osascript(script: string): string {
	return execSync("osascript -e " + shellEscape(script), {
		encoding: "utf-8",
		timeout: 10_000,
	}).trim();
}

/** Escape a string for safe embedding in an AppleScript string literal. */
function appleScriptEscape(s: string): string {
	return s.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

const BOOTSTRAP_LOGIN_SHELL = `"${appleScriptEscape("bash -l")}"`;

function bestEffortCleanup(cleanup: () => void): void {
  try {
    cleanup();
  } catch {
    // Cleanup must not mask pane creation or dispatch failures.
  }
}

/**
 * Find a session by UUID across all windows/tabs and run an AppleScript
 * action on it.  Returns the action's result, "OK" for void actions
 * (where the action has no explicit `return`), or null if the session
 * was not found.
 */
function withSession(sessionId: string, action: string): string | null {
	const script = `
tell application "iTerm2"
	repeat with w in windows
		tell w
			repeat with t in tabs
				tell t
					repeat with s in sessions
						tell s
							if unique ID is "${sessionId}" then
								${action}
								return "OK"
							end if
						end tell
					end repeat
				end tell
			end repeat
		end tell
	end repeat
	return "NOT_FOUND"
end tell`;
	try {
		const result = osascript(script);
		return result === "NOT_FOUND" ? null : result;
	} catch {
		return null;
	}
}

/** Build the short command typed into the bootstrapped login shell. */
function spawnScriptLaunchCommand(filePath: string): string {
  return `exec bash ${shellEscape(filePath)}`;
}

// ---------------------------------------------------------------------------
// PaneManager interface implementation
// ---------------------------------------------------------------------------

/**
 * Return the session UUID of the current iTerm2 session.
 *
 * Uses $ITERM_SESSION_ID which has format "wNtNpN:UUID".
 * Falls back to querying the current session via AppleScript.
 */
export function getCurrentPaneId(): string {
	const envId = process.env.ITERM_SESSION_ID;
	if (envId) {
		// Format: "w0t0p0:UUID" — extract the UUID after the colon
		const parts = envId.split(":");
		if (parts.length >= 2) return parts[1];
	}
	// Fallback: ask iTerm2 directly
	return osascript(`
tell application "iTerm2"
	tell current window
		tell current tab
			tell current session
				return unique ID
			end tell
		end tell
	end tell
end tell`);
}

/**
 * Create the first teammate session.
 *
 * Behavior depends on layout mode:
 *   - "split"  — split the lead pane (direction per itermSplitDirection;
 *                "auto" picks side-by-side or top/bottom from window aspect)
 *   - "tab"    — create a new tab in the lead's window
 *   - "window" — create a new window
 *
 * The pane starts an explicit login shell, then receives only a short command
 * that opens a staged spawn script. The full teammate command never crosses
 * the interactive terminal's canonical-input boundary.
 * Returns the new session's UUID.
 */
function createSplitForSession(sessionId: string, direction: "vertically" | "horizontally"): string | null {
	const result = osascript(`
tell application "iTerm2"
	repeat with w in windows
		tell w
			repeat with t in tabs
				tell t
					repeat with s in sessions
						if unique ID of s is "${sessionId}" then
							tell s
								set newSession to (split ${direction} with default profile command ${BOOTSTRAP_LOGIN_SHELL})
								tell newSession
									return unique ID
								end tell
							end tell
						end if
					end repeat
				end tell
			end repeat
		end tell
	end repeat
	return "NOT_FOUND"
end tell`);
	return result === "NOT_FOUND" ? null : result;
}

function createCurrentSessionSplit(direction: "vertically" | "horizontally"): string {
	return osascript(`
tell application "iTerm2"
	tell current window
		tell current tab
			tell current session
				set newSession to (split ${direction} with default profile command ${BOOTSTRAP_LOGIN_SHELL})
				tell newSession
					return unique ID
				end tell
			end tell
		end tell
	end tell
end tell`);
}

/**
 * Best-effort aspect ratio of the window containing the given session.
 * Falls back to the current window when the session's window is not found.
 * Returns null on any AppleScript failure so callers can use the historical
 * default direction.
 */
function getWindowAspectRatio(sessionId: string): number | null {
	try {
		const raw = osascript(`
tell application "iTerm2"
	repeat with w in windows
		tell w
			repeat with t in tabs
				tell t
					repeat with s in sessions
						if unique ID of s is "${sessionId}" then
							return bounds of w
						end if
					end repeat
				end tell
			end repeat
		end tell
	end repeat
	return bounds of current window
end tell`);
		return parseBoundsAspectRatio(raw);
	} catch {
		return null;
	}
}

/**
 * Resolve the first teammate's split direction from the configured
 * preference, querying window geometry only when preference is "auto".
 */
function firstTeammateSplitDirection(
	preference: ITermSplitDirection,
	leadPaneId: string,
): "vertically" | "horizontally" {
	if (preference === "right") return "vertically";
	if (preference === "down") return "horizontally";
	return resolveAutoSplitDirection(getWindowAspectRatio(leadPaneId));
}

function startTeammateSession(sessionId: string, spawnScriptPath: string, name?: string): void {
	const safeName = appleScriptEscape(name ?? "teammate");
  const launchCommand = `"${appleScriptEscape(spawnScriptLaunchCommand(spawnScriptPath))}"`;
	const result = withSession(sessionId, `
set name to "${safeName}"
write text ${launchCommand}`);
	if (result === null) throw new Error(`Failed to start teammate in iTerm2 session ${sessionId}`);
}

function createFirstTeammate(
	layout: ITermLayout,
	splitDirection: ITermSplitDirection,
	leadPaneId: string,
	command: string,
	name?: string,
	observer?: PaneCreationObserver,
): string {
  const spawnScript = createSpawnScript(command);
  try {
    observer?.onStartupArtifactCreated?.(spawnScript.cleanup);
  } catch (error) {
    bestEffortCleanup(spawnScript.cleanup);
    throw error;
  }
	let newSessionId: string;
	try {
		if (layout === "tab") {
			// Find the lead's window by UUID and create a tab there,
			// rather than using "current window" which may differ if the
			// user has switched focus.
			newSessionId = osascript(`
tell application "iTerm2"
	repeat with w in windows
		tell w
			repeat with t in tabs
				tell t
					repeat with s in sessions
						if unique ID of s is "${leadPaneId}" then
							set newTab to (create tab with default profile command ${BOOTSTRAP_LOGIN_SHELL})
							tell current session of newTab
								return unique ID
							end tell
						end if
					end repeat
				end tell
			end repeat
		end tell
	end repeat
	-- Fallback: use frontmost window
	tell current window
		set newTab to (create tab with default profile command ${BOOTSTRAP_LOGIN_SHELL})
		tell current session of newTab
			return unique ID
		end tell
	end tell
end tell`);
		} else if (layout === "window") {
			newSessionId = osascript(`
tell application "iTerm2"
	set newWindow to (create window with default profile command ${BOOTSTRAP_LOGIN_SHELL})
	tell current session of current tab of newWindow
		return unique ID
	end tell
end tell`);
		} else {
			const direction = firstTeammateSplitDirection(splitDirection, leadPaneId);
			newSessionId = createSplitForSession(leadPaneId, direction) ?? createCurrentSessionSplit(direction);
		}
	} catch (error) {
    bestEffortCleanup(spawnScript.cleanup);
		observer?.onPaneCreationUncertain();
		throw error;
	}

	observer?.onPaneCreated(newSessionId);
  try {
    startTeammateSession(newSessionId, spawnScript.filePath, name);
  } catch (error) {
    bestEffortCleanup(spawnScript.cleanup);
    throw error;
  }
	return newSessionId;
}

/**
 * Stack an additional teammate below an existing one.
 *
 * Uses "split horizontally" which creates a top/bottom split in iTerm2.
 * This works the same regardless of layout mode — additional teammates
 * always stack vertically within whatever container the first one created.
 * Returns the new session's UUID.
 */
function createAdditionalTeammate(
	existingPaneId: string,
	command: string,
	name?: string,
	observer?: PaneCreationObserver,
): string {
  const spawnScript = createSpawnScript(command);
  try {
    observer?.onStartupArtifactCreated?.(spawnScript.cleanup);
  } catch (error) {
    bestEffortCleanup(spawnScript.cleanup);
    throw error;
  }
	let newSessionId: string;
	try {
		newSessionId =
			createSplitForSession(existingPaneId, "horizontally") ?? createCurrentSessionSplit("horizontally");
	} catch (error) {
    bestEffortCleanup(spawnScript.cleanup);
		observer?.onPaneCreationUncertain();
		throw error;
	}

	observer?.onPaneCreated(newSessionId);
  try {
    startTeammateSession(newSessionId, spawnScript.filePath, name);
  } catch (error) {
    bestEffortCleanup(spawnScript.cleanup);
    throw error;
  }
	return newSessionId;
}

/** Set a human-readable title on a session. */
export function setPaneTitle(paneId: string, title: string): void {
	const safeTitle = appleScriptEscape(title);
	withSession(paneId, `set name to "${safeTitle}"`);
}

/** Close an iTerm2 session by UUID. */
export function killPane(paneId: string): void {
	try {
		withSession(paneId, "close");
	} catch {
		/* session may already be dead */
	}
}

/** Check whether a session is still alive by searching for its UUID. */
export function isPaneAlive(paneId: string): boolean {
	if (!paneId || paneId === "none") return false;
	const result = withSession(paneId, 'return "ALIVE"');
	return result === "ALIVE";
}

/**
 * Capture the last N lines of terminal content from a session.
 *
 * iTerm2's `contents` property returns the full visible buffer.
 * We extract the last N non-empty lines.
 */
export function capturePaneContent(paneId: string, lines: number): string | null {
	if (!paneId || paneId === "none") return null;
	const content = withSession(paneId, "return contents");
	if (content === null) return null;

	// Filter out empty trailing lines and take the last N
	const allLines = content.split("\n");
	let end = allLines.length;
	while (end > 0 && allLines[end - 1].trim() === "") {
		end--;
	}
	const trimmed = allLines.slice(0, end);
	const lastN = trimmed.slice(-lines);
	return lastN.join("\n");
}

// ---------------------------------------------------------------------------
// PaneManager factory
// ---------------------------------------------------------------------------

/**
 * Create an iTerm2 PaneManager with the given layout mode.
 *
 * Layout modes:
 *   - "split"  — split the lead pane (default, matches tmux/zellij behavior)
 *   - "tab"    — teammates in a new tab (lead pane stays untouched)
 *   - "window" — teammates in a new window (lead pane stays untouched)
 */
function createITermManager(
	layout: ITermLayout = "split",
	splitDirection: ITermSplitDirection = "auto",
): PaneManager {
	return {
		kind: "iterm",
		getCurrentPaneId,
		splitForFirstTeammate: (leadPaneId, command, name, observer) =>
			createFirstTeammate(layout, splitDirection, leadPaneId, command, name, observer),
		splitForAdditionalTeammate: (existingPaneId, command, name, observer) =>
			createAdditionalTeammate(existingPaneId, command, name, observer),
		setPaneTitle,
		killPane,
		isPaneAlive,
		capturePaneContent,
	};
}

let _itermManager: PaneManager | null = null;

/** Get the iTerm2 pane manager (lazy-initialized on first call). */
export function getItermManager(): PaneManager {
	if (!_itermManager) {
		_itermManager = createITermManager(loadITermLayout(), loadITermSplitDirection());
	}
	return _itermManager;
}
