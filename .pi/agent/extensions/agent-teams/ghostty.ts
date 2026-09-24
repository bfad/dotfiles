/**
 * Ghostty pane management for agent teams via AppleScript (macOS).
 *
 * Ghostty 1.3.0+ exposes a native scripting dictionary
 * (https://ghostty.org/docs/features/applescript) with the object model
 * application -> windows -> tabs -> terminals.  Splits are created with
 * `split <terminal> direction right|down`, commands are dispatched with
 * `input text` (paste-style, so a trailing "\n" submits the line), and
 * titles with `perform action "set_surface_title:<title>"`.
 *
 * Pane IDs are Ghostty terminal UUIDs.  Layout mirrors tmux/zellij: the
 * first teammate splits the lead pane to the right, additional teammates
 * stack downward off an existing teammate pane.
 *
 * Limitations:
 *   - macOS only (the AppleScript API does not exist on Linux builds).
 *   - The dictionary has no buffer-read command, so capturePaneContent
 *     always returns null and /team_diagnose is unavailable under Ghostty.
 *   - Terminal `name` is read-only in AppleScript; titles go through the
 *     set_surface_title action instead.
 */

import { execSync } from "node:child_process";
import type { PaneCreationObserver, PaneManager } from "./pane-manager.js";
import { createSpawnScript, shellEscape } from "./shared-utils.js";

/** Minimum Ghostty version whose AppleScript dictionary supports split/input. */
const MIN_APPLESCRIPT_VERSION = "1.3.0";

/**
 * True when the process is running inside Ghostty (and NOT inside tmux/zellij).
 *
 * The multiplexer guard treats any set value (including an empty string) as
 * "inside a multiplexer"; unset variables are the only pass-through.
 */
export function isInGhostty(): boolean {
	// Only use Ghostty native panes when NOT already inside a multiplexer.
	const multiplexers = [
		process.env.TMUX,
		process.env.ZELLIJ,
		process.env.SHUTTLE_SESSION_ID,
		process.env.CMUX_WORKSPACE_ID,
		process.env.HERDR_PANE_ID,
	];
	if (multiplexers.some((value) => value !== undefined)) return false;
	if (process.platform !== "darwin") return false;
	// Ghostty sets $TERM_PROGRAM inconsistently across versions; both
	// "ghostty" and "Ghostty" appear in the wild (see extensions/notify).
	if (process.env.TERM_PROGRAM?.toLowerCase() !== "ghostty") return false;
	return supportsAppleScript(process.env.TERM_PROGRAM_VERSION);
}

/** True when the given Ghostty version supports the AppleScript API. */
export function supportsAppleScript(version: string | undefined): boolean {
	const parsed = parseVersion(version);
	const minimum = parseVersion(MIN_APPLESCRIPT_VERSION);
	if (!parsed || !minimum) return false;
	if (parsed[0] !== minimum[0]) return parsed[0] > minimum[0];
	if (parsed[1] !== minimum[1]) return parsed[1] > minimum[1];
	return parsed[2] >= minimum[2];
}

function parseVersion(version: string | undefined): [number, number, number] | null {
	if (!version) return null;
	const parts = version.trim().split(".");
	if (parts.length < 3) return null;
	const nums = parts.slice(0, 3).map((part) => Number.parseInt(part, 10));
	if (nums.some((n) => !Number.isFinite(n))) return null;
	return [nums[0], nums[1], nums[2]];
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

function bestEffortCleanup(cleanup: () => void): void {
	try {
		cleanup();
	} catch {
		// Cleanup must not mask pane creation or dispatch failures.
	}
}

/**
 * Find a terminal by UUID across all windows/tabs and run an AppleScript
 * action on it.  Returns the action's result, "OK" for void actions
 * (where the action has no explicit `return`), or null if the terminal
 * was not found.
 */
function withTerminal(terminalId: string, action: string): string | null {
	const script = `
tell application "Ghostty"
	repeat with w in windows
		repeat with tb in tabs of w
			repeat with t in terminals of tb
				if id of t is "${terminalId}" then
					${action}
					return "OK"
				end if
			end repeat
		end repeat
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

/** Build the short command dispatched to the bootstrapped pane. */
function spawnScriptLaunchCommand(filePath: string): string {
	return `exec bash ${shellEscape(filePath)}`;
}

// ---------------------------------------------------------------------------
// PaneManager interface implementation
// ---------------------------------------------------------------------------

/**
 * Return the terminal UUID of the currently focused Ghostty terminal.
 *
 * Ghostty's AppleScript dictionary exposes no way to map a PID to a
 * terminal (no process-id property, no per-surface env var), so the lead
 * pane can only be identified by focus.  `ensureTeam()` calls this while
 * handling the first team_spawn; if the user has clicked into another
 * Ghostty pane before that point, the wrong terminal becomes the recorded
 * lead.  Same exposure as the iTerm2 backend's current-session fallback.
 */
export function getCurrentPaneId(): string {
	return osascript(`
tell application "Ghostty"
	set t to focused terminal of selected tab of front window
	return id of t
end tell`);
}

/**
 * Split a terminal and return the new terminal's UUID, or null when the
 * target terminal was not found.
 */
function createSplitForTerminal(
	terminalId: string,
	direction: "right" | "down",
): string | null {
	const result = osascript(`
tell application "Ghostty"
	repeat with w in windows
		repeat with tb in tabs of w
			repeat with t in terminals of tb
				if id of t is "${terminalId}" then
					set newTerminal to (split t direction ${direction})
					return id of newTerminal
				end if
			end repeat
		end repeat
	end repeat
	return "NOT_FOUND"
end tell`);
	return result === "NOT_FOUND" ? null : result;
}

/** Split the focused terminal, used when the lead pane lookup fails. */
function createCurrentTerminalSplit(direction: "right" | "down"): string {
	return osascript(`
tell application "Ghostty"
	set currentTerminal to focused terminal of selected tab of front window
	set newTerminal to (split currentTerminal direction ${direction})
	return id of newTerminal
end tell`);
}

function startTeammateTerminal(
	terminalId: string,
	spawnScriptPath: string,
	name?: string,
): void {
	const launchCommand = appleScriptEscape(spawnScriptLaunchCommand(spawnScriptPath));
	const titleLine = name
		? `perform action "set_surface_title:${appleScriptEscape(name)}" on t\n`
		: "";
	// `input text` is paste-style input, so the trailing newline submits the
	// line; a separate `send key "enter"` would arrive as a second keystroke.
	const result = withTerminal(
		terminalId,
		`${titleLine}input text "${launchCommand}\\n" to t`,
	);
	if (result === null) {
		throw new Error(`Failed to start teammate in Ghostty terminal ${terminalId}`);
	}
}

/**
 * Create the first teammate pane by splitting the lead pane to the right.
 *
 * The full teammate command is staged in an owner-only self-removing spawn
 * script; only a short `exec bash <script>` line crosses the terminal
 * input boundary.  Returns the new terminal's UUID.
 */
function createFirstTeammate(
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
	let newTerminalId: string;
	try {
		newTerminalId =
			createSplitForTerminal(leadPaneId, "right") ?? createCurrentTerminalSplit("right");
	} catch (error) {
		bestEffortCleanup(spawnScript.cleanup);
		observer?.onPaneCreationUncertain();
		throw error;
	}

	observer?.onPaneCreated(newTerminalId);
	try {
		startTeammateTerminal(newTerminalId, spawnScript.filePath, name);
	} catch (error) {
		bestEffortCleanup(spawnScript.cleanup);
		throw error;
	}
	return newTerminalId;
}

/**
 * Stack an additional teammate below an existing one.
 * Returns the new terminal's UUID.
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
	let newTerminalId: string;
	try {
		newTerminalId =
			createSplitForTerminal(existingPaneId, "down") ?? createCurrentTerminalSplit("down");
	} catch (error) {
		bestEffortCleanup(spawnScript.cleanup);
		observer?.onPaneCreationUncertain();
		throw error;
	}

	observer?.onPaneCreated(newTerminalId);
	try {
		startTeammateTerminal(newTerminalId, spawnScript.filePath, name);
	} catch (error) {
		bestEffortCleanup(spawnScript.cleanup);
		throw error;
	}
	return newTerminalId;
}

/**
 * Set a human-readable title on a terminal.
 *
 * The `name` property is read-only in Ghostty's AppleScript dictionary,
 * so titles go through the set_surface_title keybind action.  Best-effort.
 */
export function setPaneTitle(paneId: string, title: string): void {
	withTerminal(paneId, `perform action "set_surface_title:${appleScriptEscape(title)}" on t`);
}

/** Close a Ghostty terminal by UUID.  Best-effort: `withTerminal`
 * already swallows osascript failures, so a dead terminal is a no-op. */
export function killPane(paneId: string): void {
	if (!paneId || paneId === "none") return;
	withTerminal(paneId, "close t");
}

/** Check whether a terminal is still alive by searching for its UUID. */
export function isPaneAlive(paneId: string): boolean {
	if (!paneId || paneId === "none") return false;
	const result = withTerminal(paneId, 'return "ALIVE"');
	return result === "ALIVE";
}

/**
 * Ghostty's AppleScript dictionary exposes no terminal-content read
 * command, so pane capture (used by team_diagnose) is unsupported.
 */
export function capturePaneContent(_paneId: string, _lines: number): null {
	return null;
}

// ---------------------------------------------------------------------------
// PaneManager factory
// ---------------------------------------------------------------------------

/** PaneManager backed by Ghostty's AppleScript API. */
export const ghosttyManager: PaneManager = {
	kind: "ghostty",
	getCurrentPaneId,
	splitForFirstTeammate: createFirstTeammate,
	splitForAdditionalTeammate: createAdditionalTeammate,
	setPaneTitle,
	killPane,
	isPaneAlive,
	capturePaneContent,
};

/** Get the Ghostty pane manager.  No deferred init (unlike iterm/cmux,
 * whose factories load config from disk); kept as a getter for call-site
 * symmetry with the other backends in `pane-manager.ts`. */
export function getGhosttyManager(): PaneManager {
	return ghosttyManager;
}
