/**
 * cmux pane management for agent teams.
 *
 * cmux is a macOS workspace manager built on Ghostty (https://github.com/manaflow-ai/cmux).
 * It organises terminals into windows → workspaces → panes → surfaces.
 *
 * Supports two layout modes:
 *   - "split" (default): lead on the left, teammates stacked on the right.
 *   - "tab": teammates are sibling surfaces beside the lead's tab.
 *
 * Split implementation:
 *   - First teammate: `new-split right` from the lead surface, then
 *     resize the lead pane leftward to give ~⅓ of the width.
 *   - Additional teammates: `new-split down` from an existing teammate
 *     surface → stacking vertically in the right column.
 *
 * Tab implementation:
 *   - Every teammate: `new-surface` in the anchor surface's pane without
 *     changing focus.
 *
 * We track **surface** refs (e.g. "surface:14") as the "paneId" in team
 * config, since surfaces are what we send commands to and close.
 *
 * Key CLI commands:
 *   cmux new-split right|down --surface <surface>
 *   cmux new-surface --type terminal --pane <pane> --focus false
 *   cmux send --surface <surface> -- <text>
 *   cmux send-key --surface <surface> -- Return
 *   cmux read-screen --surface <surface> [--scrollback] [--lines <n>]
 *   cmux close-surface --surface <surface>
 *   cmux list-panes --workspace <workspace>
 *   cmux tree --workspace <workspace>
 *   cmux focus-pane --pane <pane>
 *   cmux resize-pane --pane <pane> -L|-R|-U|-D --amount <n>
 *   cmux rename-tab --surface <surface> -- <title>
 *   cmux surface-health --workspace <workspace>
 */

import { execSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import type { PaneCreationObserver, PaneManager } from "./pane-manager.js";
import { createSpawnScript, shellEscape } from "./shared-utils.js";

export type CmuxLayout = "split" | "tab";

const TEAMS_CONFIG_PATH = join(homedir(), ".pi", "teams", "config.json");

/** Load the cmux teammate layout from ~/.pi/teams/config.json. */
export function loadCmuxLayout(): CmuxLayout {
  try {
    const raw = JSON.parse(readFileSync(TEAMS_CONFIG_PATH, "utf-8"));
    const value = raw?.cmuxLayout?.toLowerCase();
    if (value === "tab" || value === "split") return value;
  } catch {
    /* file missing or invalid — use default */
  }
  return "split";
}

// ---------------------------------------------------------------------------
// Detection
// ---------------------------------------------------------------------------

/** True when the process is running inside a cmux-managed terminal. */
export function isInCmux(): boolean {
	return !!process.env.CMUX_WORKSPACE_ID && !!process.env.CMUX_SURFACE_ID;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Run a cmux command and return its stdout. */
function cmuxExec(cmd: string): string {
	return execSync(`cmux ${cmd}`, { encoding: "utf-8" }).trim();
}

/** Get the current workspace ref from the environment. */
function getWorkspaceId(): string {
	const id = process.env.CMUX_WORKSPACE_ID;
	if (!id) throw new Error("CMUX_WORKSPACE_ID not set — are we running inside cmux?");
	return id;
}

/**
 * Extract the surface ref from a cmux command's output.
 * cmux outputs lines like "OK surface:14 workspace:6".
 */
function extractSurfaceRef(output: string): string | null {
	const match = output.match(/surface:\d+/);
	return match ? match[0] : null;
}

/**
 * Find which pane a surface lives in by parsing the tree output.
 * Returns the pane ref (e.g. "pane:10") or null.
 */
function paneForSurface(surfaceRef: string): string | null {
	try {
		const tree = cmuxExec(`tree --workspace ${getWorkspaceId()}`);
		const lines = tree.split("\n");
		const surfacePattern = new RegExp(`\\b${surfaceRef}\\b`);
		let currentPane: string | null = null;
		for (const line of lines) {
			const paneMatch = line.match(/(pane:\d+)/);
			if (paneMatch) {
				currentPane = paneMatch[1];
			}
			if (surfacePattern.test(line)) {
				return currentPane;
			}
		}
	} catch {
		/* tree command failed */
	}
	return null;
}

/**
 * Block until the shell in a newly-created surface is ready to accept input.
 * Polls read-screen looking for a prompt character (% for zsh, $ for bash).
 * Falls back to a simple sleep if the prompt isn't found.
 */
function waitForShellReady(surfaceRef: string, timeoutMs: number = 5000): void {
	const start = Date.now();
	const pollInterval = 200; // ms
	while (Date.now() - start < timeoutMs) {
		try {
			const screen = cmuxExec(`read-screen --surface ${surfaceRef} --lines 5`);
			// Look for common shell prompt characters at the end of a line
			if (screen.match(/[%$#>]\s*$/m)) {
				return;
			}
		} catch {
			/* surface may not be ready yet */
		}
		execSync(`sleep ${pollInterval / 1000}`);
	}
	// Last resort: blind wait
	execSync("sleep 1");
}

/**
 * Spawn a command in a cmux surface without echoing it to the terminal.
 *
 * cmux's `new-split` doesn't accept a command (unlike tmux's
 * `split-window <cmd>`), so we have to "type" the command into the new
 * surface via `cmux send`. Our spawn command can be long, so naively sending
 * it produces a wall of text in the pane, and that wall is a problem: keystrokes
 * from another workspace can interleave with the synthetic typing and corrupt
 * the spawn unrecoverably.
 *
 * To avoid both, we stage the command in a tempfile and only type a
 * short `exec bash <path>` line into the surface.
 */
function sendSpawnCommand(
  surfaceRef: string,
  command: string,
  observer?: PaneCreationObserver,
): void {
  const spawnScript = createSpawnScript(command);
  try {
    observer?.onStartupArtifactCreated?.(spawnScript.cleanup);
  } catch (error) {
    try {
      spawnScript.cleanup();
    } catch {
      // Cleanup must not mask lifecycle registration failure.
    }
    throw error;
  }

  try {
    // Two layers of shellEscape are required: the inner one quotes
    // scriptPath for the *pane's* shell (which parses what `cmux send`
    // types into it), the outer one quotes the whole command for our
    // local shell. Don't collapse them — `os.tmpdir()` is env-derived
    // (TMPDIR/TMP/TEMP) and may contain whitespace or shell metacharacters.
    const innerCmd = `exec bash ${shellEscape(spawnScript.filePath)}`;
    cmuxExec(`send --surface ${surfaceRef} -- ${shellEscape(innerCmd)}`);
    cmuxExec(`send-key --surface ${surfaceRef} -- Return`);
  } catch (error) {
    try {
      spawnScript.cleanup();
    } catch {
      // Cleanup must not mask command dispatch failure.
    }
    throw error;
  }
}

interface FocusSnapshot {
	workspaceRef: string;
	paneRef: string;
}

/**
 * Snapshot what the user is currently looking at, so it can be restored
 * after a `new-split` (which always yanks focus to the new pane).
 *
 * Reads the `focused` field from `cmux identify`, not `caller` — they
 * differ when the user has navigated to another workspace while pi is
 * running in the background.
 */
function snapshotFocus(): FocusSnapshot | null {
	try {
		const result = cmuxExec("identify");
		const parsed = JSON.parse(result);
		const workspaceRef: unknown = parsed?.focused?.workspace_ref;
		const paneRef: unknown = parsed?.focused?.pane_ref;
		if (typeof workspaceRef === "string" && typeof paneRef === "string") {
			return { workspaceRef, paneRef };
		}
	} catch {
		/* fall through */
	}
	return null;
}

/** Best-effort restore of a focus snapshot. No-op if the snapshot is null. */
function restoreFocus(snapshot: FocusSnapshot | null): void {
	if (!snapshot) return;
	try {
		cmuxExec(`focus-pane --pane ${snapshot.paneRef} --workspace ${snapshot.workspaceRef}`);
	} catch {
		/* best-effort */
	}
}

/**
 * Get all teammate surface refs (excluding the lead) by parsing the tree.
 */
function getTeammateSurfaces(leadSurfaceRef: string): string[] {
	try {
		const tree = cmuxExec(`tree --workspace ${getWorkspaceId()}`);
		const surfaces: string[] = [];
		const surfacePattern = /(surface:\d+)/g;
		let match;
		while ((match = surfacePattern.exec(tree)) !== null) {
			if (match[1] !== leadSurfaceRef) {
				surfaces.push(match[1]);
			}
		}
		return surfaces;
	} catch {
		return [];
	}
}

// ---------------------------------------------------------------------------
// PaneManager implementation
// ---------------------------------------------------------------------------

/**
 * Return the surface ref of the current terminal.
 * cmux sets $CMUX_SURFACE_ID automatically (a UUID), but we need the
 * short ref form (surface:N) for CLI commands.
 */
export function getCurrentPaneId(): string {
	// Try the identify command which gives us the short ref
	try {
		const result = cmuxExec("identify");
		const parsed = JSON.parse(result);
		const callerRef = parsed?.caller?.surface_ref;
		if (callerRef) return callerRef;
	} catch {
		/* fall through */
	}
	// Fallback: use the UUID from env and try to find the ref via tree
	const uuid = process.env.CMUX_SURFACE_ID;
	if (!uuid) throw new Error("CMUX_SURFACE_ID not set — are we running inside cmux?");
	// The tree output uses refs not UUIDs, but surface-health can help
	try {
		const health = cmuxExec(`surface-health --workspace ${getWorkspaceId()}`);
		// Parse lines like "surface:13  type=terminal in_window=true"
		const lines = health.split("\n");
		if (lines.length === 1) {
			const ref = lines[0].match(/(surface:\d+)/);
			if (ref) return ref[1];
		}
	} catch {
		/* fall through */
	}
	throw new Error("Cannot determine current cmux surface ref");
}

/** Create a teammate as a sibling tab in the pane containing the anchor surface. */
function createTabbedTeammate(
  anchorSurfaceRef: string,
  command: string,
  observer?: PaneCreationObserver,
): string {
  const paneRef = paneForSurface(anchorSurfaceRef);
  if (!paneRef) {
    throw new Error(`Cannot find cmux pane containing ${anchorSurfaceRef}`);
  }

  let output: string;
  try {
    output = cmuxExec(
      `new-surface --type terminal --pane ${paneRef} --workspace ${getWorkspaceId()} --focus false`,
    );
  } catch (error) {
    observer?.onPaneCreationUncertain();
    throw error;
  }

  const newSurface = extractSurfaceRef(output);
  if (!newSurface || newSurface === anchorSurfaceRef) {
    observer?.onPaneCreationUncertain();
    throw new Error(`Failed to create tab in ${paneRef}: ${output}`);
  }
  observer?.onPaneCreated(newSurface);

  waitForShellReady(newSurface);
  sendSpawnCommand(newSurface, command, observer);

  return newSurface;
}

/**
 * Split the lead surface to the right. The new (right) surface runs `command`.
 * Returns the new surface's ref (used as paneId in team config).
 */
export function splitForFirstTeammate(
	leadSurfaceRef: string,
	command: string,
	_name?: string,
	observer?: PaneCreationObserver,
): string {
	// Capture focus before the split — `new-split` will yank it.
	const preFocus = snapshotFocus();

	// Split right from the lead surface
	let output: string;
	try {
		output = cmuxExec(`new-split right --surface ${leadSurfaceRef}`);
	} catch (error) {
		observer?.onPaneCreationUncertain();
		throw error;
	}
	const newSurface = extractSurfaceRef(output);
	if (!newSurface || newSurface === leadSurfaceRef) {
		observer?.onPaneCreationUncertain();
		throw new Error(`Failed to create right split: ${output}`);
	}
	observer?.onPaneCreated(newSurface);

	// Restore focus right away. `new-split` is the only command in this
	// function that changes workspace focus; everything below targets a
	// specific surface or pane by ref. Don't move this further down —
	// `waitForShellReady` polls for up to 5s and the user's view would be
	// stuck on the wrong workspace that whole time.
	restoreFocus(preFocus);

	// Resize so the lead gets ~⅓ and teammates get ~⅔.
	// The new pane (right side) was created at 50/50. We push its LEFT
	// border leftward to grow it (stealing width from the lead).
	// We can't use -R here: the new pane is at the workspace's right edge
	// with no adjacent border on its right, so cmux errors with
	// "Pane has no adjacent border in direction right".
	const newPane = paneForSurface(newSurface);
	if (newPane) {
		try {
			cmuxExec(`resize-pane --pane ${newPane} --workspace ${getWorkspaceId()} -L --amount 20`);
		} catch {
			/* best-effort — layout still works at default 50/50 */
		}
	}

	// Wait for the shell in the new surface to be ready.
	waitForShellReady(newSurface);

	sendSpawnCommand(newSurface, command, observer);

	return newSurface;
}

/**
 * Split an existing teammate's surface downward to stack a new teammate below.
 * Returns the new surface's ref.
 */
export function splitForAdditionalTeammate(
	existingTeammateSurfaceRef: string,
	command: string,
	_name?: string,
	observer?: PaneCreationObserver,
): string {
	const preFocus = snapshotFocus();

	// Split down from the existing teammate's surface
	let output: string;
	try {
		output = cmuxExec(`new-split down --surface ${existingTeammateSurfaceRef}`);
	} catch (error) {
		observer?.onPaneCreationUncertain();
		throw error;
	}
	const newSurface = extractSurfaceRef(output);
	if (!newSurface || newSurface === existingTeammateSurfaceRef) {
		observer?.onPaneCreationUncertain();
		throw new Error(`Failed to create down split: ${output}`);
	}
	observer?.onPaneCreated(newSurface);

	restoreFocus(preFocus);

	// Wait for the shell to be ready.
	waitForShellReady(newSurface);

	sendSpawnCommand(newSurface, command, observer);

	return newSurface;
}

/**
 * Set a title on a surface's tab.
 */
export function setPaneTitle(surfaceRef: string, title: string): void {
	try {
		cmuxExec(`rename-tab --surface ${surfaceRef} -- ${shellEscape(title)}`);
	} catch {
		/* best-effort — pi sets the terminal title which cmux picks up */
	}
}

/** Close a cmux surface. */
export function killPane(surfaceRef: string): void {
	if (!surfaceRef || surfaceRef === "none") return;
	try {
		cmuxExec(`close-surface --surface ${surfaceRef}`);
	} catch {
		/* surface may already be gone */
	}
}

/**
 * Check whether a surface is still alive.
 * Uses surface-health to check if the surface exists and is a live terminal.
 */
export function isPaneAlive(surfaceRef: string): boolean {
	if (!surfaceRef || surfaceRef === "none") return false;
	try {
		const health = cmuxExec(`surface-health --workspace ${getWorkspaceId()}`);
		// surface-health lists all surfaces: "surface:13  type=terminal in_window=true"
		// Use regex with word boundary to avoid "surface:1" matching "surface:10"
		return new RegExp(`\\b${surfaceRef}\\b`).test(health);
	} catch {
		return false;
	}
}

/**
 * Capture the last N lines of terminal content from a surface.
 */
export function capturePaneContent(surfaceRef: string, lines: number): string | null {
	if (!surfaceRef || surfaceRef === "none") return null;
	try {
		return cmuxExec(`read-screen --surface ${surfaceRef} --lines ${lines}`);
	} catch {
		return null;
	}
}

// ---------------------------------------------------------------------------
// Export
// ---------------------------------------------------------------------------

/** Create a cmux PaneManager with the requested teammate layout. */
export function createCmuxManager(layout: CmuxLayout = "split"): PaneManager {
  return {
    kind: "cmux",
    getCurrentPaneId,
    splitForFirstTeammate: layout === "tab"
      ? (leadPaneId, command, _name, observer) => createTabbedTeammate(leadPaneId, command, observer)
      : splitForFirstTeammate,
    splitForAdditionalTeammate: layout === "tab"
      ? (existingPaneId, command, _name, observer) => createTabbedTeammate(existingPaneId, command, observer)
      : splitForAdditionalTeammate,
    setPaneTitle,
    killPane,
    isPaneAlive,
    capturePaneContent,
  };
}

/** Backward-compatible cmux manager using the default split layout. */
export const cmuxManager = createCmuxManager();

let configuredCmuxManager: PaneManager | null = null;

/** Get the cmux pane manager, loading its layout once for this Pi process. */
export function getCmuxManager(): PaneManager {
  if (!configuredCmuxManager) {
    configuredCmuxManager = createCmuxManager(loadCmuxLayout());
  }
  return configuredCmuxManager;
}
