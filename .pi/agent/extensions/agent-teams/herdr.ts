/**
 * herdr pane management for agent teams.
 *
 * herdr (https://herdr.dev) is a terminal workspace manager for AI coding
 * agents. It organises terminals into workspaces → tabs → panes and exposes a
 * socket API through the `herdr` CLI. When pi runs inside herdr we prefer its
 * native pane management over the underlying terminal emulator (herdr commonly
 * runs on top of WezTerm, so both HERDR_* and WEZTERM_* env vars are present).
 *
 * Layout mirrors tmux/zellij/wezterm:
 *   - Lead occupies the left ~1/3 of the tab.
 *   - Teammates share the right ~2/3, stacking vertically.
 *
 * Implementation:
 *   - First teammate: `herdr pane split <lead> --direction right --ratio 0.33`
 *     (ratio is the fraction the original left pane keeps, so the lead keeps
 *     ~1/3 and the new right pane gets ~2/3).
 *   - Additional teammates: `herdr pane split <existing> --direction down`,
 *     stacking vertically in the right column.
 *   - `herdr pane split` doesn't accept an inline command, so we split to
 *     create a shell pane, wait for its prompt, then type the spawn command
 *     with `herdr pane run` (command text + Enter).
 *
 * Pane ids are herdr refs like "w4:p2" and are used as the "paneId" in team
 * config.
 *
 * Key CLI commands:
 *   herdr pane current [--current]
 *   herdr pane get <pane_id>
 *   herdr pane split <pane_id> --direction right|down [--ratio FLOAT] [--no-focus]
 *   herdr pane run <pane_id> <command>
 *   herdr pane read <pane_id> --source visible [--lines N]
 *   herdr pane rename <pane_id> <label>
 *   herdr pane close <pane_id>
 */

import { execSync } from "node:child_process";
import { accessSync, constants as fsConstants } from "node:fs";
import type { PaneCreationObserver, PaneLocation, PaneManager, TeamMemberMetadata } from "./pane-manager.js";
import { createSpawnScript, shellEscape } from "./shared-utils.js";

/** Reporter id used for all agent-teams metadata we push to herdr. */
const METADATA_SOURCE = "pi-agent-teams";

// ---------------------------------------------------------------------------
// Detection
// ---------------------------------------------------------------------------

/**
 * True when the process is running inside a herdr-managed pane.
 *
 * Excludes inner multiplexer/workspace sessions (tmux, zellij, Shuttle, cmux):
 * launching one from a herdr pane leaves $HERDR_PANE_ID inherited (pointing at
 * the now-outer herdr pane) while the inner tool's own env var marks the
 * session the user is actually working in. In that case detection should fall
 * through to that backend, mirroring the WezTerm guard. herdr is still
 * preferred over its host terminal (WezTerm), whose $WEZTERM_PANE we
 * intentionally don't exclude.
 */
export function isInHerdr(): boolean {
	return !process.env.TMUX
		&& !process.env.ZELLIJ
		&& !process.env.SHUTTLE_SESSION_ID
		&& !process.env.CMUX_WORKSPACE_ID
		&& !!process.env.HERDR_PANE_ID;
}

/**
 * True when the `herdr` CLI is reachable via $HERDR_BIN_PATH (herdr exports it
 * into every pane) or on PATH. Teammates can have incomplete PATH propagation;
 * when the binary isn't found we fall back to mailbox-only (RPC) mode rather
 * than crashing on every pane command.
 */
export function isHerdrBinaryAvailable(): boolean {
	if (herdrBinaryFromEnv()) return true;
	try {
		execSync("command -v herdr", { stdio: "ignore" });
		return true;
	} catch {
		return false;
	}
}

function herdrBinaryFromEnv(): string | null {
	const candidate = process.env.HERDR_BIN_PATH;
	if (!candidate) return null;
	try {
		accessSync(candidate, fsConstants.X_OK);
		return candidate;
	} catch {
		return null;
	}
}

function herdrCommand(): string {
	const fromEnv = herdrBinaryFromEnv();
	return fromEnv ? shellEscape(fromEnv) : "herdr";
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Run a herdr command and return its stdout, trimmed.
 *
 * stderr is piped rather than ignored: herdr reports usage errors (e.g. an
 * unrecognised flag) there, and discarding it left callers holding a thrown
 * error whose only text was the generic "Command failed" line. Piping captures
 * it for execErrorDetail() without letting it reach the TUI.
 */
function herdrExec(args: string): string {
	return execSync(`${herdrCommand()} ${args}`, {
		encoding: "utf-8",
		stdio: ["ignore", "pipe", "pipe"],
	}).trim();
}

/**
 * Best available description of a failed herdr command.
 *
 * Prefers the command's stderr, which is where herdr puts the actionable text
 * ("unknown option: --foo"), and falls back to the error's own message.
 */
function execErrorDetail(error: unknown): string {
	const stderr = (error as { stderr?: unknown } | null | undefined)?.stderr;
	const text = typeof stderr === "string"
		? stderr
		: Buffer.isBuffer(stderr) ? stderr.toString("utf-8") : "";
	if (text.trim()) return text.trim();
	return error instanceof Error ? error.message : String(error);
}

/**
 * Run a herdr command and parse its JSON response. Returns null on failure.
 *
 * Optional `onError` receives the actionable failure text so a caller that
 * raises can quote it. Without it the detail is lost: a stale herdr server
 * rejects every socket command with "client protocol 20 is newer than server
 * protocol 19" plus the fix, and swallowing that turned a one-line diagnosis
 * into a bare "failed to return a pane id" and a dead teammate.
 */
function herdrJson(args: string, onError?: (detail: string) => void): any | null {
	try {
		const raw = herdrExec(args);
		if (!raw) return null;
		return JSON.parse(raw);
	} catch (error) {
		onError?.(execErrorDetail(error));
		return null;
	}
}

/** Extract a pane id (e.g. "w4:p2") from a herdr pane command response. */
function extractPaneId(response: any): string | null {
	const paneId: unknown = response?.result?.pane?.pane_id;
	return typeof paneId === "string" ? paneId : null;
}

/**
 * Block until the shell in a newly-created pane is ready to accept input.
 * `herdr pane split` starts a shell asynchronously, and `herdr pane run` types
 * literal text; typing before the prompt is drawn drops the command. Poll the
 * visible screen for a prompt character, falling back to a blind wait.
 */
function waitForShellReady(paneId: string, timeoutMs: number = 5000): void {
	const start = Date.now();
	while (Date.now() - start < timeoutMs) {
		const screen = capturePaneContent(paneId, 5);
		if (screen && /[%$#>❯]\s*$/m.test(screen)) return;
		execSync("sleep 0.2");
	}
	execSync("sleep 1");
}

/**
 * Type the spawn command into a pane so it *replaces* the pane's shell.
 *
 * `herdr pane split` creates a bare interactive shell and `herdr pane run`
 * types text into it; running the command directly would leave the shell alive
 * after `pi` exits, so herdr would report the pane (and teammate) as alive
 * forever. Like cmux, we stage the command in a tempfile and `exec bash` it,
 * so the shell is replaced and the pane dies when the command exits.
 */
function sendSpawnCommand(
  paneId: string,
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
    // Two escaping layers: the inner quotes scriptPath for the *pane's* shell
    // (which parses what `pane run` types), the outer quotes the whole thing for
    // our local shell running `herdr`.
    const innerCmd = `exec bash ${shellEscape(spawnScript.filePath)}`;
    herdrExec(`pane run ${shellEscape(paneId)} ${shellEscape(innerCmd)}`);
  } catch (error) {
    try {
      spawnScript.cleanup();
    } catch {
      // Cleanup must not mask command dispatch failure.
    }
    throw error;
  }
}

/** Split off a new pane, wait for its shell, and run the spawn command in it. */
function splitAndRun(
	anchorPaneId: string,
	direction: "right" | "down",
	command: string,
	ratio?: number,
	observer?: PaneCreationObserver,
): string {
	const ratioFlag = ratio !== undefined ? ` --ratio ${ratio}` : "";
	let failureDetail = "";
	const response = herdrJson(
		`pane split ${shellEscape(anchorPaneId)} --direction ${direction}${ratioFlag} --no-focus`,
		(detail) => {
			failureDetail = detail;
		},
	);
	const newPaneId = extractPaneId(response);
	if (!newPaneId) {
		observer?.onPaneCreationUncertain();
		const suffix = failureDetail ? `: ${failureDetail}` : "";
		throw new Error(`herdr pane split (${direction}) failed to return a pane id${suffix}`);
	}
	observer?.onPaneCreated(newPaneId);

	waitForShellReady(newPaneId);
	sendSpawnCommand(newPaneId, command, observer);
	return newPaneId;
}

// ---------------------------------------------------------------------------
// PaneManager implementation
// ---------------------------------------------------------------------------

/** Return the current herdr pane id. */
export function getCurrentPaneId(): string {
	const envId = process.env.HERDR_PANE_ID;
	if (envId) return envId;
	const paneId = extractPaneId(herdrJson("pane current --current"));
	if (paneId) return paneId;
	throw new Error("HERDR_PANE_ID not set — are we running inside herdr?");
}

/**
 * Split the lead pane to the right. `--ratio` is the fraction the lead (left)
 * pane keeps, so 0.33 leaves the lead ~1/3 and the new pane ~2/3.
 * `--no-focus` keeps the user's focus on the lead.
 */
export function splitForFirstTeammate(
	leadPaneId: string,
	command: string,
	_name?: string,
	observer?: PaneCreationObserver,
): string {
	return splitAndRun(leadPaneId, "right", command, 0.33, observer);
}

/** Split an existing teammate pane downward to stack a new teammate below it. */
export function splitForAdditionalTeammate(
	existingPaneId: string,
	command: string,
	_name?: string,
	observer?: PaneCreationObserver,
): string {
	return splitAndRun(existingPaneId, "down", command, undefined, observer);
}

/** Set a human-readable label on a pane. */
export function setPaneTitle(paneId: string, title: string): void {
	try {
		herdrExec(`pane rename ${shellEscape(paneId)} ${shellEscape(title)}`);
	} catch {
		/* best-effort — pi also sets the terminal title */
	}
}

/** Close a herdr pane. */
export function killPane(paneId: string): void {
	if (!paneId || paneId === "none") return;
	try {
		herdrExec(`pane close ${shellEscape(paneId)}`);
	} catch {
		/* pane may already be gone */
	}
}

/** Check whether a herdr pane is still alive. */
export function isPaneAlive(paneId: string): boolean {
	if (!paneId || paneId === "none") return false;
	const response = herdrJson(`pane get ${shellEscape(paneId)}`);
	return extractPaneId(response) === paneId;
}

/**
 * Return the workspace/tab containing a live pane, or null.
 *
 * Returns null (rather than a synthetic placeholder) whenever workspace_id or
 * tab_id is unavailable. selectTeammateAnchor() treats a null location as
 * "can't validate — skip this anchor"; a shared placeholder would instead make
 * every pane compare equal and silently pass the cross-tab/workspace guard.
 */
export function getPaneLocation(paneId: string): PaneLocation | null {
	if (!paneId || paneId === "none") return null;
	const pane = herdrJson(`pane get ${shellEscape(paneId)}`)?.result?.pane;
	if (!pane || typeof pane.pane_id !== "string") return null;
	if (typeof pane.workspace_id !== "string" || typeof pane.tab_id !== "string") return null;
	return { sessionId: pane.workspace_id, windowId: pane.tab_id };
}

/** Resolve a pane's herdr workspace label (e.g. "ATC loop"), or null. */
function getWorkspaceLabel(paneId: string): string | null {
	const workspaceId = herdrJson(`pane get ${shellEscape(paneId)}`)?.result?.pane?.workspace_id;
	if (typeof workspaceId !== "string") return null;
	const label = herdrJson(`workspace get ${shellEscape(workspaceId)}`)?.result?.workspace?.label;
	return typeof label === "string" && label.trim() ? label : null;
}

/**
 * Label a pane in herdr's agents panel with its team membership.
 *
 * herdr auto-detects the `pi` agent per pane and, by default, shows only the
 * workspace label — so team members spawned in the lead's workspace are
 * indistinguishable. We layer `report-metadata` (keyed by our own --source, so
 * it coexists with herdr's auto-detection) to set a `display_agent` of
 * "{workspace} - {member}". When the workspace label can't be resolved we fall
 * back to the pi team name.
 *
 * `meta.status` is intentionally NOT sent. herdr exposes no per-agent status
 * field on `pane report-metadata`, and an unrecognised flag makes herdr reject
 * the entire command — which cost us the `display_agent` too, leaving every
 * teammate card blank. herdr does exit non-zero (2) and report
 * "unknown option: ..." on stderr, so this was never undetectable: the catch
 * below simply discarded the error, and herdrExec discarded the stderr that
 * explained it. Both are now reported.
 *
 * Surfacing the status needs a field herdr actually supports. `--state-label
 * STATUS=TEXT` is the candidate — the CLI accepts it and stores it as
 * `state_labels` on the pane — pending confirmation that the agents panel
 * renders it.
 */
export function setTeamMetadata(paneId: string, meta: TeamMemberMetadata): void {
	if (!paneId || paneId === "none") return;
	const prefix = getWorkspaceLabel(paneId) ?? meta.teamName;
	const member = meta.role === "lead" ? "lead" : meta.memberName;
	const displayAgent = `${prefix} - ${member}`;
	const parts = [
		`pane report-metadata ${shellEscape(paneId)}`,
		`--source ${shellEscape(METADATA_SOURCE)}`,
		`--display-agent ${shellEscape(displayAgent)}`,
	];
	try {
		herdrExec(parts.join(" "));
	} catch (error) {
		// Best-effort: panel labeling is cosmetic, so a failure must never break a
		// spawn. But it must not be silent either — report-metadata is all-or-
		// nothing, so any rejection means NO label was applied. A bare `catch {}`
		// here is what let an unsupported flag blank every teammate card unnoticed.
		console.error(
			`[agent-teams] herdr pane report-metadata failed (pane not labelled): ${execErrorDetail(error)}`,
		);
	}
}

/** Capture the last N lines of visible terminal content from a pane. */
export function capturePaneContent(paneId: string, lines: number): string | null {
	if (!paneId || paneId === "none") return null;
	try {
		const out = herdrExec(
			`pane read ${shellEscape(paneId)} --source visible --lines ${Math.max(1, Math.floor(lines))}`,
		);
		return out || null;
	} catch {
		return null;
	}
}

// ---------------------------------------------------------------------------
// Export
// ---------------------------------------------------------------------------

/** PaneManager backed by herdr. */
export const herdrManager: PaneManager = {
	kind: "herdr",
	getCurrentPaneId,
	splitForFirstTeammate,
	splitForAdditionalTeammate,
	setPaneTitle,
	killPane,
	isPaneAlive,
	getPaneLocation,
	capturePaneContent,
	setTeamMetadata,
};
