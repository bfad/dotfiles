/**
 * Pane manager abstraction for agent teams.
 *
 * Provides a common interface over terminal multiplexers (tmux, zellij,
 * WezTerm mux, and friends) so the rest of the extension doesn't need to know which one is in use.
 */

export interface PaneLocation {
	/** Multiplexer session id containing the pane, when available. */
	sessionId: string;

	/** Multiplexer window/tab id containing the pane, when available. */
	windowId: string;
}

export interface PaneCreationObserver {
  /** The backend created a pane with a stable id. */
  onPaneCreated(paneId: string): void;

  /** The create command failed after creation may already have occurred. */
  onPaneCreationUncertain(): void;

  /**
   * The backend created a temporary startup artifact. Report it before
   * dispatch so the spawn lifecycle can clean it on rollback, timeout,
   * shutdown, force shutdown, or full team cleanup.
   *
   * Cleanup must be idempotent and safe after prior success or artifact
   * self-removal. Aggregate cleanup retries every callback when a sibling
   * artifact fails, so a successful callback may be invoked again. The
   * callback must throw on its own failure so the lifecycle can retry it.
   */
  onStartupArtifactCreated?(cleanup: () => void): void;
}

export interface PaneManager {
	/** Which multiplexer/terminal backend is active. */
	readonly kind: "tmux" | "zellij" | "wezterm" | "shuttle" | "iterm" | "ghostty" | "cmux" | "herdr";

	/** Return the pane id of the currently focused pane. */
	getCurrentPaneId(): string;

	/**
	 * Split the lead pane to create the first teammate pane (right column).
	 * Returns the new pane id.
	 */
	splitForFirstTeammate(
		leadPaneId: string,
		command: string,
		name?: string,
		observer?: PaneCreationObserver,
	): string;

	/**
	 * Split an existing teammate pane to stack a new teammate below it.
	 * Returns the new pane id.
	 */
	splitForAdditionalTeammate(
		existingPaneId: string,
		command: string,
		name?: string,
		observer?: PaneCreationObserver,
	): string;

	/** Set a human-readable title on a pane. */
	setPaneTitle(paneId: string, title: string): void;

	/** Kill a pane by id. */
	killPane(paneId: string): void;

	/** Close a held pane only after the current Pi process has exited. */
	closePaneAfterProcessExit?(paneId: string): void;

	/** Check whether a pane is still alive. */
	isPaneAlive(paneId: string): boolean;

	/** Return the pane's session/window location, or null when unknown/dead. */
	getPaneLocation?(paneId: string): PaneLocation | null;

	/** Capture the last N lines of terminal content from a pane. */
	capturePaneContent(paneId: string, lines: number): string | null;

	/**
	 * Optionally surface team membership to the backend's agent UI (e.g. herdr's
	 * agents panel). Backends that have no such UI omit this. Best-effort — must
	 * never throw.
	 */
	setTeamMetadata?(paneId: string, meta: TeamMemberMetadata): void;
}

export interface TeamMemberMetadata {
	teamName: string;
	memberName: string;
	role: "lead" | "teammate";
	/** Short status line (e.g. task summary) shown alongside the member. */
	status?: string;
}

import { getGhosttyManager, isInGhostty } from "./ghostty.js";
import { isInITerm, getItermManager } from "./iterm.js";
import { herdrManager, isInHerdr, isHerdrBinaryAvailable } from "./herdr.js";
import { tmuxManager, isInTmux } from "./tmux.js";
import { zellijManager, isInZellij, checkZellijVersion, getZellijVersion } from "./zellij.js";
import { weztermManager, isInWezTerm } from "./wezterm.js";
import { shuttleManager, isInShuttle } from "./shuttle.js";
import { getCmuxManager, isInCmux } from "./cmux.js";

/**
 * Detect which terminal multiplexer or terminal emulator is available and
 * return the appropriate pane manager.  Returns null if none is detected.
 *
 * Detection order: herdr first (via $HERDR_PANE_ID), then tmux (via $TMUX),
 * then zellij (via $ZELLIJ), then WezTerm mux (via $WEZTERM_PANE),
 * then Shuttle (via $SHUTTLE_SESSION_ID), then cmux (via $CMUX_WORKSPACE_ID),
 * then Ghostty (via $TERM_PROGRAM=ghostty, requires Ghostty 1.3.0+ for its
 * AppleScript API), then iTerm2 (via $ITERM_SESSION_ID / $TERM_PROGRAM).
 *
 * herdr is checked before its host terminal (commonly WezTerm) because both
 * sets of env vars are present when running inside herdr, and we prefer
 * herdr's native pane management. But `isInHerdr()` excludes inner tmux/zellij
 * sessions (which inherit $HERDR_PANE_ID), so a multiplexer the user launches
 * from a herdr pane still wins.
 *
 * Throws if Zellij is detected but the version is too old (< 0.44.0).
 */
export function detectPaneManager(): PaneManager | null {
	if (isInHerdr()) {
		// Mirror the zellij binary check: teammates can have incomplete PATH
		// propagation. Without the `herdr` CLI we can't manage panes, so fall
		// back to mailbox-only (RPC) mode instead of crashing.
		return isHerdrBinaryAvailable() ? herdrManager : null;
	}
	if (isInTmux()) return tmuxManager;
	if (isInZellij()) {
		// getZellijVersion() returns null when the binary isn't on PATH
		// (common for teammates where PATH propagation is incomplete).
		// In that case return null — teammates only need the mailbox,
		// not pane management.  When the binary IS found, delegate to
		// checkZellijVersion() which throws an actionable error if the
		// version is too old.
		if (getZellijVersion() === null) {
			return null;
		}
		checkZellijVersion();
		return zellijManager;
	}
	if (isInWezTerm()) return weztermManager;
	if (isInShuttle()) return shuttleManager;
	if (isInCmux()) return getCmuxManager();
	if (isInGhostty()) return getGhosttyManager();
	if (isInITerm()) return getItermManager();
	return null;
}
