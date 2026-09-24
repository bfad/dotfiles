/**
 * WezTerm mux pane management for agent teams.
 *
 * Layout mirrors tmux/zellij:
 *   - Lead occupies the left ~1/3 of the terminal.
 *   - Teammates share the right ~2/3, stacking vertically.
 *
 * WezTerm exposes its mux through `wezterm cli`. Pane IDs are numeric strings
 * from $WEZTERM_PANE / `wezterm cli list --format json`.
 */

import { execSync } from "node:child_process";
import type { PaneLocation, PaneManager } from "./pane-manager.js";
import { shellEscape } from "./shared-utils.js";

interface WezTermPaneInfo {
  window_id: number;
  tab_id: number;
  pane_id: number;
  workspace?: string;
  tty_name?: string;
}

/** True when the process is running directly inside a WezTerm pane. */
export function isInWezTerm(): boolean {
  return !process.env.TMUX
    && !process.env.ZELLIJ
    && !process.env.SHUTTLE_SESSION_ID
    && !process.env.CMUX_WORKSPACE_ID
    && !process.env.HERDR_PANE_ID
    && !!process.env.WEZTERM_PANE;
}

/** Return the current WezTerm mux pane id. */
export function getCurrentPaneId(): string {
  const id = process.env.WEZTERM_PANE;
  if (!id) throw new Error("WEZTERM_PANE not set — are we running inside WezTerm?");
  return id;
}

/** Split the lead pane to the right and restore focus to the lead. */
export function splitForFirstTeammate(leadPaneId: string, command: string, _name?: string): string {
  const paneId = execSync(
    `wezterm cli split-pane --pane-id ${shellEscape(leadPaneId)} --right --percent 67 -- bash -lc ${shellEscape(command)}`,
    { encoding: "utf-8" },
  ).trim();
  focusPane(leadPaneId);
  return paneId;
}

/** Split an existing teammate pane downward and restore focus to the lead. */
export function splitForAdditionalTeammate(existingPaneId: string, command: string, _name?: string): string {
  const leadPaneId = getCurrentPaneId();
  const paneId = execSync(
    `wezterm cli split-pane --pane-id ${shellEscape(existingPaneId)} --bottom -- bash -lc ${shellEscape(command)}`,
    { encoding: "utf-8" },
  ).trim();
  focusPane(leadPaneId);
  return paneId;
}

/** Set a human-readable tab title for the pane's tab. */
export function setPaneTitle(paneId: string, title: string): void {
  try {
    execSync(`wezterm cli set-tab-title --pane-id ${shellEscape(paneId)} ${shellEscape(title)}`);
  } catch {
    /* best-effort only */
  }
}

/** Kill a WezTerm mux pane. */
export function killPane(paneId: string): void {
  if (!paneId || paneId === "none") return;
  try {
    execSync(`wezterm cli kill-pane --pane-id ${shellEscape(paneId)}`);
  } catch {
    /* pane may already be gone */
  }
}

/** Check whether a WezTerm pane is still alive. */
export function isPaneAlive(paneId: string): boolean {
  if (!paneId || paneId === "none") return false;
  const pane = findPane(paneId);
  if (!pane) return false;
  return hasLiveProcessOnTty(pane);
}

/** Return the WezTerm workspace/window/tab containing a live pane, or null. */
export function getPaneLocation(paneId: string): PaneLocation | null {
  if (!paneId || paneId === "none") return null;
  const pane = findPane(paneId);
  if (!pane) return null;
  return {
    sessionId: `wezterm:${pane.workspace ?? "default"}:${pane.window_id}`,
    windowId: `tab:${pane.tab_id}`,
  };
}

/** Capture the last N lines of terminal content from a pane. */
export function capturePaneContent(paneId: string, lines: number): string | null {
  if (!paneId || paneId === "none") return null;
  try {
    return execSync(
      `wezterm cli get-text --pane-id ${shellEscape(paneId)} | tail -n ${Math.max(1, Math.floor(lines))}`,
      { encoding: "utf-8", shell: "/bin/bash" },
    );
  } catch {
    return null;
  }
}

function focusPane(paneId: string): void {
  try {
    execSync(`wezterm cli activate-pane --pane-id ${shellEscape(paneId)}`);
  } catch {
    /* best-effort only */
  }
}

function findPane(paneId: string): WezTermPaneInfo | null {
  const pane = listPanes().find((candidate) => String(candidate.pane_id) === String(paneId));
  return pane ?? null;
}

function hasLiveProcessOnTty(pane: WezTermPaneInfo): boolean {
  // WezTerm can hold exited panes in the mux. When the CLI exposes the
  // pane PTY, require at least one non-zombie process on it before treating
  // the pane as alive. Older WezTerm builds omit tty_name, so keep the
  // previous list-presence behavior as a compatibility fallback.
  if (typeof pane.tty_name !== "string") return true;

  const ttyName = pane.tty_name.trim();
  if (!ttyName) return true;

  try {
    const tty = ttyName.replace(/^\/dev\//, "");
    const output = execSync(`ps -t ${shellEscape(tty)} -o stat=`, {
      encoding: "utf-8",
      stdio: ["ignore", "pipe", "ignore"],
    });
    return output.split("\n").some((line) => {
      const status = line.trim();
      return status !== "" && !status.startsWith("Z");
    });
  } catch {
    return false;
  }
}

function listPanes(): WezTermPaneInfo[] {
  try {
    const raw = execSync("wezterm cli list --format json", {
      encoding: "utf-8",
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
    if (!raw) return [];
    const parsed: unknown = JSON.parse(raw);
    if (!Array.isArray(parsed)) return [];
    return parsed.filter(isWezTermPaneInfo);
  } catch {
    return [];
  }
}

function isWezTermPaneInfo(value: unknown): value is WezTermPaneInfo {
  if (typeof value !== "object" || value === null) return false;
  const record = value as Record<string, unknown>;
  if (typeof record.window_id !== "number") return false;
  if (typeof record.tab_id !== "number") return false;
  if (typeof record.pane_id !== "number") return false;
  return true;
}

/** PaneManager backed by WezTerm mux. */
export const weztermManager: PaneManager = {
  kind: "wezterm",
  getCurrentPaneId,
  splitForFirstTeammate,
  splitForAdditionalTeammate,
  setPaneTitle,
  killPane,
  isPaneAlive,
  getPaneLocation,
  capturePaneContent,
};
