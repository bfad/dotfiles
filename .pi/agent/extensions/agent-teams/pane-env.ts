import {
  chmodSync,
  lstatSync,
  readdirSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { randomUUID } from "node:crypto";
import * as os from "node:os";
import * as path from "node:path";
import { shellEscape } from "./shared-utils.js";

export const PANE_ENV_ALLOWLIST = ["TOOL_GATEWAY_TOKEN", "TOOL_GATEWAY_MCP_URL"] as const;
const PANE_ENV_FILE_PATTERN = /^pi-agent-teams-env-([1-9]\d*)-([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.sh$/;
const STARTUP_SCRIPT_FILE_PATTERN = /^(?:pi-team-spawn|pi-agent-teams-launch)-[0-9a-f]{32}\.sh$/;

export interface PaneEnvHandoffDeps {
  randomUUID(): string;
  pid(): number;
  writeFile(
    filePath: string,
    content: string,
    options: { encoding: "utf-8"; flag: "wx"; mode: number },
  ): void;
  chmod(filePath: string, mode: number): void;
  remove(filePath: string, options: { force: true }): void;
  onError?: (context: string, error: unknown) => void;
}

const defaultHandoffDeps: PaneEnvHandoffDeps = {
  randomUUID,
  pid: () => process.pid,
  writeFile: writeFileSync,
  chmod: chmodSync,
  remove: rmSync,
};

export interface PaneEnvHandoff {
  filePath: string;
  loadCommand: string;
  exportCommand: string;
  cleanup(): void;
}

export interface PaneEnvHandoffRegistry {
  register(instanceId: string, cleanup: () => void): void;
  cleanup(instanceId: string): void;
  cleanupAll(): void;
  cleanupAllAndWait(options?: PaneEnvHandoffCleanupOptions): Promise<void>;
}

export interface PaneEnvHandoffCleanupOptions {
  attempts?: number;
  delayMs?: number;
}

export interface PaneEnvHandoffRegistryOptions {
  staleAfterMs?: number;
  tempRoot?: string;
  now?: () => number;
  getUid?: () => number | undefined;
  isProcessAlive?: (pid: number) => boolean;
  onError?: (context: string, error: unknown) => void;
}

function serializePaneEnvironment(environment: NodeJS.ProcessEnv): string {
  const lines: string[] = [];
  for (const key of PANE_ENV_ALLOWLIST) {
    lines.push(`unset ${key}`);
    const value = environment[key];
    if (value !== undefined) lines.push(`${key}=${shellEscape(value)}`);
  }
  return `${lines.join("\n")}\n`;
}

function buildLoadCommand(filePath: string): string {
  const escapedPath = shellEscape(filePath);
  return [
    "{ set +x",
    "set +v",
    "set +a",
    `. ${escapedPath}`,
    "pane_env_load_status=$?",
    `command rm -f -- ${escapedPath}`,
    "pane_env_remove_status=$?",
    '[ "$pane_env_load_status" -eq 0 ] || exit "$pane_env_load_status"',
    '[ "$pane_env_remove_status" -eq 0 ] || exit "$pane_env_remove_status"',
    "}",
  ].join("; ");
}

function defaultIsProcessAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return !(error instanceof Error && "code" in error && error.code === "ESRCH");
  }
}

function defaultReportError(context: string, error: unknown): void {
  console.error(`[agent-teams] ${context}`, error);
}

/**
 * Remove old pane startup artifacts. Environment handoffs are eligible only
 * after their lead process dies; startup scripts are eligible by age alone.
 * Both require exact names and owner-controlled regular-file metadata.
 */
export function scavengeStalePaneEnvHandoffs(options: PaneEnvHandoffRegistryOptions = {}): void {
  const tempRoot = options.tempRoot ?? os.tmpdir();
  const staleAfterMs = options.staleAfterMs ?? 60_000;
  const now = options.now ?? Date.now;
  const getUid = options.getUid ?? (() => process.getuid?.());
  const isProcessAlive = options.isProcessAlive ?? defaultIsProcessAlive;
  const onError = options.onError ?? defaultReportError;
  const uid = getUid();

  // Without an ownership primitive, preserve files rather than weakening the
  // cleanup boundary. Agent Teams' pane backends are supported on Unix hosts.
  if (uid === undefined) return;

  let names: string[];
  try {
    names = readdirSync(tempRoot);
  } catch (error) {
    onError(`could not scan pane environment handoffs in ${tempRoot}`, error);
    return;
  }

  for (const name of names) {
    const handoffMatch = PANE_ENV_FILE_PATTERN.exec(name);
    const isStartupScript = STARTUP_SCRIPT_FILE_PATTERN.test(name);
    if (!handoffMatch && !isStartupScript) continue;
    const filePath = path.join(tempRoot, name);
    try {
      const stat = lstatSync(filePath);
      if (!stat.isFile() || stat.isSymbolicLink()) continue;
      const expectedMode = isStartupScript ? 0o700 : 0o600;
      if (stat.uid !== uid || stat.nlink !== 1 || (stat.mode & 0o777) !== expectedMode) continue;
      if (now() - stat.mtimeMs < staleAfterMs) continue;
      if (handoffMatch && isProcessAlive(Number(handoffMatch[1]))) continue;
      rmSync(filePath, { force: true });
    } catch (error) {
      if (error instanceof Error && "code" in error && error.code === "ENOENT") continue;
      onError(`could not scavenge pane environment handoff ${filePath}`, error);
    }
  }
}

/**
 * Snapshot the pane-safe environment allowlist into a unique, owner-readable
 * file. The returned shell fragment sources and unlinks it before Pi starts;
 * no environment values are interpolated into the pane command itself.
 */
export function createPaneEnvHandoff(
  environment: NodeJS.ProcessEnv = process.env,
  tempRoot: string = os.tmpdir(),
  deps: PaneEnvHandoffDeps = defaultHandoffDeps,
): PaneEnvHandoff {
  const filePath = path.join(
    tempRoot,
    `pi-agent-teams-env-${deps.pid()}-${deps.randomUUID()}.sh`,
  );
  try {
    deps.writeFile(filePath, serializePaneEnvironment(environment), {
      encoding: "utf-8",
      flag: "wx",
      mode: 0o600,
    });
    deps.chmod(filePath, 0o600);
  } catch (error) {
    try {
      deps.remove(filePath, { force: true });
    } catch (cleanupError) {
      try {
        (deps.onError ?? defaultReportError)(
          `could not remove incomplete pane environment handoff ${filePath}`,
          cleanupError,
        );
      } catch {
        // Reporting must never mask the original creation failure.
      }
    }
    throw error;
  }

  return {
    filePath,
    loadCommand: buildLoadCommand(filePath),
    exportCommand: `export ${PANE_ENV_ALLOWLIST.join(" ")}`,
    cleanup: () => deps.remove(filePath, { force: true }),
  };
}

/**
 * Keep a lead-local cleanup handle until the child consumes the handoff. The
 * timeout covers a backend that reports a pane but never starts its shell.
 * Failed cleanup remains registered and is retried without interrupting other
 * pane or team teardown.
 */
export function createPaneEnvHandoffRegistry(
  staleAfterMsOrOptions: number | PaneEnvHandoffRegistryOptions = 60_000,
): PaneEnvHandoffRegistry {
  const options = typeof staleAfterMsOrOptions === "number"
    ? { staleAfterMs: staleAfterMsOrOptions }
    : staleAfterMsOrOptions;
  const staleAfterMs = options.staleAfterMs ?? 60_000;
  const onError = options.onError ?? defaultReportError;
  type PendingEntry = { cleanup: () => void; timer?: NodeJS.Timeout; lastError?: unknown };
  const pending = new Map<string, Set<PendingEntry>>();

  scavengeStalePaneEnvHandoffs({ ...options, staleAfterMs, onError });

  const removeEntry = (instanceId: string, entry: PendingEntry): void => {
    const entries = pending.get(instanceId);
    entries?.delete(entry);
    if (entries?.size === 0) pending.delete(instanceId);
  };

  const report = (context: string, error: unknown): void => {
    try {
      onError(context, error);
    } catch {
      // Reporting must not turn best-effort credential cleanup into a crash.
    }
  };

  const disarm = (entry: PendingEntry): void => {
    if (entry.timer) clearTimeout(entry.timer);
    entry.timer = undefined;
  };

  const arm = (instanceId: string, entry: PendingEntry): void => {
    const timer = setTimeout(() => {
      entry.timer = undefined;
      cleanupEntry(instanceId, entry);
    }, staleAfterMs);
    timer.unref();
    entry.timer = timer;
  };

  const cleanupEntry = (
    instanceId: string,
    entry: PendingEntry,
    scheduleRetry = true,
  ): void => {
    try {
      entry.cleanup();
    } catch (error) {
      entry.lastError = error;
      if (scheduleRetry) {
        report(
          `could not clean pane environment handoff for ${instanceId}; retry scheduled`,
          error,
        );
        if (!entry.timer) arm(instanceId, entry);
      }
      return;
    }
    disarm(entry);
    removeEntry(instanceId, entry);
  };

  const cleanup = (instanceId: string): void => {
    const entries = pending.get(instanceId);
    if (!entries) return;
    for (const entry of [...entries]) cleanupEntry(instanceId, entry);
  };

  const cleanupAll = (): void => {
    for (const [instanceId, entries] of [...pending]) {
      for (const entry of [...entries]) cleanupEntry(instanceId, entry);
    }
  };

  return {
    register(instanceId, cleanupHandoff) {
      // Dispose any prior successful registration. A failed prior cleanup is
      // retained alongside the replacement so neither handle is lost.
      cleanup(instanceId);
      const entry: PendingEntry = { cleanup: cleanupHandoff };
      const entries = pending.get(instanceId) ?? new Set<PendingEntry>();
      entries.add(entry);
      pending.set(instanceId, entries);
      arm(instanceId, entry);
    },
    cleanup,
    cleanupAll,
    async cleanupAllAndWait({ attempts = 3, delayMs = 25 } = {}) {
      for (let attempt = 1; attempt <= attempts; attempt += 1) {
        for (const [instanceId, entries] of [...pending]) {
          for (const entry of [...entries]) {
            disarm(entry);
            cleanupEntry(instanceId, entry, false);
          }
        }
        if (pending.size === 0) return;
        if (attempt < attempts) {
          await new Promise<void>((resolve) => setTimeout(resolve, delayMs));
        }
      }

      for (const [instanceId, entries] of pending) {
        for (const entry of entries) {
          report(
            `final shutdown cleanup failed for pane environment handoff ${instanceId} after ${attempts} attempts; retry scheduled`,
            entry.lastError,
          );
          disarm(entry);
          arm(instanceId, entry);
        }
      }
    },
  };
}
