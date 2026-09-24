import { randomBytes } from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

/**
 * Shared utilities for agent-teams pane manager backends.
 */

const SPAWN_SCRIPT_CLEANUP_DELAY_MS = 60_000;

export interface SpawnScriptArtifact {
  filePath: string;
  /** Idempotent and intentionally throwing so lifecycle cleanup can retry. */
  cleanup(): void;
}

function bestEffortCleanup(cleanup: () => void): void {
  try {
    cleanup();
  } catch {
    // Cleanup must not mask artifact creation or timer failures.
  }
}

/**
 * Stage a teammate command in an owner-only, self-removing script.
 *
 * The random path is created with O_EXCL and tightened to mode 0700. The
 * fallback timer covers a pane that never consumes the script, while the
 * throwing, idempotent cleanup callback supports lifecycle retries.
 */
export function createSpawnScript(command: string): SpawnScriptArtifact {
  const filePath = path.join(
    os.tmpdir(),
    `pi-team-spawn-${randomBytes(16).toString("hex")}.sh`,
  );
  const script = [
    "#!/bin/bash",
    `rm -f -- ${shellEscape(filePath)}`,
    command,
  ].join("\n") + "\n";
  let created = false;

  try {
    fs.writeFileSync(filePath, script, {
      encoding: "utf-8",
      flag: "wx",
      mode: 0o700,
    });
    created = true;
    fs.chmodSync(filePath, 0o700);
  } catch (error) {
    const isCollision = error instanceof Error
      && "code" in error
      && error.code === "EEXIST";
    if (created || !isCollision) {
      bestEffortCleanup(() => fs.rmSync(filePath, { force: true }));
    }
    throw error;
  }

  let cleanupTimer: NodeJS.Timeout | undefined;
  const cleanup = (): void => {
    fs.rmSync(filePath, { force: true });
    if (cleanupTimer) clearTimeout(cleanupTimer);
    cleanupTimer = undefined;
  };
  cleanupTimer = setTimeout(
    () => bestEffortCleanup(cleanup),
    SPAWN_SCRIPT_CLEANUP_DELAY_MS,
  );
  cleanupTimer.unref();

  return { filePath, cleanup };
}

/** Shell-escape a string for safe embedding in shell commands. */
export function shellEscape(s: string): string {
	return `'${s.replace(/'/g, "'\\''")}'`;
}

/**
 * Check whether a team member is alive, regardless of transport.
 *
 * The vscode PID-check logic is transport-agnostic; only the final
 * `isPaneAlive` call is backend-specific, so callers pass it in.
 */
export function isTransportAlive(
	member: { paneId: string; transport?: string; pid?: number },
	isPaneAlive: (id: string) => boolean,
): boolean {
	if (member.transport === "vscode") {
		if (member.pid) {
			try {
				process.kill(member.pid, 0);
				return true;
			} catch {
				return false;
			}
		}
		return false;
	}
	return isPaneAlive(member.paneId);
}
