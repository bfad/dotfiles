import { randomBytes } from "node:crypto";
import { chmodSync, rmSync, writeFileSync } from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { shellEscape } from "./shared-utils.js";

export interface PaneLaunchHandoffDeps {
  randomBytes(size: number): { toString(encoding: "hex"): string };
  writeFile(
    filePath: string,
    content: string,
    options: { encoding: "utf-8"; flag: "wx"; mode: number },
  ): void;
  chmod(filePath: string, mode: number): void;
  remove(filePath: string, options: { force: true }): void;
  onError?: (context: string, error: unknown) => void;
}

const defaultHandoffDeps: PaneLaunchHandoffDeps = {
  randomBytes,
  writeFile: writeFileSync,
  chmod: chmodSync,
  remove: rmSync,
};

export interface PaneLaunchHandoff {
  filePath: string;
  command: string;
  cleanup(): void;
}

function defaultReportError(context: string, error: unknown): void {
  console.error(`[agent-teams] ${context}`, error);
}

/**
 * Stage the full child launch command in a script and return a short pane-safe
 * command. Some pane backends pass the launch string through terminal APIs with
 * lower limits than ARG_MAX; keep the transported command bounded while leaving
 * PATH and other child environment assignments in the script.
 */
export function createPaneLaunchHandoff(
  command: string,
  tempRoot: string = os.tmpdir(),
  deps: PaneLaunchHandoffDeps = defaultHandoffDeps,
): PaneLaunchHandoff {
  const filePath = path.join(
    tempRoot,
    `pi-agent-teams-launch-${deps.randomBytes(16).toString("hex")}.sh`,
  );
  const script = [
    "#!/usr/bin/env bash",
    `rm -f -- ${shellEscape(filePath)}`,
    command,
  ].join("\n") + "\n";

  try {
    deps.writeFile(filePath, script, { encoding: "utf-8", flag: "wx", mode: 0o700 });
    deps.chmod(filePath, 0o700);
  } catch (error) {
    try {
      deps.remove(filePath, { force: true });
    } catch (cleanupError) {
      try {
        (deps.onError ?? defaultReportError)(
          `could not remove incomplete pane launch handoff ${filePath}`,
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
    command: `exec bash ${shellEscape(filePath)}`,
    cleanup: () => deps.remove(filePath, { force: true }),
  };
}
