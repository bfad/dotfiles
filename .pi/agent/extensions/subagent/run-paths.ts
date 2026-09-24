import * as os from "node:os";
import * as path from "node:path";

export const RUN_STATUS_FILENAME = "status.json";
export const RUN_EVENTS_FILENAME = "events.ndjson";
export const SUBAGENT_RUNS_DIRNAME = "subagent-runs";

let testHomeDir: string | undefined;

export function __setSubagentRunStoreHomeForTests(homeDir: string | undefined): void {
  testHomeDir = homeDir;
}

function runStoreHomeDir(): string {
  return testHomeDir ?? os.homedir();
}

export function defaultRunStateRoot(): string {
  return path.join(runStoreHomeDir(), ".pi", SUBAGENT_RUNS_DIRNAME);
}

export function defaultRunStateDir(runId: string): string {
  return path.join(defaultRunStateRoot(), runId);
}

export function runStatusPath(runId: string): string {
  return path.join(defaultRunStateDir(runId), RUN_STATUS_FILENAME);
}

export function runEventsPath(runId: string): string {
  return path.join(defaultRunStateDir(runId), RUN_EVENTS_FILENAME);
}
