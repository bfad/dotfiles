export interface RpcRef<Rpc> {
  current: Rpc | undefined;
}

export interface ExitCleanupOptions<Rpc> {
  name: string;
  teamName: string;
  instanceId: string;
  rpcRef: RpcRef<Rpc>;
  rpcTeammates: Map<string, Rpc>;
  removeMemberRegistration(teamName: string, name: string, instanceId: string): "removed" | "skipped-mismatch" | "skipped-no-instance";
  emitTeamRemove(teamName: string, name: string): void;
  updateTeammateStatus(): void;
}

export interface SpawnTransactionOperations {
  memberName: string;
  reserve(): "reserved" | "name-held";
  kickoff(): { id: string };
  start(): void;
  finalize(): "running" | "skipped-mismatch";
  retire(): "non-consumption-proven" | "confirmed-dead" | "unconfirmed";
  cleanup(): void;
  deleteKickoff(id: string): void;
  markStopping(): void;
}

export type SpawnTransactionResult =
  | { ok: true }
  | {
      ok: false;
      stage: "reserve" | "kickoff" | "start" | "finalize";
      error: Error;
      rollback: "not-needed" | "cleaned" | "stopping";
      kickoff: "none" | "deleted" | "preserved";
    };

export function makeExitCleanup<Rpc>(options: ExitCleanupOptions<Rpc>): () => void {
  const {
    name,
    teamName,
    instanceId,
    rpcRef,
    rpcTeammates,
    removeMemberRegistration,
    emitTeamRemove,
    updateTeammateStatus,
  } = options;
  return () => {
    const capturedRpc = rpcRef.current;
    if (capturedRpc === undefined) return;

    if (rpcTeammates.get(name) === capturedRpc) rpcTeammates.delete(name);
    const removal = removeMemberRegistration(teamName, name, instanceId);
    if (removal === "removed") emitTeamRemove(teamName, name);
    updateTeammateStatus();
  };
}

function asError(error: unknown): Error {
  return error instanceof Error ? error : new Error(String(error));
}

function failed(
  stage: "reserve" | "kickoff" | "start" | "finalize",
  error: unknown,
  rollback: "not-needed" | "cleaned" | "stopping",
  kickoff: "none" | "deleted" | "preserved",
): SpawnTransactionResult {
  return { ok: false, stage, error: asError(error), rollback, kickoff };
}

function leaveStopping(operations: SpawnTransactionOperations): "stopping" {
  operations.markStopping();
  return "stopping";
}

function rollbackBeforeStart(
  operations: SpawnTransactionOperations,
  stage: "kickoff",
  error: unknown,
): SpawnTransactionResult {
  try {
    operations.cleanup();
    return failed(stage, error, "cleaned", "none");
  } catch {
    return failed(stage, error, leaveStopping(operations), "none");
  }
}

function rollbackWorker(
  operations: SpawnTransactionOperations,
  stage: "start" | "finalize",
  error: unknown,
  kickoffId: string,
): SpawnTransactionResult {
  let retirement: "non-consumption-proven" | "confirmed-dead" | "unconfirmed";
  try {
    retirement = operations.retire();
  } catch {
    return failed(stage, error, leaveStopping(operations), "preserved");
  }

  if (retirement === "unconfirmed") {
    return failed(stage, error, leaveStopping(operations), "preserved");
  }

  try {
    operations.cleanup();
  } catch {
    return failed(stage, error, leaveStopping(operations), "preserved");
  }

  if (retirement === "confirmed-dead") {
    return failed(stage, error, "cleaned", "preserved");
  }

  try {
    operations.deleteKickoff(kickoffId);
    return failed(stage, error, "cleaned", "deleted");
  } catch {
    return failed(stage, error, "cleaned", "preserved");
  }
}

export function runSpawnTransaction(operations: SpawnTransactionOperations): SpawnTransactionResult {
  try {
    if (operations.reserve() === "name-held") {
      return failed("reserve", new Error(`Teammate @${operations.memberName} already exists.`), "not-needed", "none");
    }
  } catch (error) {
    return failed("reserve", error, "not-needed", "none");
  }

  let kickoffId: string;
  try {
    kickoffId = operations.kickoff().id;
  } catch (error) {
    return rollbackBeforeStart(operations, "kickoff", error);
  }

  try {
    operations.start();
  } catch (error) {
    return rollbackWorker(operations, "start", error, kickoffId);
  }

  try {
    if (operations.finalize() !== "running") throw new Error("Spawn finalization failed due to an instance mismatch.");
  } catch (error) {
    return rollbackWorker(operations, "finalize", error, kickoffId);
  }

  return { ok: true };
}
