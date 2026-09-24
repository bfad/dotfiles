import { randomUUID } from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

export const PRIVATE_SESSION_DIR_NAME = "team-sessions";
export const PRIVATE_SESSION_RETENTION_MS = 7 * 24 * 60 * 60 * 1000;
export const PRIVATE_SESSION_LEASE_FILE_NAME = ".agent-teams-lease.json";
export const PI_TEAM_INSTANCE_ID_ENV = "PI_TEAM_INSTANCE_ID";
export const PI_TEAM_SESSION_DIR_ENV = "PI_TEAM_SESSION_DIR";

const INSTANCE_ID_PATTERN = /^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$/;

export function isValidInstanceId(instanceId: string): boolean {
  return INSTANCE_ID_PATTERN.test(instanceId);
}

export function resolveAgentDir(agentDir?: string): string {
  return path.resolve(agentDir ?? path.join(os.homedir(), ".pi", "agent"));
}

export function defaultAgentDir(): string {
  return resolveAgentDir(process.env.PI_CODING_AGENT_DIR);
}

export function privateSessionRoot(agentDir: string = defaultAgentDir()): string {
  return path.join(path.resolve(agentDir), PRIVATE_SESSION_DIR_NAME);
}

export function privateSessionDir(instanceId: string, agentDir: string = defaultAgentDir()): string {
  if (!isValidInstanceId(instanceId)) {
    throw new Error(`Invalid Agent Teams instance ID: ${JSON.stringify(instanceId)}`);
  }
  return path.join(privateSessionRoot(agentDir), instanceId);
}

export interface PrivateSessionLease {
  filePath: string;
  instanceId: string;
  pid: number;
}

interface LeaseData {
  instanceId: string;
  pid: number;
}

function parseLeaseData(value: string): LeaseData | null {
  try {
    const parsed = JSON.parse(value) as Partial<LeaseData>;
    if (typeof parsed.pid !== "number" || !Number.isSafeInteger(parsed.pid) || parsed.pid <= 0) return null;
    if (typeof parsed.instanceId !== "string" || !isValidInstanceId(parsed.instanceId)) return null;
    return { instanceId: parsed.instanceId, pid: parsed.pid };
  } catch {
    return null;
  }
}

export function privateSessionLeasePath(sessionDir: string): string {
  return path.join(path.resolve(sessionDir), PRIVATE_SESSION_LEASE_FILE_NAME);
}

export function acquirePrivateSessionLease(
  sessionDir: string,
  instanceId: string,
  pid: number = process.pid,
): PrivateSessionLease {
  const expectedDir = privateSessionDir(instanceId);
  const resolvedDir = path.resolve(sessionDir);
  if (resolvedDir !== expectedDir) {
    throw new Error(`Agent Teams private session directory does not match instance ${JSON.stringify(instanceId)}`);
  }
  if (!Number.isSafeInteger(pid) || pid <= 0) {
    throw new Error(`Invalid Agent Teams lease PID: ${JSON.stringify(pid)}`);
  }

  fs.mkdirSync(resolvedDir, { recursive: true });
  const filePath = privateSessionLeasePath(resolvedDir);
  const tempPath = path.join(resolvedDir, `.${PRIVATE_SESSION_LEASE_FILE_NAME}.${pid}.${randomUUID()}.tmp`);
  try {
    fs.writeFileSync(tempPath, `${JSON.stringify({ pid, instanceId })}\n`, {
      encoding: "utf-8",
      flag: "wx",
      mode: 0o600,
    });
    fs.renameSync(tempPath, filePath);
  } finally {
    fs.rmSync(tempPath, { force: true });
  }
  return { filePath, instanceId, pid };
}

export function releasePrivateSessionLease(lease: PrivateSessionLease): void {
  try {
    const stat = fs.lstatSync(lease.filePath);
    if (!stat.isFile() || stat.isSymbolicLink()) return;
    const current = parseLeaseData(fs.readFileSync(lease.filePath, "utf-8"));
    if (!current || current.pid !== lease.pid || current.instanceId !== lease.instanceId) return;
    fs.rmSync(lease.filePath, { force: true });
  } catch {
    // Teammate shutdown must continue if the lease was already removed or is unreadable.
  }
}

function defaultIsProcessAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return (error as NodeJS.ErrnoException).code === "EPERM";
  }
}

function liveLeaseProtects(dir: string, instanceId: string, isProcessAlive: (pid: number) => boolean): boolean {
  const leasePath = privateSessionLeasePath(dir);
  try {
    const stat = fs.lstatSync(leasePath);
    if (!stat.isFile() || stat.isSymbolicLink()) return false;
    const lease = parseLeaseData(fs.readFileSync(leasePath, "utf-8"));
    return lease?.instanceId === instanceId && isProcessAlive(lease.pid);
  } catch {
    return false;
  }
}

function directoryIsSafeAndStale(dir: string, cutoffMs: number): boolean {
  const pending = [dir];
  while (pending.length > 0) {
    const current = pending.pop()!;
    const entries = fs.readdirSync(current, { withFileTypes: true });
    for (const entry of entries) {
      const entryPath = path.join(current, entry.name);
      const stat = fs.lstatSync(entryPath);
      if (stat.isSymbolicLink()) return false;
      if (stat.isDirectory()) {
        if (stat.mtimeMs >= cutoffMs) return false;
        pending.push(entryPath);
      } else if (current === dir && entry.name === PRIVATE_SESSION_LEASE_FILE_NAME) {
        // A dead or invalid lease is not session activity and cannot extend retention.
        continue;
      } else if (stat.mtimeMs >= cutoffMs) {
        return false;
      }
    }
  }
  return true;
}

export interface PrunePrivateSessionOptions {
  agentDir?: string;
  nowMs?: number;
  retentionMs?: number;
  isProcessAlive?: (pid: number) => boolean;
}

export function prunePrivateSessionDirs(options: PrunePrivateSessionOptions = {}): string[] {
  const root = privateSessionRoot(options.agentDir);
  const nowMs = options.nowMs ?? Date.now();
  const retentionMs = options.retentionMs ?? PRIVATE_SESSION_RETENTION_MS;
  const cutoffMs = nowMs - retentionMs;
  const isProcessAlive = options.isProcessAlive ?? defaultIsProcessAlive;
  let rootStat: fs.Stats;
  let entries: string[];
  try {
    rootStat = fs.lstatSync(root);
    if (rootStat.isSymbolicLink()) return [];
    if (!rootStat.isDirectory()) return [];
    entries = fs.readdirSync(root);
  } catch {
    return [];
  }

  const pruned: string[] = [];
  for (const entry of entries) {
    if (!isValidInstanceId(entry)) continue;
    const candidate = path.join(root, entry);
    try {
      const stat = fs.lstatSync(candidate);
      if (stat.isSymbolicLink()) continue;
      if (!stat.isDirectory()) continue;
      if (stat.mtimeMs >= cutoffMs) continue;
      if (liveLeaseProtects(candidate, entry, isProcessAlive)) continue;
      if (!directoryIsSafeAndStale(candidate, cutoffMs)) continue;
      fs.rmSync(candidate, { recursive: true, force: true });
      pruned.push(candidate);
    } catch {
      // Pruning is best-effort. One unreadable or concurrently changed directory
      // must not prevent other private session directories from being checked.
    }
  }
  return pruned;
}

export function schedulePrivateSessionPrune(
  prune: () => unknown = () => prunePrivateSessionDirs(),
): void {
  const timer = setTimeout(() => {
    try {
      prune();
    } catch {
      // Session startup must never fail because stale-session pruning failed.
    }
  }, 0);
  timer.unref();
}
