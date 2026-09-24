import * as fs from "node:fs";
import * as path from "node:path";
import {
  PRIVATE_SESSION_DIR_NAME,
  defaultAgentDir,
  isValidInstanceId,
} from "./private-sessions.js";

export { defaultAgentDir } from "./private-sessions.js";

export interface TrackedMember {
  name: string;
  cwd: string;
  spawnedAt: number;
  instanceId?: string;
  sessionDir?: string;
  sessionFile: string | null;
}

export interface SessionFileInfo {
  path: string;
  startedAt: number;
}

export interface CostCacheEntry {
  offset: number;
  cost: number;
}

export interface TeamStatusParts {
  teamName: string | null;
  cost: number | null;
  rpcCounts: { running: number; idle: number } | null;
}

const SESSION_FILE_RE = /^(\d{4})-(\d{2})-(\d{2})T(\d{2})-(\d{2})-(\d{2})-(\d{3})Z_.+\.jsonl$/;
const SESSION_NAME_SCAN_BYTES = 16_384;

export function sessionDirForCwd(cwd: string, agentDir: string = defaultAgentDir()): string {
  const resolved = path.resolve(cwd);
  const safe = `--${resolved.replace(/^[/\\]/, "").replace(/[/\\:]/g, "-")}--`;
  return path.join(agentDir, "sessions", safe);
}

export function parseSessionStart(filename: string): number | null {
  const match = SESSION_FILE_RE.exec(filename);
  if (!match) return null;
  const [, year, month, day, hour, minute, second, millis] = match;
  return Date.parse(`${year}-${month}-${day}T${hour}:${minute}:${second}.${millis}Z`);
}

export function listSessionFiles(dir: string): SessionFileInfo[] {
  let names: string[];
  try {
    names = fs.readdirSync(dir);
  } catch {
    return [];
  }
  const files: SessionFileInfo[] = [];
  for (const name of names) {
    const startedAt = parseSessionStart(name);
    if (startedAt === null) continue;
    files.push({ path: path.join(dir, name), startedAt });
  }
  return files.sort((a, b) => a.startedAt - b.startedAt);
}

export function sessionFileHasName(filePath: string, name: string): boolean {
  let text: string;
  try {
    const fd = fs.openSync(filePath, "r");
    const buffer = Buffer.alloc(SESSION_NAME_SCAN_BYTES);
    let bytesRead: number;
    try {
      bytesRead = fs.readSync(fd, buffer, 0, buffer.length, 0);
    } finally {
      fs.closeSync(fd);
    }
    text = buffer.toString("utf8", 0, bytesRead);
  } catch {
    return false;
  }
  for (const line of text.split("\n")) {
    if (!line.trim()) continue;
    let entry: unknown;
    try {
      entry = JSON.parse(line);
    } catch {
      continue;
    }
    const parsed = entry as { type?: unknown; name?: unknown };
    if (parsed.type === "session_info" && parsed.name === name) return true;
  }
  return false;
}

export function resolveSessionFiles(members: TrackedMember[], agentDir: string = defaultAgentDir()): void {
  const claimed = new Set<string>();
  for (const member of members) {
    if (member.sessionFile !== null) claimed.add(member.sessionFile);
  }

  for (const member of members) {
    if (member.sessionFile !== null || !member.instanceId || !member.sessionDir) continue;
    const file = listSessionFiles(member.sessionDir).find(
      (candidate) => !claimed.has(candidate.path) && sessionFileHasName(candidate.path, `@${member.name}`),
    );
    if (!file) continue;
    member.sessionFile = file.path;
    claimed.add(file.path);
  }

  const pendingByCwd = new Map<string, TrackedMember[]>();
  for (const member of members) {
    if (member.sessionFile !== null) continue;
    if (member.instanceId !== undefined || member.sessionDir !== undefined) continue;
    const pending = pendingByCwd.get(member.cwd) ?? [];
    pending.push(member);
    pendingByCwd.set(member.cwd, pending);
  }
  for (const [cwd, pending] of pendingByCwd) {
    const files = listSessionFiles(sessionDirForCwd(cwd, agentDir));
    pending.sort((a, b) => a.spawnedAt - b.spawnedAt);
    for (const member of pending) {
      const file = files.find(
        (candidate) =>
          !claimed.has(candidate.path) &&
          candidate.startedAt >= member.spawnedAt &&
          sessionFileHasName(candidate.path, `@${member.name}`),
      );
      if (!file) continue;
      member.sessionFile = file.path;
      claimed.add(file.path);
    }
  }
}

export function sumSessionCost(filePath: string, cache: Map<string, CostCacheEntry>): number {
  let size: number;
  try {
    size = fs.statSync(filePath).size;
  } catch {
    return cache.get(filePath)?.cost ?? 0;
  }
  const cached = cache.get(filePath);
  let offset = cached?.offset ?? 0;
  let cost = cached?.cost ?? 0;
  if (size < offset) {
    offset = 0;
    cost = 0;
  }
  if (size === offset) return cost;
  let chunk: Buffer;
  try {
    const fd = fs.openSync(filePath, "r");
    const buffer = Buffer.alloc(size - offset);
    let bytesRead: number;
    try {
      bytesRead = fs.readSync(fd, buffer, 0, buffer.length, offset);
    } finally {
      fs.closeSync(fd);
    }
    chunk = buffer.subarray(0, bytesRead);
  } catch {
    return cost;
  }
  const lastNewline = chunk.lastIndexOf(10);
  if (lastNewline < 0) return cost;
  const consumed = chunk.subarray(0, lastNewline + 1);
  for (const line of consumed.toString("utf8").split("\n")) {
    if (!line.trim()) continue;
    let entry: unknown;
    try {
      entry = JSON.parse(line);
    } catch {
      continue;
    }
    const total = (entry as { message?: { usage?: { cost?: { total?: unknown } } } }).message?.usage?.cost?.total;
    if (typeof total === "number") cost += total;
  }
  cache.set(filePath, { offset: offset + consumed.length, cost });
  return cost;
}

export function memberFromEntryData(
  data: unknown,
): Omit<TrackedMember, "sessionFile"> | null {
  const parsed = data as {
    name?: unknown;
    cwd?: unknown;
    spawnedAt?: unknown;
    instanceId?: unknown;
    sessionDir?: unknown;
  } | null | undefined;
  if (!parsed) return null;
  if (typeof parsed.name !== "string" || typeof parsed.cwd !== "string" || typeof parsed.spawnedAt !== "number") {
    return null;
  }

  const hasInstanceId = parsed.instanceId !== undefined;
  const hasSessionDir = parsed.sessionDir !== undefined;
  if (hasInstanceId !== hasSessionDir) return null;
  if (!hasInstanceId) return { name: parsed.name, cwd: parsed.cwd, spawnedAt: parsed.spawnedAt };
  if (
    typeof parsed.instanceId !== "string" ||
    !isValidInstanceId(parsed.instanceId) ||
    typeof parsed.sessionDir !== "string" ||
    !path.isAbsolute(parsed.sessionDir) ||
    path.basename(parsed.sessionDir) !== parsed.instanceId ||
    path.basename(path.dirname(parsed.sessionDir)) !== PRIVATE_SESSION_DIR_NAME
  ) {
    return null;
  }
  return {
    name: parsed.name,
    cwd: parsed.cwd,
    spawnedAt: parsed.spawnedAt,
    instanceId: parsed.instanceId,
    sessionDir: parsed.sessionDir,
  };
}

export function formatCost(cost: number): string {
  return `$${cost.toFixed(2)}`;
}

export function formatTeamStatus(parts: TeamStatusParts): string | undefined {
  const costSegment = parts.cost === null ? null : `team cost ${formatCost(parts.cost)}`;
  if (parts.rpcCounts) {
    const counts: string[] = [];
    if (parts.rpcCounts.running > 0) counts.push(`${parts.rpcCounts.running} running`);
    if (parts.rpcCounts.idle > 0) counts.push(`${parts.rpcCounts.idle} idle`);
    const segments = [`Teammates: ${counts.join(", ")}`];
    if (costSegment) segments.push(costSegment);
    segments.push("Ctrl+Shift+M to view");
    return segments.join(" · ");
  }
  if (parts.teamName) {
    const base = `@team_lead [${parts.teamName}]`;
    return costSegment ? `${base} · ${costSegment}` : base;
  }
  return costSegment ? `${costSegment} (session total)` : undefined;
}

export class TeamCostTracker {
  private readonly members: TrackedMember[] = [];
  private readonly costCache = new Map<string, CostCacheEntry>();
  private readonly agentDir: string;

  constructor(agentDir: string = defaultAgentDir()) {
    this.agentDir = agentDir;
  }

  addMember(
    name: string,
    cwd: string,
    spawnedAt: number,
    instanceId?: string,
    sessionDir?: string,
  ): void {
    if ((instanceId === undefined) !== (sessionDir === undefined)) {
      throw new Error("Agent Teams cost identity requires both instanceId and sessionDir");
    }
    const exists = this.members.some((member) =>
      instanceId !== undefined
        ? member.instanceId === instanceId && member.sessionDir === sessionDir
        : member.instanceId === undefined && member.name === name && member.cwd === cwd && member.spawnedAt === spawnedAt,
    );
    if (exists) return;
    this.members.push({ name, cwd, spawnedAt, instanceId, sessionDir, sessionFile: null });
  }

  hasMembers(): boolean {
    return this.members.length > 0;
  }

  totalCost(): number {
    resolveSessionFiles(this.members, this.agentDir);
    let total = 0;
    for (const member of this.members) {
      if (member.sessionFile === null) continue;
      total += sumSessionCost(member.sessionFile, this.costCache);
    }
    return total;
  }
}
