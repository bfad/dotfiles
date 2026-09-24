/**
 * Team configuration & file-based mailbox.
 *
 * Storage layout (under ~/.pi/teams/<team-name>/):
 *   config.json              – team metadata & member list
 *   mailbox/<member-name>/   – incoming messages (one JSON file each)
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import type { ContextUsage, MemberConfig, MessageOrder, TeamConfig, TeamMessage, VscodeWorkspace } from "./types.js";
export type { ContextUsage, MemberConfig, MessageOrder, TeamConfig, TeamMessage, VscodeWorkspace };

export type MemberStatus = "working" | "waiting-for-lead" | "completed-awaiting-shutdown";
export interface MemberState {
  status: MemberStatus;
  reason?: string;
  updatedAt: number;
  lastProgressAt?: number;
}

const MEMBER_STATUSES = new Set<MemberStatus>(["working", "waiting-for-lead", "completed-awaiting-shutdown"]);
const TEAMS_ROOT = path.join(os.homedir(), ".pi", "teams");
const MAX_LOGICAL_MS = 999_999_999_999_999;
const MAX_SEQUENCE = 999999;
const MAX_PUBLICATION_ATTEMPTS = 100;
const ORDER_STEM_PATTERN = /^(\d{15})-([0-9a-z]{13})-(\d{6})-([0-9a-z]{12})$/;
const PATH_SAFE_WRITER_PATTERN = /^[0-9a-z]{13}$/;
const PATH_SAFE_NONCE_PATTERN = /^[0-9a-z]{12}$/;

export interface MailboxStorage {
	mkdirSync(dir: string, opts: { recursive: true }): void;
	writeFileSync(file: string, data: string): void;
	linkSync(existing: string, next: string): void;
	unlinkSync(file: string): void;
	renameSync(from: string, to: string): void;
	readdirSync(dir: string): string[];
	readFileSync(file: string, enc: "utf-8"): string;
	existsSync(file: string): boolean;
	rmSync(file: string, opts: { recursive: true; force: true }): void;
}

export interface Clock {
	now(): number;
}

export interface TeamDeps {
	fs: MailboxStorage;
	clock: Clock;
	writerId: string;
	nonce: () => string;
	root?: string;
	order: { lastLogicalMs: number; seq: number };
}

const nodeStorage: MailboxStorage = {
	mkdirSync: (dir, opts) => fs.mkdirSync(dir, opts),
	writeFileSync: (file, data) => fs.writeFileSync(file, data),
	linkSync: (existing, next) => fs.linkSync(existing, next),
	unlinkSync: (file) => fs.unlinkSync(file),
	renameSync: (from, to) => fs.renameSync(from, to),
	readdirSync: (dir) => fs.readdirSync(dir),
	readFileSync: (file, enc) => fs.readFileSync(file, enc),
	existsSync: (file) => fs.existsSync(file),
	rmSync: (file, opts) => fs.rmSync(file, opts),
};

function randomBase36(width: number): string {
	return randomUUID().replaceAll("-", "").slice(0, width).toLowerCase();
}

export function createTeamDeps(overrides: Partial<TeamDeps> = {}): TeamDeps {
	const pid = process.pid.toString(36).slice(-6).padStart(6, "0");
	return {
		fs: nodeStorage,
		clock: { now: () => Date.now() },
		writerId: `${pid}${randomBase36(7)}`,
		nonce: () => randomBase36(12),
		order: { lastLogicalMs: -1, seq: -1 },
		...overrides,
	};
}

const defaultDeps = createTeamDeps();

// ---------------------------------------------------------------------------
// Path helpers
// ---------------------------------------------------------------------------

function teamDir(teamName: string): string {
	return path.join(TEAMS_ROOT, teamName);
}

function teamDirWithDeps(teamName: string, deps: TeamDeps): string {
	return path.join(deps.root ?? TEAMS_ROOT, teamName);
}

function mailboxDirWithDeps(teamName: string, memberName: string, deps: TeamDeps): string {
	return path.join(teamDirWithDeps(teamName, deps), "mailbox", memberName);
}

function configPath(teamName: string): string {
	return path.join(teamDir(teamName), "config.json");
}

function configPathWithDeps(teamName: string, deps: TeamDeps): string {
  return path.join(teamDirWithDeps(teamName, deps), "config.json");
}

function memberStatePathWithDeps(teamName: string, memberName: string, deps: TeamDeps): string {
  return path.join(teamDirWithDeps(teamName, deps), "member-state", `${memberName}.json`);
}

export function mailboxDir(teamName: string, memberName: string): string {
	return path.join(teamDir(teamName), "mailbox", memberName);
}

export function oversizedDir(teamName: string): string {
	return path.join(teamDir(teamName), "oversized");
}

// ---------------------------------------------------------------------------
// Team CRUD
// ---------------------------------------------------------------------------

export function createTeam(name: string, leadPaneId: string, cwd: string): TeamConfig {
	const config: TeamConfig = {
		name,
		leadPaneId,
		cwd,
		members: [{ name: "team_lead", role: "lead", paneId: leadPaneId, spawnedAt: Date.now() }],
		createdAt: Date.now(),
	};
	fs.mkdirSync(teamDir(name), { recursive: true });
	fs.mkdirSync(mailboxDir(name, "team_lead"), { recursive: true });
	writeConfig(name, config);
	return config;
}

export function readConfig(teamName: string): TeamConfig | null {
	try {
		return JSON.parse(fs.readFileSync(configPath(teamName), "utf-8"));
	} catch {
		return null;
	}
}

export function writeConfig(teamName: string, config: TeamConfig): void {
	const tmp = configPath(teamName) + ".tmp";
	fs.writeFileSync(tmp, JSON.stringify(config, null, 2));
	fs.renameSync(tmp, configPath(teamName));
}

export function addMember(teamName: string, member: MemberConfig): void {
	const config = readConfig(teamName);
	if (!config) throw new Error(`Team ${teamName} not found`);
	config.members.push(member);
	fs.mkdirSync(mailboxDir(teamName, member.name), { recursive: true });
	writeConfig(teamName, config);
}

export function removeMember(teamName: string, memberName: string): void {
	const config = readConfig(teamName);
	if (!config) return;
	config.members = config.members.filter((m) => m.name !== memberName);
	writeConfig(teamName, config);
}

function readConfigWithDeps(teamName: string, deps: TeamDeps): TeamConfig | null {
  try {
    return JSON.parse(deps.fs.readFileSync(configPathWithDeps(teamName, deps), "utf-8"));
  } catch {
    return null;
  }
}

function writeConfigWithDeps(teamName: string, config: TeamConfig, deps: TeamDeps): void {
  const target = configPathWithDeps(teamName, deps);
  const temp = `${target}.tmp`;
  deps.fs.writeFileSync(temp, JSON.stringify(config, null, 2));
  deps.fs.renameSync(temp, target);
}

function isMemberState(value: unknown): value is MemberState {
  if (!isRecord(value) || typeof value.status !== "string" || !MEMBER_STATUSES.has(value.status as MemberStatus)) return false;
  if (!isFiniteNumber(value.updatedAt)) return false;
  if (value.reason !== undefined && typeof value.reason !== "string") return false;
  return value.lastProgressAt === undefined || isFiniteNumber(value.lastProgressAt);
}

export function readMemberState(
  teamName: string,
  memberName: string,
  deps: TeamDeps = defaultDeps,
): MemberState | null {
  try {
    const state = JSON.parse(deps.fs.readFileSync(memberStatePathWithDeps(teamName, memberName, deps), "utf-8"));
    return isMemberState(state) ? state : null;
  } catch {
    return null;
  }
}

export function writeMemberState(
  teamName: string,
  memberName: string,
  state: MemberState,
  deps: TeamDeps = defaultDeps,
): void {
  if (!isMemberState(state)) throw new Error("Invalid member state");
  const dir = path.join(teamDirWithDeps(teamName, deps), "member-state");
  const target = memberStatePathWithDeps(teamName, memberName, deps);
  const temp = path.join(dir, `.${memberName}.${deps.nonce()}.tmp`);
  deps.fs.mkdirSync(dir, { recursive: true });
  deps.fs.writeFileSync(temp, JSON.stringify(state));
  deps.fs.renameSync(temp, target);
}

export function removeMemberState(
  teamName: string,
  memberName: string,
  deps: TeamDeps = defaultDeps,
): void {
  try {
    deps.fs.unlinkSync(memberStatePathWithDeps(teamName, memberName, deps));
  } catch (error) {
    if (!isMissingFile(error)) throw error;
  }
}

export function reserveMember(
  teamName: string,
  member: MemberConfig,
  deps: TeamDeps = defaultDeps,
): "reserved" | "name-held" {
  const config = readConfigWithDeps(teamName, deps);
  if (!config) throw new Error(`Team ${teamName} not found`);
  if (config.members.some((existing) => existing.name === member.name)) return "name-held";

  // Removing a registration never emptied the name-scoped mailbox, so a reused
  // name could hand a fresh instance the previous occupant's unread messages
  // (obsolete work, or a stale shutdown request). Reserving a name means a
  // clean slate: anything still queued was addressed to an instance that is
  // already gone.
  // Moved aside as a whole rather than emptied file by file: sendMessage()
  // publishes into this directory with a .tmp write followed by a hard link,
  // and unlinking underneath an in-flight sender would delete files it is
  // still using. A rename leaves the sender's inodes intact, and any send that
  // races with it fails cleanly instead of half-landing.
  const dir = mailboxDirWithDeps(teamName, member.name, deps);
  if (deps.fs.existsSync(dir)) {
    const discarded = `${dir}.discarded-${deps.clock.now()}`;
    try {
      deps.fs.renameSync(dir, discarded);
      deps.fs.rmSync(discarded, { recursive: true, force: true });
    } catch (error) {
      if (!isMissingFile(error)) throw error;
    }
  }
  deps.fs.mkdirSync(dir, { recursive: true });
  config.members.push({ ...member, state: "starting" });
  writeConfigWithDeps(teamName, config, deps);
  return "reserved";
}

function transitionMemberRegistration(
  teamName: string,
  memberName: string,
  expectedInstanceId: string,
  state: "running" | "stopping",
  deps: TeamDeps,
): boolean {
  const config = readConfigWithDeps(teamName, deps);
  const member = config?.members.find((candidate) => candidate.name === memberName);
  if (!config || member?.instanceId !== expectedInstanceId) return false;
  member.state = state;
  writeConfigWithDeps(teamName, config, deps);
  return true;
}

export function finalizeMemberRegistration(
  teamName: string,
  memberName: string,
  expectedInstanceId: string,
  deps: TeamDeps = defaultDeps,
): "running" | "skipped-mismatch" {
  return transitionMemberRegistration(teamName, memberName, expectedInstanceId, "running", deps)
    ? "running"
    : "skipped-mismatch";
}

export function markMemberStopping(
  teamName: string,
  memberName: string,
  expectedInstanceId: string,
  deps: TeamDeps = defaultDeps,
): "stopping" | "skipped-mismatch" {
  return transitionMemberRegistration(teamName, memberName, expectedInstanceId, "stopping", deps)
    ? "stopping"
    : "skipped-mismatch";
}

export function removeMemberRegistration(
  teamName: string,
  memberName: string,
  expectedInstanceId: string,
  deps: TeamDeps = defaultDeps,
): "removed" | "skipped-mismatch" | "skipped-no-instance" {
  const config = readConfigWithDeps(teamName, deps);
  const memberIndex = config?.members.findIndex((candidate) => candidate.name === memberName) ?? -1;
  if (!config || memberIndex < 0) return "skipped-mismatch";
  const member = config.members[memberIndex];
  if (member.instanceId === undefined) return "skipped-no-instance";
  if (member.instanceId !== expectedInstanceId) return "skipped-mismatch";

  try {
    deps.fs.unlinkSync(memberStatePathWithDeps(teamName, memberName, deps));
  } catch {
    // Registration teardown is best-effort for name-scoped state residue.
  }
  config.members.splice(memberIndex, 1);
  writeConfigWithDeps(teamName, config, deps);
  return "removed";
}

export function addVscodeWorkspace(teamName: string, workspacePath: string, branch?: string): void {
	const config = readConfig(teamName);
	if (!config) throw new Error(`Team ${teamName} not found`);
	const workspaces = config.vscodeWorkspaces ?? [];
	const entry: VscodeWorkspace = { path: workspacePath };
	if (branch) entry.branch = branch;
	// Upsert by path — replace existing entry or append
	const idx = workspaces.findIndex((w) => w.path === workspacePath);
	if (idx >= 0) {
		workspaces[idx] = entry;
	} else {
		workspaces.push(entry);
	}
	config.vscodeWorkspaces = workspaces;
	writeConfig(teamName, config);
}

// ---------------------------------------------------------------------------
// Mailbox
// ---------------------------------------------------------------------------

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null && !Array.isArray(value);
}

function isFiniteNumber(value: unknown): value is number {
	return typeof value === "number" && Number.isFinite(value);
}

function assertOrder(order: unknown): asserts order is MessageOrder {
	if (!isRecord(order)) throw new Error("Message order must be an object");
	if (order.v !== 1) throw new Error("Unsupported message order version");
	if (!Number.isInteger(order.logicalMs) || (order.logicalMs as number) < 0 || (order.logicalMs as number) > MAX_LOGICAL_MS) {
		throw new Error("Message order logicalMs must be a 15-digit nonnegative integer");
	}
	if (typeof order.writerId !== "string" || !PATH_SAFE_WRITER_PATTERN.test(order.writerId)) {
		throw new Error("Message order writerId must contain exactly 13 lowercase base36 characters");
	}
	if (!Number.isInteger(order.seq) || (order.seq as number) < 0 || (order.seq as number) > MAX_SEQUENCE) {
		throw new Error("Message order seq must be an integer from 0 through 999999");
	}
	if (typeof order.nonce !== "string" || !PATH_SAFE_NONCE_PATTERN.test(order.nonce)) {
		throw new Error("Message order nonce must contain exactly 12 lowercase base36 characters");
	}
}

export function encodeMessageOrder(order: MessageOrder): string {
	assertOrder(order);
	return `${String(order.logicalMs).padStart(15, "0")}-${order.writerId}-${String(order.seq).padStart(6, "0")}-${order.nonce}`;
}

export function decodeMessageOrder(stem: string): MessageOrder {
	const match = ORDER_STEM_PATTERN.exec(stem);
	if (!match) throw new Error("Malformed message order basename");
	const order: MessageOrder = {
		v: 1,
		logicalMs: Number(match[1]),
		writerId: match[2],
		seq: Number(match[3]),
		nonce: match[4],
	};
	assertOrder(order);
	return order;
}

export function assertTeamMessage(message: unknown, expectedId?: string): asserts message is TeamMessage {
	if (!isRecord(message)) throw new Error("Team message must be an object");
	if (typeof message.id !== "string") throw new Error("Team message id must be a string");
	if (typeof message.from !== "string") throw new Error("Team message from must be a string");
	if (typeof message.to !== "string") throw new Error("Team message to must be a string");
	if (typeof message.content !== "string") throw new Error("Team message content must be a string");
	if (!isFiniteNumber(message.timestamp)) throw new Error("Team message timestamp must be a finite number");
	if (message.order !== undefined) {
		assertOrder(message.order);
		if (message.id !== encodeMessageOrder(message.order)) throw new Error("Team message id does not match its order tuple");
	}
	if (expectedId !== undefined && message.id !== expectedId) throw new Error("Team message id does not match its basename");
	if (message.contextUsage !== undefined) {
		if (!isRecord(message.contextUsage)) throw new Error("Team message contextUsage must be an object");
		const { tokens, contextWindow, percent } = message.contextUsage;
		if (tokens !== null && !isFiniteNumber(tokens)) throw new Error("Team message contextUsage.tokens is invalid");
		if (!isFiniteNumber(contextWindow)) throw new Error("Team message contextUsage.contextWindow is invalid");
		if (percent !== null && !isFiniteNumber(percent)) throw new Error("Team message contextUsage.percent is invalid");
	}
}

export class BatchValidationError extends Error {
  constructor(file: string, cause: unknown) {
    super(`Mailbox batch validation failed for ${file}`, { cause });
    this.name = "BatchValidationError";
  }
}

export function peekMailboxBatch(
  teamName: string,
  memberName: string,
  deps: TeamDeps = defaultDeps,
): { messages: TeamMessage[]; ids: string[] } {
  const dir = mailboxDirWithDeps(teamName, memberName, deps);
  if (!deps.fs.existsSync(dir)) return { messages: [], ids: [] };

  const files = deps.fs.readdirSync(dir).filter((file) => file.endsWith(".json")).sort();
  const messages: TeamMessage[] = [];
  for (const file of files) {
    try {
      const stem = file.slice(0, -".json".length);
      decodeMessageOrder(stem);
      const message: unknown = JSON.parse(deps.fs.readFileSync(path.join(dir, file), "utf-8"));
      assertTeamMessage(message, stem);
      if (message.order === undefined) throw new Error("Team message order is required for batch delivery");
      messages.push(message);
    } catch (error) {
      throw new BatchValidationError(file, error);
    }
  }
  return { messages, ids: messages.map((message) => message.id) };
}

function isMissingFile(error: unknown): boolean {
  return typeof error === "object" && error !== null && "code" in error && error.code === "ENOENT";
}

export function deleteMailboxMessages(
  teamName: string,
  memberName: string,
  ids: string[],
  deps: TeamDeps = defaultDeps,
): { deleted: string[]; failed: string[] } {
  const dir = mailboxDirWithDeps(teamName, memberName, deps);
  const deleted: string[] = [];
  const failed: string[] = [];

  for (const id of ids) {
    try {
      decodeMessageOrder(id);
    } catch {
      failed.push(id);
      continue;
    }

    try {
      deps.fs.unlinkSync(path.join(dir, `${id}.json`));
      deleted.push(id);
    } catch (error) {
      if (!isMissingFile(error)) failed.push(id);
    }
  }

  return { deleted, failed };
}

function nextMessageOrder(deps: TeamDeps, now: number, nonce: string): MessageOrder {
	if (!Number.isInteger(now) || now < 0 || now > MAX_LOGICAL_MS) throw new Error("Clock must return an encodable millisecond integer");
	if (!Number.isInteger(deps.order.lastLogicalMs) || deps.order.lastLogicalMs < -1 || deps.order.lastLogicalMs > MAX_LOGICAL_MS) {
		throw new Error("Invalid lastLogicalMs order state");
	}
	if (!Number.isInteger(deps.order.seq) || deps.order.seq < -1 || deps.order.seq > MAX_SEQUENCE) {
		throw new Error("Invalid sequence order state");
	}
	let logicalMs = Math.max(now, deps.order.lastLogicalMs);
	let seq = logicalMs > deps.order.lastLogicalMs ? 0 : deps.order.seq + 1;
	if (seq > MAX_SEQUENCE) {
		logicalMs += 1;
		seq = 0;
	}
	const order = { v: 1 as const, logicalMs, writerId: deps.writerId, seq, nonce };
	encodeMessageOrder(order);
	deps.order.lastLogicalMs = logicalMs;
	deps.order.seq = seq;
	return order;
}

function isAlreadyExists(error: unknown): boolean {
	return typeof error === "object" && error !== null && "code" in error && error.code === "EEXIST";
}

function bestEffortUnlink(storage: MailboxStorage, file: string): void {
	try {
		storage.unlinkSync(file);
	} catch {
		// Ignored: a linked final is already published, and temp residue is non-.json.
	}
}

export function sendMessage(
	teamName: string,
	from: string,
	to: string,
	content: string,
	contextUsage?: ContextUsage,
	deps: TeamDeps = defaultDeps,
): TeamMessage {
	const dir = mailboxDirWithDeps(teamName, to, deps);
	deps.fs.mkdirSync(dir, { recursive: true });
	const now = deps.clock.now();
	const order = nextMessageOrder(deps, now, deps.nonce());

	for (let attempt = 0; attempt < MAX_PUBLICATION_ATTEMPTS; attempt++) {
		if (attempt > 0) order.nonce = deps.nonce();
		const id = encodeMessageOrder(order);
		const message: TeamMessage = { id, order: { ...order }, from, to, content, timestamp: now };
		if (contextUsage) message.contextUsage = contextUsage;
		const temp = path.join(dir, `.${id}.${attempt}.tmp`);
		const final = path.join(dir, `${id}.json`);
		deps.fs.writeFileSync(temp, JSON.stringify(message));
		try {
			deps.fs.linkSync(temp, final);
		} catch (error) {
			bestEffortUnlink(deps.fs, temp);
			if (isAlreadyExists(error)) continue;
			throw error;
		}
		bestEffortUnlink(deps.fs, temp);
		return message;
	}

	throw new Error(`Unable to publish mailbox message after ${MAX_PUBLICATION_ATTEMPTS} collisions`);
}

/** Read and delete all pending messages for a member (oldest first). */
export function pollMailbox(teamName: string, memberName: string): TeamMessage[] {
	const dir = mailboxDir(teamName, memberName);
	if (!fs.existsSync(dir)) return [];

	const messages: TeamMessage[] = [];
	for (const file of fs.readdirSync(dir).filter((f) => f.endsWith(".json")).sort()) {
		const fp = path.join(dir, file);
		try {
			messages.push(JSON.parse(fs.readFileSync(fp, "utf-8")));
			fs.unlinkSync(fp);
		} catch {
			/* skip corrupt files */
		}
	}
	return messages;
}

// ---------------------------------------------------------------------------
// Cleanup
// ---------------------------------------------------------------------------

export function cleanupTeam(teamName: string): void {
	const dir = teamDir(teamName);
	if (fs.existsSync(dir)) {
		fs.rmSync(dir, { recursive: true, force: true });
	}
}

// ---------------------------------------------------------------------------
// Role detection
// ---------------------------------------------------------------------------

export function getTeamEnv(): {
	teamName: string | null;
	role: "lead" | "teammate" | null;
	agentName: string | null;
} {
	return {
		teamName: process.env.PI_TEAM_NAME ?? null,
		role: (process.env.PI_TEAM_ROLE as "lead" | "teammate") ?? null,
		agentName: process.env.PI_TEAM_AGENT_NAME ?? null,
	};
}
