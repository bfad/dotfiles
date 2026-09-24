import * as fs from "node:fs";
import * as path from "node:path";
import { withFileMutationQueue } from "@mariozechner/pi-coding-agent";
import type { SubagentRunMode } from "./artifacts.js";
import {
  RUN_EVENTS_FILENAME,
  RUN_STATUS_FILENAME,
  defaultRunStateDir as defaultRunStateDirFromPaths,
  defaultRunStateRoot as defaultRunStateRootFromPaths,
} from "./run-paths.js";
import type { AgentScope, ChildActivityUpdate, ResultOutputSummary, SingleResult, UsageStats, WorktreeCleanupPolicy, WorktreeCleanupState } from "./types.js";
import { cleanupWorktree, type CleanupWorktreeOptions, type WorktreeMetadata } from "./worktrees.js";

export type RunStatusState = "running" | "succeeded" | "failed" | "canceled";
export type RunTaskState = "pending" | "running" | "succeeded" | "failed" | "canceled" | "skipped";
export type RunEventType =
  | "run_started"
  | "child_started"
  | "child_message"
  | "child_tool_started"
  | "child_tool_finished"
  | "child_stderr"
  | "child_finished"
  | "child_skipped"
  | "artifact_written"
  | "run_finished"
  | "run_failed"
  | "run_canceled";

export interface RecentToolActivity {
  name: string;
  toolCallId?: string;
  startedAt?: string;
  finishedAt?: string;
  path?: string;
  status?: "running" | "finished";
  preview?: string;
  isError?: boolean;
}

export interface RunTaskSummary {
  id: string;
  agent: string;
  task: string;
  state: RunTaskState;
  cwd?: string;
  step?: number;
  startedAt?: string;
  endedAt?: string;
  exitCode?: number;
  stopReason?: string;
  errorMessage?: string;
  skipReason?: string;
  artifactDir?: string;
  artifactResultPath?: string;
  artifactTranscriptPath?: string;
  artifactStderrPath?: string;
  artifactSummaryPath?: string;
  outputPath?: string;
  outputSummary?: ResultOutputSummary;
  outputLogPath?: string;
  lastActivityAt?: string;
  currentTool?: string;
  currentToolStartedAt?: string;
  currentPath?: string;
  recentTools?: RecentToolActivity[];
  recentOutput?: string[];
  turnCount?: number;
  toolCount?: number;
  model?: string;
  usage?: UsageStats;
  worktreePath?: string;
  worktreeBranch?: string;
  worktreeBaseCommit?: string;
  worktreeCleanupState?: WorktreeCleanupState;
  worktreeCleanupPolicy?: WorktreeCleanupPolicy;
  worktreeCleanupReason?: string;
  worktreePatchCaptureError?: string;
  worktreeCleanupError?: string;
  worktreePatchPath?: string;
  worktreeDiffstatPath?: string;
  worktreeManifestPath?: string;
  worktreeNodeModulesLinked?: boolean;
  worktreeSetupHookPath?: string;
  worktreeSetupHookDurationMs?: number;
  worktreeSyntheticPaths?: string[];
}

export interface RunStatus {
  runId: string;
  mode: SubagentRunMode;
  state: RunStatusState;
  cwd: string;
  agentScope: AgentScope;
  parentSessionId?: string;
  startedAt: string;
  updatedAt: string;
  endedAt?: string;
  tasks: RunTaskSummary[];
  artifactDir: string;
  runStateDir: string;
  errors: string[];
  usage: UsageStats;
  warnings: string[];
  disabledAgents: string[];
  lastActivityAt?: string;
  turnCount: number;
  toolCount: number;
}

export interface RunEvent {
  runId: string;
  type: RunEventType;
  timestamp: string;
  mode?: SubagentRunMode;
  state?: RunStatusState | RunTaskState;
  taskId?: string;
  taskIndex?: number;
  agent?: string;
  artifactPaths?: string[];
  outputSummary?: ResultOutputSummary;
  message?: string;
  error?: string;
  toolName?: string;
  toolCallId?: string;
  path?: string;
  preview?: string;
  isError?: boolean;
}

export interface RecentRunsResult {
  statuses: RunStatus[];
  warnings: string[];
}

export type WorktreeCleanupExecutor = (options: CleanupWorktreeOptions) => Promise<void>;
export type PruneRunAction = "would-prune" | "pruned" | "skipped" | "failed";
export type PruneWorktreeAction = "would-clean" | "cleaned" | "skipped" | "failed";

export interface PruneStaleRunsOptions {
  cwd?: string;
  olderThanDays?: number;
  dryRun?: boolean;
  includeRunning?: boolean;
  pruneWorktrees?: boolean;
  now?: Date;
  cleanupWorktree?: WorktreeCleanupExecutor;
}

export interface PruneRunWorktreeResult {
  taskId: string;
  worktreePath: string;
  branchName?: string;
  action: PruneWorktreeAction;
  message?: string;
}

export interface PruneRunResult {
  runId: string;
  runStateDir: string;
  state?: RunStatusState;
  updatedAt?: string;
  ageDays: number;
  action: PruneRunAction;
  reason?: string;
  worktrees: PruneRunWorktreeResult[];
}

export interface PruneStaleRunsResult {
  root: string;
  cwd?: string;
  dryRun: boolean;
  olderThanDays: number;
  includeRunning: boolean;
  pruneWorktrees: boolean;
  scanned: number;
  candidateCount: number;
  prunedCount: number;
  wouldPruneCount: number;
  skippedCount: number;
  failedCount: number;
  runs: PruneRunResult[];
  warnings: string[];
}

export interface RunStateContext {
  cwd: string;
  runId: string;
  runStateDir: string;
  statusPath: string;
  eventsPath: string;
  artifactDir: string;
  artifactStatusPath?: string;
}

export interface StartRunStateOptions {
  runId: string;
  mode: SubagentRunMode;
  cwd: string;
  agentScope: AgentScope;
  artifactDir: string;
  tasks: RunTaskSummary[];
  warnings?: string[];
  disabledAgents?: string[];
  parentSessionId?: string;
  deferArtifactStatusMirror?: boolean;
}

export class RunStatusNotFoundError extends Error {
  constructor(readonly runId: string, readonly statusPath: string) {
    super(`No subagent run status found for runId "${runId}" at ${statusPath}`);
    this.name = "RunStatusNotFoundError";
  }
}

export class RunStatusCorruptError extends Error {
  constructor(readonly runId: string, readonly statusPath: string, cause: unknown) {
    super(`Subagent run status for runId "${runId}" is corrupt at ${statusPath}: ${errorMessage(cause)}`);
    this.name = "RunStatusCorruptError";
  }
}

export class RunStatusInvalidRunIdError extends Error {
  constructor(readonly runId: string) {
    super(`Invalid subagent runId "${runId}". Expected format YYYYMMDDTHHMMSSZ-xxxxxxxx (suffix at least 8 lowercase base36 chars).`);
    this.name = "RunStatusInvalidRunIdError";
  }
}

const RUN_ID_PATTERN = /^\d{8}T\d{6}Z-[0-9a-z]{8,}$/;
const DAY_MS = 24 * 60 * 60 * 1000;
const DEFAULT_PRUNE_OLDER_THAN_DAYS = 14;
const MAX_RECENT_RUN_CANDIDATES = 200;
const MAX_RECENT_TOOLS = 10;
const MAX_RECENT_OUTPUT = 8;
const MAX_ACTIVITY_PREVIEW_CHARS = 500;
const ANSI_ESCAPE_SEQUENCE_RE = /\u001b(?:\[[0-?]*[ -/]*[@-~]|\][^\u0007\u001b]*(?:\u0007|\u001b\\)?|[PX^_][\s\S]*?(?:\u001b\\|$)|[@-Z\\-_])/g;

function normalizeCwdScope(cwd: string | undefined): string | undefined {
  return cwd ? path.resolve(cwd) : undefined;
}

function isPathWithinCwdScope(scopeCwd: string, candidatePath: unknown): boolean {
  if (typeof candidatePath !== "string" || candidatePath.trim() === "") return false;
  const relative = path.relative(scopeCwd, path.resolve(candidatePath));
  return relative === "" || (relative !== "" && !relative.startsWith("..") && !path.isAbsolute(relative));
}

function parsedStatusMatchesCwdScope(parsed: unknown, cwd: string): boolean {
  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) return false;
  return isPathWithinCwdScope(cwd, (parsed as { cwd?: unknown }).cwd);
}

export function isValidRunId(runId: string): boolean {
  return RUN_ID_PATTERN.test(runId);
}

export function assertValidRunId(runId: string): void {
  if (!isValidRunId(runId)) throw new RunStatusInvalidRunIdError(runId);
}

export function emptyRunUsageStats(): UsageStats {
  return { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0, contextTokens: 0, turns: 0 };
}

export function defaultRunStateRoot(): string;
export function defaultRunStateRoot(_ignoredCwd: string): string;
export function defaultRunStateRoot(_ignoredCwd?: string): string {
  return defaultRunStateRootFromPaths();
}

export function defaultRunStateDir(runId: string): string;
export function defaultRunStateDir(_ignoredCwd: string, runId: string): string;
export function defaultRunStateDir(first: string, second?: string): string {
  return defaultRunStateDirFromPaths(second ?? first);
}

export function createRunStateContext(cwd: string, runId: string, artifactDir: string): RunStateContext {
  const resolvedCwd = path.resolve(cwd);
  const runStateDir = defaultRunStateDir(runId);
  const resolvedArtifactDir = path.resolve(artifactDir);
  const statusPath = path.join(runStateDir, RUN_STATUS_FILENAME);
  return {
    cwd: resolvedCwd,
    runId,
    runStateDir,
    statusPath,
    eventsPath: path.join(runStateDir, RUN_EVENTS_FILENAME),
    artifactDir: resolvedArtifactDir,
    artifactStatusPath: path.resolve(runStateDir) === resolvedArtifactDir
      ? undefined
      : path.join(resolvedArtifactDir, "status.json"),
  };
}

function outputLogPathForTask(runStateDir: string, index: number): string {
  return path.join(runStateDir, `output-${String(index + 1).padStart(2, "0")}.log`);
}

function withLiveTaskDefaults(tasks: RunTaskSummary[], runStateDir: string): RunTaskSummary[] {
  return tasks.map((task, index) => ({
    ...task,
    outputLogPath: typeof task.outputLogPath === "string" ? task.outputLogPath : outputLogPathForTask(runStateDir, index),
    recentTools: Array.isArray(task.recentTools) ? task.recentTools : [],
    recentOutput: Array.isArray(task.recentOutput) ? task.recentOutput.filter((item): item is string => typeof item === "string") : [],
    turnCount: typeof task.turnCount === "number" && Number.isFinite(task.turnCount) ? task.turnCount : task.usage?.turns ?? 0,
    toolCount: typeof task.toolCount === "number" && Number.isFinite(task.toolCount) ? task.toolCount : 0,
    model: task.model,
  }));
}

function ensureTaskLiveDefaults(task: RunTaskSummary, runStateDir: string, index: number): RunTaskSummary {
  return withLiveTaskDefaults([task], runStateDir)[0]!;
}

function closeRunningTools(tools: RecentToolActivity[] | undefined, timestamp: string): RecentToolActivity[] | undefined {
  if (!Array.isArray(tools)) return tools;
  return tools.map((tool) => tool.status === "running"
    ? { ...tool, status: "finished" as const, finishedAt: tool.finishedAt ?? timestamp }
    : tool);
}

export function createRunTaskSummary(
  agent: string,
  task: string,
  index: number,
  options: { cwd?: string; step?: number; artifactDir?: string } = {},
): RunTaskSummary {
  return {
    id: `task-${String(index + 1).padStart(2, "0")}`,
    agent,
    task,
    state: "pending",
    ...(options.cwd ? { cwd: options.cwd } : {}),
    ...(options.step !== undefined ? { step: options.step } : {}),
    ...(options.artifactDir ? { artifactDir: options.artifactDir } : {}),
  };
}

export function createInitialRunStatus(options: StartRunStateOptions, now = new Date()): RunStatus {
  const context = createRunStateContext(options.cwd, options.runId, options.artifactDir);
  const timestamp = now.toISOString();
  const tasks = withLiveTaskDefaults(options.tasks, context.runStateDir);
  return {
    runId: options.runId,
    mode: options.mode,
    state: "running",
    cwd: context.cwd,
    agentScope: options.agentScope,
    ...(options.parentSessionId ? { parentSessionId: options.parentSessionId } : {}),
    startedAt: timestamp,
    updatedAt: timestamp,
    tasks,
    artifactDir: context.artifactDir,
    runStateDir: context.runStateDir,
    errors: collectTaskErrors(tasks),
    usage: aggregateTaskUsage(tasks),
    warnings: options.warnings ?? [],
    disabledAgents: options.disabledAgents ?? [],
    turnCount: aggregateTaskTurnCount(tasks),
    toolCount: aggregateTaskToolCount(tasks),
  };
}

export function markTaskStarted(task: RunTaskSummary, cwd: string | undefined, now = new Date()): RunTaskSummary {
  const timestamp = now.toISOString();
  return {
    ...task,
    state: "running",
    cwd: cwd ?? task.cwd,
    startedAt: task.startedAt ?? timestamp,
    lastActivityAt: task.lastActivityAt ?? timestamp,
  };
}

export function markTaskFromResult(task: RunTaskSummary, result: SingleResult, now = new Date()): RunTaskSummary {
  const failed = result.exitCode !== 0 || result.stopReason === "error" || result.stopReason === "aborted";
  const canceled = result.stopReason === "aborted";
  const endedAt = now.toISOString();
  return {
    ...task,
    agent: result.agent,
    task: result.task,
    state: canceled ? "canceled" : failed ? "failed" : "succeeded",
    endedAt,
    exitCode: result.exitCode,
    stopReason: result.stopReason,
    errorMessage: result.errorMessage,
    artifactDir: result.artifactDir ?? task.artifactDir,
    artifactResultPath: result.artifactResultPath,
    artifactTranscriptPath: result.artifactTranscriptPath,
    artifactStderrPath: result.artifactStderrPath,
    artifactSummaryPath: result.artifactSummaryPath,
    outputPath: result.outputPath,
    outputSummary: result.outputSummary,
    usage: result.usage,
    model: result.model ?? task.model,
    turnCount: Math.max(task.turnCount ?? 0, result.usage.turns ?? 0),
    lastActivityAt: task.lastActivityAt ?? endedAt,
    recentTools: closeRunningTools(task.recentTools, endedAt),
    currentTool: undefined,
    currentToolStartedAt: undefined,
    currentPath: undefined,
    ...(result.worktree
      ? {
          worktreePath: result.worktree.worktreePath,
          worktreeBranch: result.worktree.branchName,
          worktreeBaseCommit: result.worktree.baseCommit,
          worktreeCleanupState: result.worktree.cleanupState,
          worktreeCleanupPolicy: result.worktree.cleanupPolicy,
          worktreeCleanupReason: result.worktree.cleanupReason,
          worktreePatchCaptureError: result.worktree.patchCaptureError,
          worktreeCleanupError: result.worktree.cleanupError,
          worktreePatchPath: result.worktree.patchPath,
          worktreeDiffstatPath: result.worktree.diffstatPath,
          worktreeManifestPath: result.worktree.manifestPath,
          worktreeNodeModulesLinked: result.worktree.nodeModulesLinked,
          worktreeSetupHookPath: result.worktree.setupHookPath,
          worktreeSetupHookDurationMs: result.worktree.setupHookDurationMs,
          worktreeSyntheticPaths: result.worktree.syntheticPaths,
        }
      : {}),
  };
}

export function markTaskFailedFromError(
  task: RunTaskSummary,
  error: unknown,
  canceled: boolean,
  now = new Date(),
): RunTaskSummary {
  const endedAt = now.toISOString();
  return {
    ...task,
    state: canceled ? "canceled" : "failed",
    endedAt,
    exitCode: 1,
    stopReason: canceled ? "aborted" : "error",
    errorMessage: errorMessage(error),
    lastActivityAt: task.lastActivityAt ?? endedAt,
    recentTools: closeRunningTools(task.recentTools, endedAt),
    currentTool: undefined,
    currentToolStartedAt: undefined,
    currentPath: undefined,
  };
}

export function markTaskSkipped(task: RunTaskSummary, reason: string, now = new Date()): RunTaskSummary {
  const endedAt = now.toISOString();
  return {
    ...task,
    state: "skipped",
    endedAt,
    skipReason: reason,
    lastActivityAt: task.lastActivityAt ?? endedAt,
    recentTools: closeRunningTools(task.recentTools, endedAt),
    currentTool: undefined,
    currentToolStartedAt: undefined,
    currentPath: undefined,
  };
}

export function updateRunStatus(
  status: RunStatus,
  updates: Partial<Omit<RunStatus, "runId" | "mode" | "cwd" | "agentScope" | "parentSessionId" | "startedAt" | "runStateDir">>,
  now = new Date(),
): RunStatus {
  const state = updates.state ?? status.state;
  const tasks = updates.tasks ?? status.tasks;
  const timestamp = now.toISOString();
  const endedAt = state === "running" ? updates.endedAt ?? status.endedAt : updates.endedAt ?? status.endedAt ?? timestamp;
  return {
    ...status,
    ...updates,
    state,
    tasks,
    updatedAt: timestamp,
    ...(endedAt ? { endedAt } : {}),
    errors: updates.errors ?? collectTaskErrors(tasks),
    usage: updates.usage ?? aggregateTaskUsage(tasks),
    warnings: updates.warnings ?? status.warnings,
    lastActivityAt: updates.lastActivityAt ?? latestTaskActivityAt(tasks) ?? status.lastActivityAt,
    turnCount: updates.turnCount ?? aggregateTaskTurnCount(tasks),
    toolCount: updates.toolCount ?? aggregateTaskToolCount(tasks),
  };
}

export function runStateForFinishedTasks(tasks: RunTaskSummary[]): Exclude<RunStatusState, "running"> {
  if (tasks.some((task) => task.state === "canceled")) return "canceled";
  if (tasks.some((task) => task.state === "failed")) return "failed";
  return "succeeded";
}

function isTerminalRunState(state: RunStatusState): boolean {
  return state !== "running";
}

function unresolvedTaskError(state: Exclude<RunStatusState, "running">, error: unknown): unknown {
  if (error !== undefined) return error;
  return state === "canceled" ? "Run canceled before task completed." : "Run failed before task completed.";
}

function finishStateForTasks(
  requestedState: Exclude<RunStatusState, "running">,
  tasks: RunTaskSummary[],
): Exclude<RunStatusState, "running"> {
  if (requestedState === "succeeded" && tasks.some((task) => task.state === "running" || task.state === "pending")) {
    return "failed";
  }
  return requestedState;
}

function terminalizeUnresolvedTasks(
  tasks: RunTaskSummary[],
  state: Exclude<RunStatusState, "running">,
  error: unknown,
): RunTaskSummary[] {
  return tasks.map((task) => {
    if (task.state !== "running" && task.state !== "pending") return task;
    return markTaskFailedFromError(task, unresolvedTaskError(state, error), state === "canceled");
  });
}

function failStatusAfterStartupEventError(status: RunStatus, error: unknown): RunStatus {
  const startupError = `Failed to record run_started event: ${errorMessage(error)}`;
  const tasks = terminalizeUnresolvedTasks(status.tasks, "failed", startupError);
  const errors = Array.from(new Set([...collectTaskErrors(tasks), startupError]));
  return updateRunStatus(status, { state: "failed", tasks, errors });
}

export function aggregateTaskUsage(tasks: RunTaskSummary[]): UsageStats {
  const total = emptyRunUsageStats();
  for (const task of tasks) {
    if (!task.usage) continue;
    total.input += task.usage.input || 0;
    total.output += task.usage.output || 0;
    total.cacheRead += task.usage.cacheRead || 0;
    total.cacheWrite += task.usage.cacheWrite || 0;
    total.cost += task.usage.cost || 0;
    total.contextTokens = Math.max(total.contextTokens, task.usage.contextTokens || 0);
    total.turns += task.usage.turns || 0;
  }
  return total;
}

function aggregateTaskTurnCount(tasks: RunTaskSummary[]): number {
  return tasks.reduce((total, task) => total + (task.turnCount ?? task.usage?.turns ?? 0), 0);
}

function aggregateTaskToolCount(tasks: RunTaskSummary[]): number {
  return tasks.reduce((total, task) => total + (task.toolCount ?? 0), 0);
}

function latestTaskActivityAt(tasks: RunTaskSummary[]): string | undefined {
  const timestamps = tasks
    .map((task) => task.lastActivityAt)
    .filter((value): value is string => Boolean(value))
    .sort((a, b) => Date.parse(b) - Date.parse(a));
  return timestamps[0];
}

export function collectTaskErrors(tasks: RunTaskSummary[]): string[] {
  return tasks.flatMap((task) => task.errorMessage ? [`${task.id} ${task.agent}: ${task.errorMessage}`] : []);
}

export function artifactPathsFromResult(result: SingleResult): string[] {
  return [
    result.artifactResultPath,
    result.artifactTranscriptPath,
    result.artifactStderrPath,
    result.artifactSummaryPath,
    result.outputPath,
    result.worktree?.patchPath,
    result.worktree?.diffstatPath,
    result.worktree?.manifestPath,
  ].filter((value): value is string => Boolean(value));
}

export async function writeRunStatus(
  context: RunStateContext,
  status: RunStatus,
  options: { mirrorArtifactStatus?: boolean } = {},
): Promise<void> {
  await writeJsonAtomic(context.statusPath, status);
  if ((options.mirrorArtifactStatus ?? true) && context.artifactStatusPath) {
    await writeJsonAtomic(context.artifactStatusPath, status);
  }
}

export async function appendRunEvent(context: RunStateContext, event: Omit<RunEvent, "runId" | "timestamp">): Promise<void> {
  const fullEvent: RunEvent = { runId: context.runId, timestamp: new Date().toISOString(), ...event };
  await fs.promises.mkdir(path.dirname(context.eventsPath), { recursive: true, mode: 0o700 });
  await withFileMutationQueue(context.eventsPath, async () => {
    await fs.promises.appendFile(context.eventsPath, `${JSON.stringify(fullEvent)}\n`, { encoding: "utf-8", mode: 0o600 });
  });
}

async function appendTaskOutputLog(filePath: string, line: string): Promise<void> {
  await fs.promises.mkdir(path.dirname(filePath), { recursive: true, mode: 0o700 });
  await withFileMutationQueue(filePath, async () => {
    await fs.promises.appendFile(filePath, `${line}\n`, { encoding: "utf-8", mode: 0o600 });
  });
}

function sanitizeActivityText(value: string | undefined, limit = MAX_ACTIVITY_PREVIEW_CHARS): string | undefined {
  if (!value) return undefined;
  const normalized = value
    .replace(ANSI_ESCAPE_SEQUENCE_RE, "")
    .replace(/\0/g, "")
    .replace(/\r\n/g, "\n")
    .replace(/\r/g, "\n")
    .trim();
  if (!normalized) return undefined;
  return normalized.length > limit ? `${normalized.slice(0, Math.max(0, limit - 1))}…` : normalized;
}

function appendBounded<T>(items: readonly T[], item: T, limit: number): T[] {
  return [...items, item].slice(-limit);
}

function formatLogLine(timestamp: string, label: string, text: string): string {
  return `[${timestamp}] ${label}: ${text.replace(/\n/g, "\\n")}`;
}

function formatUsageLog(activity: ChildActivityUpdate): string | undefined {
  const parts: string[] = [];
  if (activity.usage) {
    parts.push(`turns=${activity.usage.turns}`);
    parts.push(`input=${activity.usage.input}`);
    parts.push(`output=${activity.usage.output}`);
  }
  if (activity.model) parts.push(`model=${activity.model}`);
  return parts.length > 0 ? parts.join(" ") : undefined;
}

function applyToolStarted(task: RunTaskSummary, activity: ChildActivityUpdate, timestamp: string): RunTaskSummary {
  const name = sanitizeActivityText(activity.toolName, 120) ?? "unknown";
  const toolCallId = sanitizeActivityText(activity.toolCallId, 160);
  const pathTarget = sanitizeActivityText(activity.path, 300);
  const preview = sanitizeActivityText(activity.preview);
  return {
    ...task,
    lastActivityAt: timestamp,
    currentTool: name,
    currentToolStartedAt: timestamp,
    currentPath: pathTarget,
    toolCount: task.toolCount! + 1,
    recentTools: appendBounded(task.recentTools!, {
      name,
      ...(toolCallId ? { toolCallId } : {}),
      startedAt: timestamp,
      ...(pathTarget ? { path: pathTarget } : {}),
      status: "running" as const,
      ...(preview ? { preview } : {}),
    }, MAX_RECENT_TOOLS),
  };
}

function findLatestRunningToolIndex(
  tools: RecentToolActivity[],
  predicate: (tool: RecentToolActivity) => boolean,
): number {
  for (let index = tools.length - 1; index >= 0; index--) {
    const tool = tools[index];
    if (tool?.status === "running" && predicate(tool)) return index;
  }
  return -1;
}

function toolFinishMatchIndex(
  tools: RecentToolActivity[],
  activityName: string | undefined,
  toolCallId: string | undefined,
): number {
  if (toolCallId) {
    const idMatch = findLatestRunningToolIndex(tools, (tool) => tool.toolCallId === toolCallId);
    if (idMatch >= 0) return idMatch;

    return findLatestRunningToolIndex(tools, (tool) => !tool.toolCallId && (!activityName || tool.name === activityName));
  }

  return findLatestRunningToolIndex(tools, (tool) => !activityName || tool.name === activityName);
}

function latestRunningTool(tools: RecentToolActivity[]): RecentToolActivity | undefined {
  const index = findLatestRunningToolIndex(tools, () => true);
  return index >= 0 ? tools[index] : undefined;
}

function applyToolFinished(task: RunTaskSummary, activity: ChildActivityUpdate, timestamp: string): {
  task: RunTaskSummary;
  finishedTool: RecentToolActivity;
} {
  const activityName = sanitizeActivityText(activity.toolName, 120);
  const toolCallId = sanitizeActivityText(activity.toolCallId, 160);
  const activityPath = sanitizeActivityText(activity.path, 300);
  const recentTools = [...task.recentTools!];
  const updateIndex = toolFinishMatchIndex(recentTools, activityName, toolCallId);
  const existingTool = updateIndex >= 0 ? recentTools[updateIndex] : undefined;
  const name = activityName ?? existingTool?.name ?? (toolCallId ? undefined : task.currentTool) ?? "unknown";
  const pathTarget = activityPath ?? existingTool?.path ?? (toolCallId ? undefined : task.currentPath);
  const finishedTool: RecentToolActivity = {
    ...(existingTool ?? { name }),
    name,
    ...(toolCallId ? { toolCallId } : {}),
    ...(pathTarget ? { path: pathTarget } : {}),
    finishedAt: timestamp,
    status: "finished" as const,
    ...(activity.isError !== undefined ? { isError: activity.isError } : {}),
  };
  if (updateIndex >= 0) recentTools[updateIndex] = finishedTool;
  else recentTools.push(finishedTool);

  const boundedRecentTools = recentTools.slice(-MAX_RECENT_TOOLS);
  const currentTool = latestRunningTool(boundedRecentTools);
  return {
    task: {
      ...task,
      lastActivityAt: timestamp,
      currentTool: currentTool?.name,
      currentToolStartedAt: currentTool?.startedAt,
      currentPath: currentTool?.path,
      recentTools: boundedRecentTools,
    },
    finishedTool,
  };
}

function activityStatusUpdate(task: RunTaskSummary, activity: ChildActivityUpdate, timestamp: string): {
  task: RunTaskSummary;
  event?: Omit<RunEvent, "runId" | "timestamp">;
  logLine?: string;
} {
  switch (activity.type) {
    case "message": {
      const text = sanitizeActivityText(activity.text ?? activity.preview);
      if (!text) return { task };
      return {
        task: {
          ...task,
          lastActivityAt: timestamp,
          recentOutput: appendBounded(task.recentOutput!, text, MAX_RECENT_OUTPUT),
        },
        event: { type: "child_message", preview: text },
        logLine: formatLogLine(timestamp, "assistant", text),
      };
    }
    case "tool_started": {
      const updated = applyToolStarted(task, activity, timestamp);
      const toolCallId = sanitizeActivityText(activity.toolCallId, 160);
      const target = updated.currentPath ? ` ${updated.currentPath}` : "";
      return {
        task: updated,
        event: {
          type: "child_tool_started",
          toolName: updated.currentTool,
          toolCallId,
          path: updated.currentPath,
          preview: sanitizeActivityText(activity.preview),
        },
        logLine: formatLogLine(timestamp, "tool started", `${updated.currentTool}${target}`),
      };
    }
    case "tool_finished": {
      const { task: updated, finishedTool } = applyToolFinished(task, activity, timestamp);
      return {
        task: updated,
        event: {
          type: "child_tool_finished",
          toolName: finishedTool.name,
          toolCallId: finishedTool.toolCallId,
          path: finishedTool.path,
          isError: activity.isError,
        },
        logLine: formatLogLine(timestamp, "tool finished", finishedTool.name),
      };
    }
    case "usage": {
      const usage = activity.usage ?? task.usage;
      const logText = formatUsageLog(activity);
      return {
        task: {
          ...task,
          ...(usage ? { usage } : {}),
          ...(activity.model ? { model: sanitizeActivityText(activity.model, 160) } : {}),
          turnCount: usage?.turns ?? task.turnCount!,
          lastActivityAt: timestamp,
        },
        ...(logText ? { logLine: formatLogLine(timestamp, "usage", logText) } : {}),
      };
    }
    case "stderr": {
      const preview = sanitizeActivityText(activity.preview ?? activity.text);
      if (!preview) return { task };
      return {
        task: {
          ...task,
          lastActivityAt: timestamp,
          recentOutput: appendBounded(task.recentOutput!, `[stderr] ${preview}`, MAX_RECENT_OUTPUT),
        },
        event: { type: "child_stderr", preview },
        logLine: formatLogLine(timestamp, "stderr", preview),
      };
    }
  }
}

export class RunStateWriter {
  readonly context: RunStateContext;
  private status: RunStatus;
  private mirrorArtifactStatus: boolean;
  private mutationQueue: Promise<void> = Promise.resolve();

  private constructor(options: StartRunStateOptions) {
    this.context = createRunStateContext(options.cwd, options.runId, options.artifactDir);
    this.status = createInitialRunStatus(options);
    this.mirrorArtifactStatus = !options.deferArtifactStatusMirror;
  }

  private async writeStatus(): Promise<void> {
    await writeRunStatus(this.context, this.status, { mirrorArtifactStatus: this.mirrorArtifactStatus });
  }

  static async start(options: StartRunStateOptions): Promise<RunStateWriter> {
    const writer = new RunStateWriter(options);
    await writer.writeStatus();
    try {
      await appendRunEvent(writer.context, { type: "run_started", mode: options.mode, state: "running" });
    } catch (error) {
      writer.status = failStatusAfterStartupEventError(writer.status, error);
      await writer.writeStatus();
      throw error;
    }
    return writer;
  }

  get currentStatus(): RunStatus {
    return this.status;
  }

  private enqueueMutation(work: () => Promise<void>): Promise<void> {
    const next = this.mutationQueue.then(work, work);
    this.mutationQueue = next.catch(() => {});
    return next;
  }

  async enableArtifactStatusMirror(): Promise<void> {
    return this.enqueueMutation(async () => {
      if (this.mirrorArtifactStatus) return;
      await writeRunStatus(this.context, this.status, { mirrorArtifactStatus: true });
      this.mirrorArtifactStatus = true;
    });
  }

  async childStarted(index: number, cwd?: string): Promise<void> {
    const timestamp = new Date().toISOString();
    return this.enqueueMutation(async () => {
      if (isTerminalRunState(this.status.state)) return;
      const task = this.status.tasks[index];
      if (!task) return;
      const tasks = [...this.status.tasks];
      tasks[index] = markTaskStarted(ensureTaskLiveDefaults(task, this.context.runStateDir, index), cwd, new Date(timestamp));
      this.status = updateRunStatus(this.status, { tasks }, new Date(timestamp));
      await this.writeStatus();
      await appendTaskOutputLog(tasks[index].outputLogPath!, formatLogLine(timestamp, "started", `cwd=${tasks[index].cwd ?? this.context.cwd}`));
      await appendRunEvent(this.context, {
        type: "child_started",
        taskId: task.id,
        taskIndex: index,
        agent: task.agent,
        state: "running",
      });
    });
  }

  async childActivity(index: number, activity: ChildActivityUpdate): Promise<void> {
    const timestamp = new Date().toISOString();
    return this.enqueueMutation(async () => {
      if (isTerminalRunState(this.status.state)) return;
      const task = this.status.tasks[index];
      if (!task) return;
      const taskWithDefaults = ensureTaskLiveDefaults(task, this.context.runStateDir, index);
      const update = activityStatusUpdate(taskWithDefaults, activity, timestamp);
      const tasks = [...this.status.tasks];
      tasks[index] = update.task;
      this.status = updateRunStatus(this.status, { tasks }, new Date(timestamp));
      await this.writeStatus();
      if (update.logLine && update.task.outputLogPath) await appendTaskOutputLog(update.task.outputLogPath, update.logLine);
      if (update.event) {
        await appendRunEvent(this.context, {
          ...update.event,
          taskId: task.id,
          taskIndex: index,
          agent: task.agent,
        });
      }
    });
  }

  async artifactWritten(index: number, result: SingleResult): Promise<void> {
    return this.enqueueMutation(async () => {
      if (isTerminalRunState(this.status.state)) return;
      await appendRunEvent(this.context, {
        type: "artifact_written",
        taskId: this.status.tasks[index]?.id,
        taskIndex: index,
        agent: result.agent,
        artifactPaths: artifactPathsFromResult(result),
        outputSummary: result.outputSummary,
      });
    });
  }

  async childFinished(index: number, result: SingleResult): Promise<void> {
    const timestamp = new Date().toISOString();
    return this.enqueueMutation(async () => {
      if (isTerminalRunState(this.status.state)) return;
      const task = this.status.tasks[index];
      if (!task) return;
      const tasks = [...this.status.tasks];
      tasks[index] = markTaskFromResult(ensureTaskLiveDefaults(task, this.context.runStateDir, index), result, new Date(timestamp));
      this.status = updateRunStatus(this.status, { tasks }, new Date(timestamp));
      await this.writeStatus();
      const state = tasks[index].state;
      const exit = tasks[index].exitCode !== undefined ? ` exit=${tasks[index].exitCode}` : "";
      await appendTaskOutputLog(tasks[index].outputLogPath!, formatLogLine(timestamp, "finished", `${state}${exit}`));
      await appendRunEvent(this.context, {
        type: "child_finished",
        taskId: task.id,
        taskIndex: index,
        agent: result.agent,
        state: tasks[index].state,
        message: result.errorMessage,
      });
    });
  }

  async childSkipped(index: number, reason: string): Promise<void> {
    const timestamp = new Date().toISOString();
    return this.enqueueMutation(async () => {
      if (isTerminalRunState(this.status.state)) return;
      const task = this.status.tasks[index];
      if (!task) return;
      const tasks = [...this.status.tasks];
      tasks[index] = markTaskSkipped(ensureTaskLiveDefaults(task, this.context.runStateDir, index), reason, new Date(timestamp));
      this.status = updateRunStatus(this.status, { tasks }, new Date(timestamp));
      await this.writeStatus();
      await appendTaskOutputLog(tasks[index].outputLogPath!, formatLogLine(timestamp, "skipped", reason));
      await appendRunEvent(this.context, {
        type: "child_skipped",
        taskId: task.id,
        taskIndex: index,
        agent: task.agent,
        state: "skipped",
        message: reason,
      });
    });
  }

  async finish(state: Exclude<RunStatusState, "running">, error?: unknown): Promise<void> {
    const timestamp = new Date().toISOString();
    return this.enqueueMutation(async () => {
      if (isTerminalRunState(this.status.state)) return;

      const finalState = finishStateForTasks(state, this.status.tasks);
      const tasks = terminalizeUnresolvedTasks(this.status.tasks, finalState, error);
      const errors = [...collectTaskErrors(tasks)];
      if (error !== undefined) errors.push(errorMessage(error));
      this.status = updateRunStatus(this.status, { state: finalState, tasks, errors: Array.from(new Set(errors)) }, new Date(timestamp));
      await this.writeStatus();
      await appendRunEvent(this.context, {
        type: finalState === "succeeded" ? "run_finished" : finalState === "canceled" ? "run_canceled" : "run_failed",
        state: finalState,
        error: error !== undefined ? errorMessage(error) : undefined,
      });
    });
  }
}

type ParsedRunStatus = Partial<Omit<RunStatus, "errors" | "warnings" | "disabledAgents" | "usage" | "tasks">> & {
  runId?: unknown;
  state?: unknown;
  tasks?: unknown;
  errors?: unknown;
  warnings?: unknown;
  disabledAgents?: unknown;
  usage?: unknown;
};

function assertStatusObject(parsed: unknown): asserts parsed is ParsedRunStatus {
  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error("status must be an object");
  }
}

function normalizeStatusStringArray(value: unknown, fieldName: "errors" | "warnings" | "disabledAgents"): string[] {
  if (value === undefined) return [];
  if (!Array.isArray(value)) throw new Error(`${fieldName} must be an array`);
  return value.map((item) => {
    if (typeof item !== "string") throw new Error(`${fieldName} must contain only strings`);
    return item;
  });
}

function normalizeUsageNumber(value: unknown, fieldName: keyof UsageStats): number {
  if (value === undefined) return 0;
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new Error(`usage.${fieldName} must be a finite number`);
  }
  return value;
}

function normalizeStatusUsage(value: unknown): UsageStats {
  if (value === undefined) return emptyRunUsageStats();
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new Error("usage must be an object");
  const usage = value as Partial<Record<keyof UsageStats, unknown>>;
  return {
    input: normalizeUsageNumber(usage.input, "input"),
    output: normalizeUsageNumber(usage.output, "output"),
    cacheRead: normalizeUsageNumber(usage.cacheRead, "cacheRead"),
    cacheWrite: normalizeUsageNumber(usage.cacheWrite, "cacheWrite"),
    cost: normalizeUsageNumber(usage.cost, "cost"),
    contextTokens: normalizeUsageNumber(usage.contextTokens, "contextTokens"),
    turns: normalizeUsageNumber(usage.turns, "turns"),
  };
}

function normalizeRunStatus(parsed: unknown, runId: string): RunStatus {
  assertStatusObject(parsed);
  if (parsed.runId !== runId || typeof parsed.state !== "string" || !Array.isArray(parsed.tasks)) {
    throw new Error("missing required status fields");
  }
  if (parsed.parentSessionId !== undefined && typeof parsed.parentSessionId !== "string") {
    throw new Error("parentSessionId must be a string");
  }
  const status = parsed as RunStatus;
  const runStateDir = typeof status.runStateDir === "string" ? status.runStateDir : defaultRunStateDir(runId);
  const tasks = withLiveTaskDefaults(status.tasks, runStateDir);
  return {
    ...status,
    runStateDir,
    tasks,
    errors: normalizeStatusStringArray(parsed.errors, "errors"),
    warnings: normalizeStatusStringArray(parsed.warnings, "warnings"),
    disabledAgents: normalizeStatusStringArray(parsed.disabledAgents, "disabledAgents"),
    usage: normalizeStatusUsage(parsed.usage),
    lastActivityAt: status.lastActivityAt ?? latestTaskActivityAt(tasks),
    turnCount: typeof status.turnCount === "number" && Number.isFinite(status.turnCount) ? status.turnCount : aggregateTaskTurnCount(tasks),
    toolCount: typeof status.toolCount === "number" && Number.isFinite(status.toolCount) ? status.toolCount : aggregateTaskToolCount(tasks),
  };
}

export function readRunStatus(runId: string): Promise<RunStatus>;
export function readRunStatus(cwd: string, runId: string): Promise<RunStatus>;
export async function readRunStatus(first: string, second?: string): Promise<RunStatus> {
  const runId = second ?? first;
  const cwd = normalizeCwdScope(second !== undefined ? first : undefined);
  assertValidRunId(runId);
  const statusPath = path.join(defaultRunStateDir(runId), RUN_STATUS_FILENAME);
  let text: string;
  try {
    text = await fs.promises.readFile(statusPath, "utf-8");
  } catch (error: any) {
    if (error?.code === "ENOENT") throw new RunStatusNotFoundError(runId, statusPath);
    throw error;
  }

  let parsed: unknown;
  try {
    parsed = JSON.parse(text);
  } catch (error) {
    if (cwd) throw new RunStatusNotFoundError(runId, statusPath);
    throw new RunStatusCorruptError(runId, statusPath, error);
  }

  if (cwd && !parsedStatusMatchesCwdScope(parsed, cwd)) {
    throw new RunStatusNotFoundError(runId, statusPath);
  }

  try {
    return normalizeRunStatus(parsed, runId);
  } catch (error) {
    throw new RunStatusCorruptError(runId, statusPath, error);
  }
}

export function listRecentRunStatuses(limit?: number): Promise<RunStatus[]>;
export function listRecentRunStatuses(cwd: string, limit?: number): Promise<RunStatus[]>;
export async function listRecentRunStatuses(first?: string | number, second?: number): Promise<RunStatus[]> {
  const result = typeof first === "string" ? await listRecentRuns(first, second) : await listRecentRuns(first);
  return result.statuses;
}

export function listRecentRuns(limit?: number): Promise<RecentRunsResult>;
export function listRecentRuns(cwd: string, limit?: number): Promise<RecentRunsResult>;
export async function listRecentRuns(first: string | number = 10, second?: number): Promise<RecentRunsResult> {
  const root = defaultRunStateRoot();
  const cwd = normalizeCwdScope(typeof first === "string" ? first : undefined);
  const limit = typeof first === "number" ? first : second ?? 10;
  const normalizedLimit = normalizeLimit(limit);
  if (normalizedLimit === 0) return { statuses: [], warnings: [] };

  let entries: fs.Dirent[];
  try {
    entries = await fs.promises.readdir(root, { withFileTypes: true });
  } catch (error: any) {
    if (error?.code === "ENOENT") return { statuses: [], warnings: [] };
    throw error;
  }

  const candidates = await recentRunCandidates(root, entries);
  const statuses: RunStatus[] = [];
  const warnings: string[] = [];
  const candidateLimit = cwd ? MAX_RECENT_RUN_CANDIDATES : recentRunCandidateLimit(normalizedLimit);
  for (const candidate of candidates.slice(0, candidateLimit)) {
    try {
      statuses.push(cwd ? await readRunStatus(cwd, candidate.runId) : await readRunStatus(candidate.runId));
    } catch (error) {
      if (!cwd) warnings.push(formatRecentRunWarning(candidate.runId, error));
      else if (error instanceof RunStatusCorruptError) warnings.push(formatRecentRunWarning(candidate.runId, error));
    }
  }

  return {
    statuses: statuses
      .sort((a, b) => Date.parse(b.updatedAt) - Date.parse(a.updatedAt))
      .slice(0, normalizedLimit),
    warnings,
  };
}

export async function pruneStaleRuns(options: PruneStaleRunsOptions = {}): Promise<PruneStaleRunsResult> {
  const cwd = normalizeCwdScope(options.cwd);
  const root = defaultRunStateRoot();
  const dryRun = options.dryRun ?? true;
  const olderThanDays = normalizeOlderThanDays(options.olderThanDays);
  const includeRunning = options.includeRunning ?? false;
  const shouldPruneWorktrees = options.pruneWorktrees ?? true;
  const now = options.now ?? new Date();
  const cutoffMs = now.getTime() - olderThanDays * DAY_MS;
  const cleanup = options.cleanupWorktree ?? cleanupWorktree;

  let entries: fs.Dirent[];
  try {
    entries = await fs.promises.readdir(root, { withFileTypes: true });
  } catch (error: any) {
    if (error?.code === "ENOENT") {
      return emptyPruneResult({ cwd, root, dryRun, olderThanDays, includeRunning, pruneWorktrees: shouldPruneWorktrees });
    }
    throw error;
  }

  const warnings: string[] = [];
  const runs: PruneRunResult[] = [];
  let scanned = 0;
  let candidateCount = 0;

  for (const entry of entries.sort((a, b) => a.name.localeCompare(b.name))) {
    if (!entry.isDirectory()) continue;
    const runStateDir = path.join(root, entry.name);
    const mtimeMs = await runStateMtimeMs(runStateDir);

    let status: RunStatus | undefined;
    let statusWarning: string | undefined;
    if (cwd) {
      try {
        status = await readRunStatus(cwd, entry.name);
      } catch {
        continue;
      }
    }

    scanned += 1;
    if (mtimeMs > cutoffMs) continue;
    candidateCount += 1;

    if (!cwd) {
      try {
        status = await readRunStatus(entry.name);
      } catch (error) {
        statusWarning = formatRecentRunWarning(entry.name, error);
        warnings.push(statusWarning);
      }
    }

    const updatedMs = status ? statusUpdatedTimeMs(status, mtimeMs) : mtimeMs;
    const ageDays = Math.max(0, (now.getTime() - updatedMs) / DAY_MS);
    if (updatedMs > cutoffMs) continue;

    if (status?.state === "running" && !includeRunning) {
      runs.push({
        runId: entry.name,
        runStateDir,
        state: status.state,
        updatedAt: status.updatedAt,
        ageDays,
        action: "skipped",
        reason: "run is still marked running; pass includeRunning: true to prune stale running runs",
        worktrees: [],
      });
      continue;
    }

    const worktrees = status
      ? await pruneWorktreesForStatus({ status, dryRun, pruneWorktrees: shouldPruneWorktrees, cleanup })
      : [];
    if (worktrees.some((worktree) => worktree.action === "failed")) {
      runs.push({
        runId: entry.name,
        runStateDir,
        state: status?.state,
        updatedAt: status?.updatedAt,
        ageDays,
        action: "failed",
        reason: "worktree cleanup failed; kept run-state so recovery metadata is not lost",
        worktrees,
      });
      continue;
    }

    if (!shouldPruneWorktrees && worktrees.length > 0) {
      runs.push({
        runId: entry.name,
        runStateDir,
        state: status?.state,
        updatedAt: status?.updatedAt,
        ageDays,
        action: "skipped",
        reason: "worktree cleanup skipped because pruneWorktrees=false; kept run-state so recovery metadata is not lost",
        worktrees,
      });
      continue;
    }

    if (dryRun) {
      runs.push({
        runId: entry.name,
        runStateDir,
        state: status?.state,
        updatedAt: status?.updatedAt,
        ageDays,
        action: "would-prune",
        reason: statusWarning,
        worktrees,
      });
      continue;
    }

    try {
      await fs.promises.rm(runStateDir, { recursive: true, force: true });
      runs.push({
        runId: entry.name,
        runStateDir,
        state: status?.state,
        updatedAt: status?.updatedAt,
        ageDays,
        action: "pruned",
        reason: statusWarning,
        worktrees,
      });
    } catch (error) {
      const message = errorMessage(error);
      warnings.push(`Failed to remove stale run ${entry.name}: ${message}`);
      runs.push({
        runId: entry.name,
        runStateDir,
        state: status?.state,
        updatedAt: status?.updatedAt,
        ageDays,
        action: "failed",
        reason: message,
        worktrees,
      });
    }
  }

  return buildPruneResult({
    cwd,
    root,
    dryRun,
    olderThanDays,
    includeRunning,
    pruneWorktrees: shouldPruneWorktrees,
    scanned,
    candidateCount,
    runs,
    warnings,
  });
}

export function formatPruneStaleRunsResult(result: PruneStaleRunsResult): string {
  const summary = result.dryRun
    ? `${result.wouldPruneCount} stale run(s) would be pruned`
    : `${result.prunedCount} stale run(s) pruned`;
  const lines = [
    `Subagent prune ${result.dryRun ? "dry run" : "complete"}: ${summary}`,
    `Root: ${result.root}`,
    `Policy: olderThanDays=${result.olderThanDays} dryRun=${result.dryRun} includeRunning=${result.includeRunning} pruneWorktrees=${result.pruneWorktrees}`,
    `Scanned: ${result.scanned} run director${result.scanned === 1 ? "y" : "ies"}; stale candidates: ${result.candidateCount}`,
  ];

  if (result.runs.length === 0) {
    lines.push("No stale subagent runs found.");
  } else {
    for (const run of result.runs) {
      lines.push(`- ${formatPruneAction(run.action)} ${run.runId} ${run.state ?? "unknown"} age=${formatAgeDays(run.ageDays)} path=${run.runStateDir}`);
      if (run.reason) lines.push(`  reason: ${run.reason}`);
      for (const worktree of run.worktrees) {
        const branch = worktree.branchName ? ` branch=${worktree.branchName}` : "";
        lines.push(`  worktree ${formatWorktreeAction(worktree.action)} ${worktree.taskId}: ${worktree.worktreePath}${branch}`);
        if (worktree.message) lines.push(`    ${worktree.message}`);
      }
    }
  }

  if (result.warnings.length > 0) lines.push("Warnings:", ...result.warnings.map((warning) => `- ${warning}`));
  if (result.dryRun && result.wouldPruneCount > 0) lines.push("Re-run with dryRun: false to remove the listed run-state directories.");
  return lines.join("\n");
}

export function formatRecentRuns(recent: RunStatus[] | RecentRunsResult, _ignoredCwd?: string): string {
  const statuses = Array.isArray(recent) ? recent : recent.statuses;
  const warnings = Array.isArray(recent) ? [] : recent.warnings;
  if (statuses.length === 0 && warnings.length === 0) return `No subagent runs found in ${defaultRunStateRoot()}.`;

  const lines = statuses.length === 0 ? [`No readable subagent runs found in ${defaultRunStateRoot()}.`] : ["Recent subagent runs:"];
  for (const status of statuses) {
    const done = status.tasks.filter((task) => task.state === "succeeded").length;
    const worktreeCleanup = formatWorktreeCleanupSummary(status.tasks);
    lines.push(
      `- ${status.runId} ${status.state} ${status.mode} updated ${status.updatedAt} (${done}/${status.tasks.length} succeeded) artifacts: ${status.artifactDir}${worktreeCleanup}`,
    );
  }
  if (warnings.length > 0) lines.push("Warnings:", ...warnings.map((warning) => `- ${warning}`));
  return lines.join("\n");
}

function formatWorktreeCleanupSummary(tasks: RunTaskSummary[]): string {
  const states = tasks
    .map((task) => task.worktreeCleanupState)
    .filter((state): state is WorktreeCleanupState => Boolean(state));
  if (states.length === 0) return "";

  const counts: Record<WorktreeCleanupState, number> = { removed: 0, kept: 0, failed: 0 };
  for (const state of states) counts[state] += 1;
  const parts = (["removed", "kept", "failed"] as const)
    .filter((state) => counts[state] > 0)
    .map((state) => `${state}=${counts[state]}`);
  return ` worktrees: ${parts.join(", ")}`;
}

function formatTaskWorktreeCleanup(task: RunTaskSummary): string {
  const details = [
    task.worktreeCleanupPolicy ? `policy: ${task.worktreeCleanupPolicy}` : undefined,
    task.worktreeCleanupReason,
  ].filter(Boolean);
  return `cleanup: ${task.worktreeCleanupState}${details.length > 0 ? ` (${details.join("; ")})` : ""}`;
}

function formatRecentRunWarning(runId: string, error: unknown): string {
  return `Skipped recent run ${runId}: ${errorMessage(error)}`;
}

export function recoverableWorktrees(statuses: RunStatus[]): PruneRunWorktreeResult[] {
  return statuses.flatMap((status) =>
    status.tasks.flatMap((task, index) => {
      if (!task.worktreePath || task.worktreeCleanupState === "removed") return [];
      const branchName = task.worktreeBranch;
      return [{
        taskId: `${status.runId}/${task.id ?? `task-${index + 1}`}`,
        worktreePath: task.worktreePath,
        branchName,
        action: "skipped" as const,
        message: task.worktreeCleanupError ?? `cleanup state: ${task.worktreeCleanupState ?? "unknown"}`,
      }];
    }),
  );
}

function formatCurrentActivity(task: RunTaskSummary): string | undefined {
  if (!task.currentTool) return undefined;
  const pathText = task.currentPath ? ` ${task.currentPath}` : "";
  const since = task.currentToolStartedAt ? ` since ${task.currentToolStartedAt}` : "";
  return `${task.currentTool}${pathText}${since}`;
}

function formatRecentTools(tools: RecentToolActivity[] | undefined): string | undefined {
  if (!Array.isArray(tools) || tools.length === 0) return undefined;
  return tools.slice(-3).map((tool) => {
    const pathText = tool.path ? ` ${tool.path}` : "";
    const status = tool.status === "running" ? " (running)" : "";
    return `${tool.name}${pathText}${status}`;
  }).join(", ");
}

function formatRecentOutput(output: string[] | undefined): string | undefined {
  if (!Array.isArray(output) || output.length === 0) return undefined;
  return output.slice(-2).map((line) => line.replace(/\n/g, "\\n")).join(" | ");
}

export function formatRunStatus(status: RunStatus): string {
  const lines = [
    `Subagent run ${status.runId}: ${status.state}`,
    `Mode: ${status.mode}`,
    `Cwd: ${status.cwd}`,
    `Agent scope: ${status.agentScope}`,
    ...(status.disabledAgents.length > 0 ? [`Disabled agents: ${status.disabledAgents.join(", ")}`] : []),
    ...(status.parentSessionId ? [`Parent session: ${status.parentSessionId}`] : []),
    `Started: ${status.startedAt}`,
    `Updated: ${status.updatedAt}`,
    ...(status.endedAt ? [`Ended: ${status.endedAt}`] : []),
    ...(status.lastActivityAt ? [`Last activity: ${status.lastActivityAt}`] : []),
    `Activity: turns=${status.turnCount} tools=${status.toolCount}`,
    `Artifacts: ${status.artifactDir}`,
    `Run state: ${status.runStateDir}`,
    "Tasks:",
  ];

  for (const task of status.tasks) {
    const prefix = task.step ? `step ${task.step} ` : "";
    lines.push(`- ${task.id} ${prefix}[${task.agent}] ${task.state}${task.exitCode !== undefined ? ` exit=${task.exitCode}` : ""}`);
    if (task.errorMessage) lines.push(`  error: ${task.errorMessage}`);
    if (task.skipReason) lines.push(`  skipped: ${task.skipReason}`);
    if (task.outputLogPath) lines.push(`  log: ${task.outputLogPath}`);
    if (task.lastActivityAt) lines.push(`  last activity: ${task.lastActivityAt}`);
    const currentActivity = formatCurrentActivity(task);
    if (currentActivity) lines.push(`  current: ${currentActivity}`);
    if ((task.turnCount ?? 0) > 0 || (task.toolCount ?? 0) > 0) lines.push(`  activity: turns=${task.turnCount ?? 0} tools=${task.toolCount ?? 0}`);
    if (task.model) lines.push(`  model: ${task.model}`);
    const recentTools = formatRecentTools(task.recentTools);
    if (recentTools) lines.push(`  recent tools: ${recentTools}`);
    const recentOutput = formatRecentOutput(task.recentOutput);
    if (recentOutput) lines.push(`  recent output: ${recentOutput}`);
    if (task.artifactResultPath) lines.push(`  result: ${task.artifactResultPath}`);
    if (task.worktreePath) lines.push(`  worktree: ${task.worktreePath}`);
    if (task.worktreeBranch) lines.push(`  branch: ${task.worktreeBranch}`);
    if (task.worktreeBaseCommit) lines.push(`  base: ${task.worktreeBaseCommit}`);
    if (task.worktreePatchPath) lines.push(`  patch: ${task.worktreePatchPath}`);
    if (task.worktreeDiffstatPath) lines.push(`  diffstat: ${task.worktreeDiffstatPath}`);
    if (task.worktreeManifestPath) lines.push(`  worktree metadata: ${task.worktreeManifestPath}`);
    if (task.worktreeNodeModulesLinked) lines.push("  node_modules: linked");
    if (task.worktreeSetupHookPath) {
      const duration = task.worktreeSetupHookDurationMs !== undefined ? ` (${task.worktreeSetupHookDurationMs}ms)` : "";
      lines.push(`  setup hook: ${task.worktreeSetupHookPath}${duration}`);
    }
    if (task.worktreeSyntheticPaths?.length) lines.push(`  synthetic paths: ${task.worktreeSyntheticPaths.join(", ")}`);
    if (task.worktreeCleanupState) lines.push(`  ${formatTaskWorktreeCleanup(task)}`);
    if (task.worktreeCleanupError) lines.push(`  cleanup error: ${task.worktreeCleanupError}`);
  }

  if (status.errors.length > 0) lines.push("Errors:", ...status.errors.map((error) => `- ${error}`));
  if (status.warnings.length > 0) lines.push("Warnings:", ...status.warnings.map((warning) => `- ${warning}`));
  lines.push(
    `Usage: input=${status.usage.input} output=${status.usage.output} cacheRead=${status.usage.cacheRead} cacheWrite=${status.usage.cacheWrite} turns=${status.usage.turns}`,
  );
  return lines.join("\n");
}

interface RecentRunCandidate {
  runId: string;
  mtimeMs: number;
}

interface EmptyPruneResultOptions {
  cwd?: string;
  root: string;
  dryRun: boolean;
  olderThanDays: number;
  includeRunning: boolean;
  pruneWorktrees: boolean;
}

interface BuildPruneResultOptions extends EmptyPruneResultOptions {
  scanned: number;
  candidateCount: number;
  runs: PruneRunResult[];
  warnings: string[];
}

interface PruneWorktreesForStatusOptions {
  status: RunStatus;
  dryRun: boolean;
  pruneWorktrees: boolean;
  cleanup: WorktreeCleanupExecutor;
}

function normalizeLimit(limit: number): number {
  if (!Number.isFinite(limit) || limit <= 0) return 0;
  return Math.floor(limit);
}

function recentRunCandidateLimit(limit: number): number {
  return Math.min(Math.max(limit * 4, limit + 20), Math.max(limit, MAX_RECENT_RUN_CANDIDATES));
}

async function recentRunCandidates(root: string, entries: fs.Dirent[]): Promise<RecentRunCandidate[]> {
  const candidates = await Promise.all(entries.map((entry) => recentRunCandidate(root, entry)));
  return candidates
    .filter((candidate): candidate is RecentRunCandidate => Boolean(candidate))
    .sort((a, b) => b.mtimeMs - a.mtimeMs);
}

async function recentRunCandidate(root: string, entry: fs.Dirent): Promise<RecentRunCandidate | undefined> {
  if (!entry.isDirectory() && !entry.isSymbolicLink()) return undefined;
  const runStateDir = path.join(root, entry.name);
  return { runId: entry.name, mtimeMs: await runStateMtimeMs(runStateDir) };
}

async function runStateMtimeMs(runStateDir: string): Promise<number> {
  try {
    return (await fs.promises.stat(path.join(runStateDir, RUN_STATUS_FILENAME))).mtimeMs;
  } catch {
    try {
      return (await fs.promises.stat(runStateDir)).mtimeMs;
    } catch {
      return 0;
    }
  }
}

function normalizeOlderThanDays(days: number | undefined): number {
  if (typeof days !== "number" || !Number.isFinite(days) || days < 0) return DEFAULT_PRUNE_OLDER_THAN_DAYS;
  return days;
}

function statusUpdatedTimeMs(status: RunStatus, fallbackMs: number): number {
  const parsed = Date.parse(status.endedAt ?? status.updatedAt);
  return Number.isFinite(parsed) ? parsed : fallbackMs;
}

function emptyPruneResult(options: EmptyPruneResultOptions): PruneStaleRunsResult {
  return buildPruneResult({ ...options, scanned: 0, candidateCount: 0, runs: [], warnings: [] });
}

function buildPruneResult(options: BuildPruneResultOptions): PruneStaleRunsResult {
  return {
    ...options,
    prunedCount: options.runs.filter((run) => run.action === "pruned").length,
    wouldPruneCount: options.runs.filter((run) => run.action === "would-prune").length,
    skippedCount: options.runs.filter((run) => run.action === "skipped").length,
    failedCount: options.runs.filter((run) => run.action === "failed").length,
  };
}

async function pruneWorktreesForStatus(options: PruneWorktreesForStatusOptions): Promise<PruneRunWorktreeResult[]> {
  const results: PruneRunWorktreeResult[] = [];
  for (let index = 0; index < options.status.tasks.length; index++) {
    const task = options.status.tasks[index];
    if (!task.worktreePath || task.worktreeCleanupState === "removed") continue;

    const taskId = task.id ?? `task-${index + 1}`;
    if (!task.worktreeBranch) {
      results.push({
        taskId,
        worktreePath: task.worktreePath,
        action: "skipped",
        message: "missing managed branch metadata; remove manually after inspecting status.json",
      });
      continue;
    }

    if (!task.worktreeBranch.startsWith(`subagent/${options.status.runId}/`)) {
      results.push({
        taskId,
        worktreePath: task.worktreePath,
        branchName: task.worktreeBranch,
        action: "skipped",
        message: "branch name is not owned by this run; remove manually if it is safe",
      });
      continue;
    }

    if (!options.pruneWorktrees) {
      results.push({
        taskId,
        worktreePath: task.worktreePath,
        branchName: task.worktreeBranch,
        action: "skipped",
        message: "pruneWorktrees=false",
      });
      continue;
    }

    if (options.dryRun) {
      results.push({
        taskId,
        worktreePath: task.worktreePath,
        branchName: task.worktreeBranch,
        action: "would-clean",
      });
      continue;
    }

    try {
      await options.cleanup({
        worktree: worktreeMetadataFromTask(options.status, task, index),
        artifactDir: options.status.artifactDir,
      });
      results.push({
        taskId,
        worktreePath: task.worktreePath,
        branchName: task.worktreeBranch,
        action: "cleaned",
      });
    } catch (error) {
      results.push({
        taskId,
        worktreePath: task.worktreePath,
        branchName: task.worktreeBranch,
        action: "failed",
        message: errorMessage(error),
      });
    }
  }
  return results;
}

function worktreeMetadataFromTask(status: RunStatus, task: RunTaskSummary, taskIndex: number): WorktreeMetadata {
  return {
    worktreePath: task.worktreePath!,
    branchName: task.worktreeBranch!,
    taskIndex,
    agentName: task.agent,
    runId: status.runId,
    repoRoot: status.cwd,
    baseBranch: "",
    baseCommit: task.worktreeBaseCommit ?? "",
    createdAt: task.startedAt ?? status.startedAt,
  };
}

function formatPruneAction(action: PruneRunAction): string {
  switch (action) {
    case "would-prune":
      return "would prune";
    case "pruned":
      return "pruned";
    case "skipped":
      return "skipped";
    case "failed":
      return "failed";
  }
}

function formatWorktreeAction(action: PruneWorktreeAction): string {
  switch (action) {
    case "would-clean":
      return "would clean";
    case "cleaned":
      return "cleaned";
    case "skipped":
      return "skipped";
    case "failed":
      return "failed";
  }
}

function formatAgeDays(ageDays: number): string {
  return `${ageDays.toFixed(ageDays < 10 ? 1 : 0)}d`;
}

async function writeJsonAtomic(filePath: string, data: unknown): Promise<void> {
  await fs.promises.mkdir(path.dirname(filePath), { recursive: true, mode: 0o700 });
  await withFileMutationQueue(filePath, async () => {
    const tmpPath = path.join(path.dirname(filePath), `.${path.basename(filePath)}.${process.pid}.${Date.now()}.tmp`);
    try {
      await fs.promises.writeFile(tmpPath, `${JSON.stringify(data, null, 2)}\n`, { encoding: "utf-8", mode: 0o600 });
      await fs.promises.rename(tmpPath, filePath);
    } catch (error) {
      await fs.promises.rm(tmpPath, { force: true }).catch(() => {});
      throw error;
    }
  });
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
