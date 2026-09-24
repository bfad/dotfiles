import { execFile } from "node:child_process";
import * as path from "node:path";
import { promisify } from "node:util";
import type { AgentConfig } from "./agents.js";
import type { SubagentTaskItem } from "./types.js";

export const DEFAULT_GIT_ROOT_TIMEOUT_MS = 1_000;

const execFileAsync = promisify(execFile);

const MUTATING_TOOLS = new Set([
  "apply_patch",
  "bash",
  "edit",
  "exec",
  "run",
  "shell",
  "sh",
  "spawn",
  "structured_return",
  "terminal",
  "write",
  "zsh",
]);

const READ_ONLY_TOOLS = new Set([
  "ast_grep",
  "canvas",
  "data_portal_analyze_query_results",
  "data_portal_get_entry_metadata",
  "data_portal_list_data_platform_docs",
  "data_portal_query_bigquery",
  "data_portal_search_data_platform",
  "fd",
  "fetch",
  "find",
  "glob",
  "grep",
  "grokt_bulk_search",
  "grokt_get_file",
  "grokt_search",
  "grokt_stats",
  "history",
  "list",
  "ls",
  "message",
  "metadata",
  "perplexity_fetch",
  "perplexity_search",
  "profile",
  "query",
  "read",
  "rg",
  "ripgrep",
  "search",
  "stats",
  "slack_canvas",
  "slack_history",
  "slack_message",
  "slack_profile",
  "slack_search",
  "slack_thread",
  "thread",
  "wiki_read",
  "wiki_search",
]);

export interface GitRootCommandResult {
  stdout: string;
}

export type GitRootCommand = (cwd: string, timeoutMs: number) => Promise<GitRootCommandResult>;

export interface ParallelSafetyDependencies {
  gitRootCommand?: GitRootCommand;
  gitRootTimeoutMs?: number;
}

export interface TaskMutability {
  writes: boolean;
  reason: string;
  toolNames: string[];
}

export interface ParallelTaskSafetyInfo {
  index: number;
  agent: string;
  cwd: string;
  checkoutRoot?: string;
  writes: boolean;
  reason: string;
}

export interface ParallelSafetyCollision {
  checkoutRoot: string;
  tasks: ParallelTaskSafetyInfo[];
}

export interface ParallelSafetyValidationResult {
  tasks: ParallelTaskSafetyInfo[];
  collisions: ParallelSafetyCollision[];
  warnings: string[];
}

export interface ValidateParallelTasksOptions extends ParallelSafetyDependencies {
  defaultCwd: string;
  tasks: SubagentTaskItem[];
  agents: AgentConfig[];
  allowParallelWrites?: boolean;
}

export class ParallelWriteSafetyError extends Error {
  constructor(
    message: string,
    readonly collisions: ParallelSafetyCollision[],
    readonly tasks: ParallelTaskSafetyInfo[],
  ) {
    super(message);
    this.name = "ParallelWriteSafetyError";
  }
}

async function defaultGitRootCommand(cwd: string, timeoutMs: number): Promise<GitRootCommandResult> {
  const { stdout } = await execFileAsync("git", ["-C", cwd, "rev-parse", "--show-toplevel"], {
    maxBuffer: 16 * 1024,
    timeout: timeoutMs,
  });
  return { stdout: String(stdout) };
}

function normalizeToolName(tool: string): string {
  const normalized = tool.trim().toLowerCase();
  return normalized.includes(".") ? normalized.split(".").pop() || normalized : normalized;
}

function isShellishTool(toolName: string): boolean {
  return /(^|[_-])(bash|shell|sh|zsh|fish|exec|spawn|terminal|command|structured_return)([_-]|$)/.test(toolName);
}

function isMutatingTool(toolName: string): boolean {
  return MUTATING_TOOLS.has(toolName) || isShellishTool(toolName);
}

function isReadOnlyTool(toolName: string): boolean {
  return READ_ONLY_TOOLS.has(toolName);
}

export function resolveEffectiveTaskCwd(defaultCwd: string, taskCwd?: string): string {
  return path.resolve(defaultCwd, taskCwd ?? ".");
}

interface GitCheckoutRootResolution {
  checkoutRoot: string;
  usedFallback: boolean;
}

async function resolveGitCheckoutRootResolution(
  cwd: string,
  deps: ParallelSafetyDependencies = {},
): Promise<GitCheckoutRootResolution> {
  const resolvedCwd = path.resolve(cwd);
  const gitRootCommand = deps.gitRootCommand ?? defaultGitRootCommand;
  const timeoutMs = deps.gitRootTimeoutMs ?? DEFAULT_GIT_ROOT_TIMEOUT_MS;

  try {
    const { stdout } = await gitRootCommand(resolvedCwd, timeoutMs);
    const checkoutRoot = stdout.trim();
    return checkoutRoot
      ? { checkoutRoot: path.resolve(checkoutRoot), usedFallback: false }
      : { checkoutRoot: resolvedCwd, usedFallback: true };
  } catch {
    return { checkoutRoot: resolvedCwd, usedFallback: true };
  }
}

export async function resolveGitCheckoutRoot(
  cwd: string,
  deps: ParallelSafetyDependencies = {},
): Promise<string> {
  const resolution = await resolveGitCheckoutRootResolution(cwd, deps);
  return resolution.checkoutRoot;
}

function isSameOrNestedPath(parentPath: string, childPath: string): boolean {
  const relativePath = path.relative(parentPath, childPath);
  return relativePath === "" || (!relativePath.startsWith("..") && !path.isAbsolute(relativePath));
}

function normalizeFallbackCollisionRoot(defaultCwd: string, checkoutRoot: string): string {
  const resolvedDefaultCwd = path.resolve(defaultCwd);
  return isSameOrNestedPath(resolvedDefaultCwd, checkoutRoot) ? resolvedDefaultCwd : checkoutRoot;
}

function fallbackCollisionRoot(left: string, right: string): string | undefined {
  if (isSameOrNestedPath(left, right)) return left;
  if (isSameOrNestedPath(right, left)) return right;
  return undefined;
}

function inferAgentToolMutability(agent: AgentConfig | undefined): TaskMutability {
  if (!agent) {
    return {
      writes: true,
      reason: "agent was not discovered; treating as potentially mutating",
      toolNames: [],
    };
  }

  if (!agent.tools || agent.tools.length === 0) {
    return {
      writes: true,
      reason: "agent has default tools; treating as potentially mutating",
      toolNames: [],
    };
  }

  const excludedToolNames = new Set((agent.excludeTools ?? []).map(normalizeToolName).filter(Boolean));
  const toolNames = agent.tools
    .map(normalizeToolName)
    .filter((toolName) => toolName && !excludedToolNames.has(toolName));
  const mutatingTools = toolNames.filter(isMutatingTool);
  if (mutatingTools.length > 0) {
    return {
      writes: true,
      reason: `agent has mutating tool(s): ${mutatingTools.join(", ")}`,
      toolNames: mutatingTools,
    };
  }

  const unknownTools = toolNames.filter((tool) => !isReadOnlyTool(tool));
  if (unknownTools.length > 0) {
    return {
      writes: true,
      reason: `agent has unknown tool(s): ${unknownTools.join(", ")}`,
      toolNames: unknownTools,
    };
  }

  return {
    writes: false,
    reason: "agent tools are read/search-only",
    toolNames,
  };
}

export function inferTaskMutability(
  agent: AgentConfig | undefined,
  writesOverride?: boolean,
  allowParallelWriteDowngrade = false,
): TaskMutability {
  if (writesOverride === true) {
    return {
      writes: true,
      reason: "writes override set to true",
      toolNames: [],
    };
  }

  const inferred = inferAgentToolMutability(agent);
  if (writesOverride !== false) return inferred;

  if (!inferred.writes) {
    return {
      writes: false,
      reason: `writes override set to false; ${inferred.reason}`,
      toolNames: inferred.toolNames,
    };
  }

  if (allowParallelWriteDowngrade) {
    return {
      writes: false,
      reason: `writes override set to false with allowParallelWrites; ${inferred.reason}`,
      toolNames: inferred.toolNames,
    };
  }

  return {
    ...inferred,
    reason: `writes:false ignored because ${inferred.reason}`,
  };
}

export function formatParallelWriteCollisionMessage(collisions: ParallelSafetyCollision[]): string {
  const roots = collisions
    .map((collision) => {
      const agents = collision.tasks
        .map((task) => `${task.agent}#${task.index + 1} (${task.cwd})`)
        .join(", ");
      return `- ${collision.checkoutRoot}: ${agents}`;
    })
    .join("\n");

  return [
    "Unsafe parallel subagent writes: multiple mutating workers would run in the same git checkout.",
    roots,
    "Use distinct cwd values that point at separate worktrees/checkouts, use declared read/search-only agents for non-mutating work (writes:false cannot downgrade default/unknown/mutating agents), or pass allowParallelWrites: true to opt in.",
  ].join("\n");
}

export function formatParallelWriteWarning(collisions: ParallelSafetyCollision[]): string {
  return `Warning: allowParallelWrites bypassed the parallel write guard.\n${formatParallelWriteCollisionMessage(collisions)}`;
}

interface MutatingTaskGroup {
  checkoutRoot: string;
  tasks: ParallelTaskSafetyInfo[];
  usedFallback: boolean;
}

function addMutatingTaskGroup(
  groups: MutatingTaskGroup[],
  checkoutRoot: string,
  taskInfo: ParallelTaskSafetyInfo,
  usedFallback: boolean,
): void {
  const exactGroup = groups.find((group) => group.checkoutRoot === checkoutRoot);
  if (exactGroup) {
    exactGroup.tasks.push(taskInfo);
    exactGroup.usedFallback = exactGroup.usedFallback || usedFallback;
    return;
  }

  if (usedFallback) {
    const nestedFallbackGroup = groups.find((group) => {
      return group.usedFallback && fallbackCollisionRoot(group.checkoutRoot, checkoutRoot);
    });

    if (nestedFallbackGroup) {
      nestedFallbackGroup.checkoutRoot = fallbackCollisionRoot(nestedFallbackGroup.checkoutRoot, checkoutRoot)!;
      nestedFallbackGroup.tasks.push(taskInfo);
      return;
    }
  }

  groups.push({ checkoutRoot, tasks: [taskInfo], usedFallback });
}

export async function validateParallelTasks(options: ValidateParallelTasksOptions): Promise<ParallelSafetyValidationResult> {
  const agentByName = new Map(options.agents.map((agent) => [agent.name, agent]));
  const tasks: ParallelTaskSafetyInfo[] = [];
  const mutatingGroups: MutatingTaskGroup[] = [];

  for (let index = 0; index < options.tasks.length; index++) {
    const task = options.tasks[index];
    const cwd = resolveEffectiveTaskCwd(options.defaultCwd, task.cwd);
    const mutability = inferTaskMutability(agentByName.get(task.agent), task.writes, options.allowParallelWrites ?? false);
    const taskInfo: ParallelTaskSafetyInfo = {
      index,
      agent: task.agent,
      cwd,
      writes: mutability.writes,
      reason: mutability.reason,
    };

    if (mutability.writes) {
      const resolution = await resolveGitCheckoutRootResolution(cwd, options);
      taskInfo.checkoutRoot = resolution.checkoutRoot;
      const collisionRoot = resolution.usedFallback
        ? normalizeFallbackCollisionRoot(options.defaultCwd, resolution.checkoutRoot)
        : resolution.checkoutRoot;
      addMutatingTaskGroup(mutatingGroups, collisionRoot, taskInfo, resolution.usedFallback);
    }

    tasks.push(taskInfo);
  }

  const collisions = mutatingGroups
    .filter((group) => group.tasks.length > 1)
    .map(({ checkoutRoot, tasks: tasksForRoot }) => ({ checkoutRoot, tasks: tasksForRoot }));

  if (collisions.length > 0 && !options.allowParallelWrites) {
    throw new ParallelWriteSafetyError(formatParallelWriteCollisionMessage(collisions), collisions, tasks);
  }

  return {
    tasks,
    collisions,
    warnings: collisions.length > 0 && options.allowParallelWrites ? [formatParallelWriteWarning(collisions)] : [],
  };
}
