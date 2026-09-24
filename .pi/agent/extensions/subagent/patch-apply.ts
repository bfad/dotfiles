import { execFile } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { promisify } from "node:util";
import { defaultRunStateDir, readRunStatus, type RunStatus, type RunTaskSummary } from "./run-state.js";
import type { GitCommandExecutor, GitCommandResult } from "./worktrees.js";

const execFileAsync = promisify(execFile);

export interface PatchApplyOptions {
  cwd: string;
  runId?: string;
  taskIds?: string[];
  taskIndexes?: number[];
  all?: boolean;
  apply?: boolean;
  threeWay?: boolean;
  gitCommand?: GitCommandExecutor;
  now?: Date;
}

export interface SelectedPatchTask {
  taskId: string;
  taskIndex: number;
  agent: string;
  patchPath: string;
}

export interface SkippedPatchTask extends SelectedPatchTask {
  reason: "no_changes";
}

export interface PatchApplyCommandAudit {
  args: string[];
  cwd: string;
  ok: boolean;
  stdout: string;
  stderr: string;
  error?: string;
}

export type PatchApplyState = "checked" | "applied" | "applied_but_audit_failed" | "failed" | "no_changes";

export interface PatchApplyResult {
  runId: string;
  cwd: string;
  apply: boolean;
  threeWay: boolean;
  state: PatchApplyState;
  selected: SelectedPatchTask[];
  skipped: SkippedPatchTask[];
  commands: PatchApplyCommandAudit[];
  auditPath: string;
}

interface PatchApplyAudit {
  runId: string;
  cwd: string;
  timestamp: string;
  selector: {
    taskIds?: string[];
    taskIndexes?: number[];
    all?: boolean;
  };
  apply: boolean;
  threeWay: boolean;
  state: PatchApplyState;
  selected: SelectedPatchTask[];
  skipped: SkippedPatchTask[];
  commands: PatchApplyCommandAudit[];
  status: "succeeded" | "failed";
  error?: string;
}

export class PatchApplyError extends Error {
  constructor(
    message: string,
    readonly code: string,
    readonly details?: Record<string, unknown>,
    readonly auditPath?: string,
  ) {
    super(auditPath ? `${message}\nAudit: ${auditPath}` : message);
    this.name = "PatchApplyError";
  }
}

/* v8 ignore next 10 -- production git adapter; unit tests inject gitCommand for deterministic orchestration */
async function defaultGitCommand(
  args: string[],
  options?: { cwd?: string; env?: Record<string, string> },
): Promise<GitCommandResult> {
  const { stdout, stderr } = await execFileAsync("git", args, {
    cwd: options?.cwd,
    env: { ...process.env, ...options?.env },
    maxBuffer: 10 * 1024 * 1024,
  });
  return { stdout: String(stdout), stderr: String(stderr) };
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function selectorCount(options: PatchApplyOptions): number {
  return [
    (options.taskIds?.length ?? 0) > 0,
    (options.taskIndexes?.length ?? 0) > 0,
    options.all === true,
  ].filter(Boolean).length;
}

function requireRunId(runId: string | undefined): string {
  const trimmed = runId?.trim();
  if (!trimmed) {
    throw new PatchApplyError('action: "apply" requires runId.', "MISSING_RUN_ID");
  }
  return trimmed;
}

function ensureExactlyOneSelector(options: PatchApplyOptions): void {
  if (selectorCount(options) !== 1) {
    throw new PatchApplyError(
      'action: "apply" requires exactly one selector: taskIds, taskIndexes, or all: true.',
      "INVALID_SELECTOR",
    );
  }
}

function assertUnique(values: Array<string | number>, selectorName: string): void {
  const seen = new Set<string | number>();
  for (const value of values) {
    if (seen.has(value)) {
      throw new PatchApplyError(`Duplicate ${selectorName} selector: ${String(value)}`, "DUPLICATE_SELECTOR");
    }
    seen.add(value);
  }
}

function selectTasks(status: RunStatus, options: PatchApplyOptions): Array<{ task: RunTaskSummary; index: number }> {
  ensureExactlyOneSelector(options);

  if (options.all === true) {
    const selected = status.tasks
      .map((task, index) => ({ task, index }))
      .filter(({ task }) => Boolean(task.worktreePatchPath));
    if (selected.length === 0) {
      throw new PatchApplyError(
        `Run ${status.runId} has no tasks with captured worktree patches.`,
        "NO_PATCH_TASKS",
      );
    }
    return selected;
  }

  if (options.taskIds && options.taskIds.length > 0) {
    assertUnique(options.taskIds, "taskIds");
    return options.taskIds.map((taskId) => {
      const index = status.tasks.findIndex((task) => task.id === taskId);
      if (index < 0) {
        throw new PatchApplyError(`Run ${status.runId} has no task with id ${taskId}.`, "TASK_NOT_FOUND");
      }
      return { task: status.tasks[index], index };
    });
  }

  const taskIndexes = options.taskIndexes!;
  assertUnique(taskIndexes, "taskIndexes");
  return taskIndexes.map((taskIndex) => {
    if (!Number.isInteger(taskIndex) || taskIndex < 0) {
      throw new PatchApplyError(
        `Invalid task index ${String(taskIndex)}. taskIndexes are zero-based non-negative integers.`,
        "INVALID_TASK_INDEX",
      );
    }
    const task = status.tasks[taskIndex];
    if (!task) {
      throw new PatchApplyError(`Run ${status.runId} has no task at index ${taskIndex}.`, "TASK_NOT_FOUND");
    }
    return { task, index: taskIndex };
  });
}

function selectedPatchTask(task: RunTaskSummary, taskIndex: number, patchPath: string): SelectedPatchTask {
  return {
    taskId: task.id,
    taskIndex,
    agent: task.agent,
    patchPath,
  };
}

async function realExistingDirectory(dirPath: string): Promise<string | undefined> {
  try {
    const stat = await fs.promises.stat(dirPath);
    if (!stat.isDirectory()) return undefined;
    return await fs.promises.realpath(dirPath);
  } catch {
    return undefined;
  }
}

function isPathInside(root: string, filePath: string): boolean {
  const relative = path.relative(root, filePath);
  return relative === "" || (!relative.startsWith("..") && !path.isAbsolute(relative));
}

async function validatePatchPath(patchPath: string, allowedRoots: string[]): Promise<string> {
  const resolvedPatchPath = path.resolve(patchPath);
  let realPatchPath: string;
  let stat: fs.Stats;
  try {
    realPatchPath = await fs.promises.realpath(resolvedPatchPath);
    stat = await fs.promises.stat(realPatchPath);
  } catch (error) {
    throw new PatchApplyError(
      `Patch path is not readable: ${resolvedPatchPath} (${errorMessage(error)})`,
      "PATCH_NOT_READABLE",
      { patchPath: resolvedPatchPath },
    );
  }

  if (!stat.isFile()) {
    throw new PatchApplyError(`Patch path is not a file: ${resolvedPatchPath}`, "PATCH_NOT_FILE", { patchPath: resolvedPatchPath });
  }

  if (!allowedRoots.some((root) => isPathInside(root, realPatchPath))) {
    throw new PatchApplyError(
      `Patch path must stay under this run's artifact or run-state directory: ${resolvedPatchPath}`,
      "PATCH_OUTSIDE_RUN_ROOTS",
      { patchPath: resolvedPatchPath, allowedRoots },
    );
  }

  return realPatchPath;
}

function sameResolvedPath(left: string, right: string): boolean {
  return path.resolve(left) === path.resolve(right);
}

async function allowedPatchRoots(cwd: string, status: RunStatus): Promise<string[]> {
  const targetRoot = path.resolve(cwd);
  const realTargetRoot = await fs.promises.realpath(targetRoot);
  const canonicalRunStateDir = path.resolve(defaultRunStateDir(status.runId));
  const roots = [
    { root: canonicalRunStateDir, allowOutsideTarget: true },
    {
      root: path.resolve(status.artifactDir),
      allowOutsideTarget: sameResolvedPath(status.artifactDir, canonicalRunStateDir),
    },
  ];
  const realRoots: string[] = [];

  for (const { root, allowOutsideTarget } of roots) {
    if (!isPathInside(targetRoot, root) && !allowOutsideTarget) {
      throw new PatchApplyError(
        `Run artifact root must stay under target cwd unless it is the canonical global run directory: ${root}`,
        "RUN_ROOT_OUTSIDE_CWD",
        { cwd: targetRoot, root, canonicalRunStateDir },
      );
    }
    const realRoot = await realExistingDirectory(root);
    if (!realRoot) continue;
    if (!isPathInside(realTargetRoot, realRoot) && !allowOutsideTarget) {
      throw new PatchApplyError(
        `Run artifact root realpath must stay under target cwd unless it is the canonical global run directory: ${root}`,
        "RUN_ROOT_OUTSIDE_CWD",
        { cwd: targetRoot, root, realRoot, canonicalRunStateDir },
      );
    }
    if (!realRoots.includes(realRoot)) realRoots.push(realRoot);
  }

  /* v8 ignore next -- readRunStatus requires the run-state directory, so selected runs always have at least one root */
  if (realRoots.length === 0) {
    throw new PatchApplyError(
      `Run ${status.runId} has no readable artifact or run-state directories.`,
      "RUN_ROOTS_NOT_READABLE",
    );
  }

  return realRoots;
}

async function patchHasChanges(patchPath: string): Promise<boolean> {
  try {
    return (await fs.promises.readFile(patchPath, "utf-8")).trim().length > 0;
  } catch (error) {
    throw new PatchApplyError(
      `Patch path is not readable: ${patchPath} (${errorMessage(error)})`,
      "PATCH_NOT_READABLE",
      { patchPath },
    );
  }
}

async function selectedPatches(
  status: RunStatus,
  options: PatchApplyOptions,
  roots: string[],
): Promise<{ selected: SelectedPatchTask[]; skipped: SkippedPatchTask[] }> {
  const selected = selectTasks(status, options);
  const patches: SelectedPatchTask[] = [];
  const skipped: SkippedPatchTask[] = [];

  for (const { task, index } of selected) {
    if (!task.worktreePatchPath) {
      throw new PatchApplyError(
        `Selected task ${task.id} has no captured worktree patch path.`,
        "TASK_WITHOUT_PATCH",
        { taskId: task.id, taskIndex: index },
      );
    }
    const patchTask = selectedPatchTask(task, index, await validatePatchPath(task.worktreePatchPath, roots));
    if (await patchHasChanges(patchTask.patchPath)) {
      patches.push(patchTask);
    } else {
      skipped.push({ ...patchTask, reason: "no_changes" });
    }
  }

  return { selected: patches, skipped };
}

function gitStatusPath(line: string): string | undefined {
  if (!line.startsWith("?? ")) return undefined;
  return line.slice(3).trim().replace(/\\/g, "/");
}

function isRelativePathInside(root: string, filePath: string): boolean {
  return filePath === root || filePath.startsWith(`${root}/`);
}

function compactPaths(paths: Array<string | undefined>): string[] {
  return Array.from(new Set(paths.filter((value): value is string => Boolean(value))));
}

function cleanStatusIgnoredArtifactPaths(status: RunStatus): string[] {
  return compactPaths([
    path.join(status.artifactDir, "status.json"),
    ...status.tasks.flatMap((task) => [
      task.artifactResultPath,
      task.artifactTranscriptPath,
      task.artifactStderrPath,
      task.artifactSummaryPath,
      task.outputPath,
      task.worktreePatchPath,
      task.worktreeDiffstatPath,
      task.worktreeManifestPath,
    ]),
  ]);
}

function safeRealPathSync(candidate: string): string | undefined {
  try {
    return fs.realpathSync(candidate);
  } catch {
    return undefined;
  }
}

function relativeStatusPath(root: string, candidate: string): string | undefined {
  const relative = path.relative(root, candidate).split(path.sep).join("/");
  if (!relative || relative.startsWith("..") || path.isAbsolute(relative)) return undefined;
  return relative;
}

function ignoredStatusPaths(cwd: string, paths: string[]): Set<string> {
  const targetRoot = path.resolve(cwd);
  const roots = compactPaths([targetRoot, safeRealPathSync(targetRoot)]);
  const ignored = new Set<string>();

  for (const candidate of paths) {
    const candidates = compactPaths([path.resolve(candidate), safeRealPathSync(candidate)]);
    for (const root of roots) {
      for (const resolvedCandidate of candidates) {
        const relative = relativeStatusPath(root, resolvedCandidate);
        if (relative) ignored.add(relative);
      }
    }
  }

  return ignored;
}

function isUntrackedPiArtifactPath(filePath: string): boolean {
  return isRelativePathInside(".pi", filePath) || filePath.includes("/.pi/");
}

function dirtyStatusLines(statusOutput: string, ignoredPaths: Set<string>): string[] {
  return statusOutput
    .split("\n")
    .map((line) => line.trimEnd())
    .filter(Boolean)
    .filter((line) => {
      const statusPath = gitStatusPath(line);
      if (!statusPath) return true;
      if (isUntrackedPiArtifactPath(statusPath)) return false;
      return !ignoredPaths.has(statusPath);
    });
}

async function runGitAuditCommand(
  gitCommand: GitCommandExecutor,
  args: string[],
  cwd: string,
): Promise<PatchApplyCommandAudit> {
  try {
    const result = await gitCommand(args, { cwd });
    return { args, cwd, ok: true, stdout: result.stdout, stderr: result.stderr };
  } catch (error: any) {
    return {
      args,
      cwd,
      ok: false,
      stdout: typeof error?.stdout === "string" ? error.stdout : "",
      stderr: typeof error?.stderr === "string" ? error.stderr : "",
      error: errorMessage(error),
    };
  }
}

async function verifyCleanTarget(
  gitCommand: GitCommandExecutor,
  cwd: string,
  commands: PatchApplyCommandAudit[],
  ignoredArtifactPaths: string[],
): Promise<void> {
  const status = await runGitAuditCommand(gitCommand, ["status", "--porcelain", "--untracked-files=all"], cwd);
  commands.push(status);
  if (!status.ok) {
    throw new PatchApplyError(`Could not inspect target git status: ${status.error}`, "GIT_STATUS_FAILED");
  }

  const dirtyLines = dirtyStatusLines(status.stdout, ignoredStatusPaths(cwd, ignoredArtifactPaths));
  if (dirtyLines.length > 0) {
    throw new PatchApplyError(
      "Target checkout has uncommitted changes. Commit or stash changes before checking or applying worktree patches.",
      "DIRTY_TARGET",
      { status: dirtyLines.join("\n") },
    );
  }
}

function auditPathFor(cwd: string, runId: string, timestamp: string): string {
  const auditDir = path.join(path.resolve(cwd), ".pi", "subagent-runs", runId);
  const auditTimestamp = timestamp.replace(/[-:]/g, "").replace(/\.(\d{3})Z$/, "$1Z");
  return path.join(auditDir, `apply-${auditTimestamp}.json`);
}

async function writeAudit(runId: string, audit: PatchApplyAudit): Promise<string> {
  const auditPath = auditPathFor(audit.cwd, runId, audit.timestamp);
  await fs.promises.mkdir(path.dirname(auditPath), { recursive: true, mode: 0o700 });
  await fs.promises.writeFile(auditPath, `${JSON.stringify(audit, null, 2)}\n`, { encoding: "utf-8", mode: 0o600 });
  return auditPath;
}

function patchApplyActionDetails(
  audit: PatchApplyAudit,
  state: PatchApplyState,
  auditPath: string,
  error: { message: string; code: string; details?: Record<string, unknown>; auditError?: string },
): Record<string, unknown> {
  return {
    action: "apply",
    state,
    runId: audit.runId,
    cwd: audit.cwd,
    apply: audit.apply,
    threeWay: audit.threeWay,
    selected: audit.selected,
    skipped: audit.skipped,
    commands: audit.commands,
    auditPath,
    audit,
    error,
  };
}

async function failWithAudit(
  runId: string,
  audit: PatchApplyAudit,
  error: unknown,
  code = "PATCH_APPLY_FAILED",
): Promise<never> {
  /* v8 ignore next -- defensive fallback for unexpected non-PatchApplyError failures inside the guarded apply block */
  const errorCode = error instanceof PatchApplyError ? error.code : code;
  /* v8 ignore next -- defensive fallback for unexpected non-PatchApplyError failures inside the guarded apply block */
  const errorDetails = error instanceof PatchApplyError ? error.details : undefined;
  audit.status = "failed";
  audit.state = "failed";
  audit.error = errorMessage(error);
  const intendedAuditPath = auditPathFor(audit.cwd, runId, audit.timestamp);
  let auditPath = intendedAuditPath;
  try {
    auditPath = await writeAudit(runId, audit);
  } catch (auditError) {
    throw new PatchApplyError(
      `${audit.error}\nAudit write failed: ${errorMessage(auditError)}`,
      errorCode,
      patchApplyActionDetails(audit, "failed", intendedAuditPath, {
        message: audit.error,
        code: errorCode,
        details: errorDetails,
        auditError: errorMessage(auditError),
      }),
      intendedAuditPath,
    );
  }

  throw new PatchApplyError(
    audit.error,
    errorCode,
    patchApplyActionDetails(audit, "failed", auditPath, {
      message: audit.error,
      code: errorCode,
      details: errorDetails,
    }),
    auditPath,
  );
}

export async function applyWorktreePatches(options: PatchApplyOptions): Promise<PatchApplyResult> {
  const runId = requireRunId(options.runId);
  ensureExactlyOneSelector(options);

  const cwd = path.resolve(options.cwd);
  const status = await readRunStatus(cwd, runId);
  const patchRoots = await allowedPatchRoots(cwd, status);
  const cleanStatusIgnoredPaths = cleanStatusIgnoredArtifactPaths(status);
  const { selected, skipped } = await selectedPatches(status, options, patchRoots);
  /* v8 ignore next -- production git adapter; tests inject gitCommand */
  const gitCommand = options.gitCommand ?? defaultGitCommand;
  const commands: PatchApplyCommandAudit[] = [];
  const apply = options.apply ?? false;
  const threeWay = options.threeWay ?? false;
  const audit: PatchApplyAudit = {
    runId,
    cwd,
    timestamp: (options.now ?? new Date()).toISOString(),
    selector: {
      ...(options.taskIds ? { taskIds: options.taskIds } : {}),
      ...(options.taskIndexes ? { taskIndexes: options.taskIndexes } : {}),
      ...(options.all !== undefined ? { all: options.all } : {}),
    },
    apply,
    threeWay,
    state: "checked",
    selected,
    skipped,
    commands,
    status: "succeeded",
  };

  try {
    const patchPaths = selected.map((task) => task.patchPath);
    if (patchPaths.length > 0) {
      await verifyCleanTarget(gitCommand, cwd, commands, cleanStatusIgnoredPaths);

      const checkArgs = ["apply", "--check", ...(threeWay ? ["--3way"] : []), "--", ...patchPaths];
      const check = await runGitAuditCommand(gitCommand, checkArgs, cwd);
      commands.push(check);
      if (!check.ok) {
        throw new PatchApplyError(`git apply --check failed: ${check.error}`, "GIT_APPLY_CHECK_FAILED");
      }

      if (apply) {
        await verifyCleanTarget(gitCommand, cwd, commands, cleanStatusIgnoredPaths);
        const applyArgs = ["apply", ...(threeWay ? ["--3way"] : []), "--", ...patchPaths];
        const applyResult = await runGitAuditCommand(gitCommand, applyArgs, cwd);
        commands.push(applyResult);
        if (!applyResult.ok) {
          throw new PatchApplyError(`git apply failed: ${applyResult.error}`, "GIT_APPLY_FAILED");
        }
      }
    }
  } catch (error) {
    return failWithAudit(runId, audit, error);
  }

  const state: PatchApplyState = selected.length === 0 ? "no_changes" : apply ? "applied" : "checked";
  audit.state = state;
  const intendedAuditPath = auditPathFor(cwd, runId, audit.timestamp);
  try {
    const auditPath = await writeAudit(runId, audit);
    return { runId, cwd, apply, threeWay, state, selected, skipped, commands, auditPath };
  } catch (error) {
    const partialState: PatchApplyState = state === "applied" ? "applied_but_audit_failed" : "failed";
    const code = state === "applied" ? "AUDIT_WRITE_FAILED_AFTER_APPLY" : "AUDIT_WRITE_FAILED";
    const message = state === "applied"
      ? `Patches were applied, but writing the audit failed: ${errorMessage(error)}`
      : `Patch apply audit write failed: ${errorMessage(error)}`;
    throw new PatchApplyError(
      message,
      code,
      patchApplyActionDetails(audit, partialState, intendedAuditPath, {
        message: errorMessage(error),
        code,
      }),
      intendedAuditPath,
    );
  }
}

export function formatPatchApplyResult(result: PatchApplyResult): string {
  const action = result.state === "no_changes" ? "had no changes" : result.apply ? "applied" : "checked";
  const skippedSuffix = result.skipped.length > 0 ? `, ${result.skipped.length} skipped` : "";
  const lines = [
    `Subagent worktree patches ${action} for run ${result.runId}: ${result.selected.length}/${result.selected.length} passed${skippedSuffix}`,
    `State: ${result.state}`,
    `Mode: ${result.apply ? "apply" : "check-only"}${result.threeWay ? " with --3way" : ""}`,
    `Cwd: ${result.cwd}`,
    "Patches:",
  ];

  if (result.selected.length === 0) lines.push("- (none with changes)");
  for (const task of result.selected) {
    lines.push(`- ${task.taskId} index=${task.taskIndex} [${task.agent}] ${task.patchPath}`);
  }

  if (result.skipped.length > 0) {
    lines.push("Skipped patches:");
    for (const task of result.skipped) {
      lines.push(`- ${task.taskId} index=${task.taskIndex} [${task.agent}] ${task.patchPath} (${task.reason})`);
    }
  }

  lines.push(`Audit: ${result.auditPath}`);
  if (result.state === "no_changes") {
    lines.push("No patch changes were found; nothing was applied.");
  } else if (!result.apply) {
    lines.push('No changes were applied. Re-run with action: "apply" and apply: true to apply these patches.');
  }
  return lines.join("\n");
}
