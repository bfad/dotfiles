import { execFile, spawn } from "node:child_process";
import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { promisify } from "node:util";
import { writeArtifactTextFile } from "./artifacts.js";
import type { SubagentTaskItem } from "./types.js";

const execFileAsync = promisify(execFile);

export type WorktreeSetupMode = "none" | "node-modules";

export interface WorktreeMetadata {
  worktreePath: string;
  worktreeBaseDir?: string;
  branchName: string;
  taskIndex: number;
  agentName: string;
  runId: string;
  repoRoot: string;
  baseBranch: string;
  baseCommit: string;
  createdAt: string;
  nodeModulesLinked?: boolean;
  setupHookPath?: string;
  setupHookDurationMs?: number;
  setupHookStderr?: string;
  syntheticPaths?: string[];
}

export interface PreparedWorktree extends WorktreeMetadata {
  taskCwd: string;
}

export interface WorktreeSetupHookInput {
  runId: string;
  repoRoot: string;
  worktreePath: string;
  taskCwd: string;
  taskIndex: number;
  agentName: string;
  branchName: string;
  baseBranch: string;
  baseCommit: string;
  syntheticPaths: string[];
}

export interface WorktreeSetupHookOutput {
  syntheticPaths?: string[];
}

export interface SetupHookRunOptions {
  hookPath: string;
  input: WorktreeSetupHookInput;
  timeoutMs: number;
  signal?: AbortSignal;
}

export interface SetupHookRunResult {
  stdout: string;
  stderr: string;
  durationMs: number;
}

export type SetupHookRunner = (options: SetupHookRunOptions) => Promise<SetupHookRunResult>;

export interface PrepareWorktreesOptions {
  runId: string;
  repoRoot: string;
  tasks: SubagentTaskItem[];
  artifactDir?: string;
  worktreeBaseDir?: string;
  worktreeSetup?: WorktreeSetupMode;
  worktreeSetupHook?: string;
  worktreeSetupHookTimeoutMs?: number;
  setupHookRunner?: SetupHookRunner;
  gitCommand?: GitCommandExecutor;
  signal?: AbortSignal;
}

export interface CaptureWorktreePatchOptions {
  worktree: WorktreeMetadata;
  artifactDir: string;
  gitCommand?: GitCommandExecutor;
}

export interface CleanupWorktreeOptions {
  worktree: WorktreeMetadata;
  artifactDir?: string;
  gitCommand?: GitCommandExecutor;
}

export interface CleanupWorktreesOptions {
  worktrees: WorktreeMetadata[];
  artifactDir?: string;
  gitCommand?: GitCommandExecutor;
}

export interface GitCommandResult {
  stdout: string;
  stderr: string;
}

export type GitCommandExecutor = (
  args: string[],
  options?: { cwd?: string; env?: Record<string, string> },
) => Promise<GitCommandResult>;

export class WorktreeError extends Error {
  constructor(
    message: string,
    readonly code: string,
    readonly details?: Record<string, any>,
  ) {
    super(message);
    this.name = "WorktreeError";
  }
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function withRollbackCleanupError(setupError: unknown, rollbackCleanupError: unknown): WorktreeError {
  const setupMessage = errorMessage(setupError);
  const rollbackCleanupMessage = errorMessage(rollbackCleanupError);
  if (setupError instanceof WorktreeError) {
    return new WorktreeError(
      `${setupError.message}; rollback cleanup failed: ${rollbackCleanupMessage}`,
      setupError.code,
      {
        ...(setupError.details ?? {}),
        setupError: setupMessage,
        rollbackCleanupError: rollbackCleanupMessage,
      },
    );
  }

  return new WorktreeError(
    `Worktree preparation failed: ${setupMessage}; rollback cleanup failed: ${rollbackCleanupMessage}`,
    "WORKTREE_PREPARE_FAILED",
    { setupError: setupMessage, rollbackCleanupError: rollbackCleanupMessage },
  );
}

const DEFAULT_WORKTREE_BASE = path.join(os.tmpdir(), "pi-subagent-worktrees");
export const DEFAULT_WORKTREE_SETUP_HOOK_TIMEOUT_MS = 120_000;
const MAX_SETUP_HOOK_OUTPUT_BYTES = 1024 * 1024;

/* v8 ignore next 10 -- production git adapter; unit tests inject gitCommand to cover orchestration deterministically */
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

function sanitizeAgentName(name: string): string {
  return name.replace(/[^a-zA-Z0-9_-]/g, "-").replace(/-+/g, "-").replace(/^-|-$/g, "");
}

function computeRepoHash(repoPath: string): string {
  return crypto.createHash("sha256").update(repoPath).digest("hex").slice(0, 16);
}

function isUntrackedPiArtifactLine(line: string): boolean {
  if (!line.startsWith("?? ")) return false;
  const filePath = line.slice(3);
  return filePath === ".pi" || filePath.startsWith(".pi/") || filePath.includes("/.pi/");
}

function dirtyStatusLines(statusOutput: string): string[] {
  return statusOutput
    .split("\n")
    .map((line) => line.trimEnd())
    .filter(Boolean)
    .filter((line) => !isUntrackedPiArtifactLine(line));
}

function untrackedFilesFromPorcelainStatusZ(statusOutput: string): string[] {
  return statusOutput
    .split("\0")
    .filter((entry) => entry.startsWith("?? ") && entry.length > 3)
    .map((entry) => entry.slice(3));
}

async function verifyCleanRepo(repoRoot: string, gitCommand: GitCommandExecutor): Promise<void> {
  const statusResult = await gitCommand(["status", "--porcelain"], { cwd: repoRoot });
  const dirtyLines = dirtyStatusLines(statusResult.stdout);
  if (dirtyLines.length > 0) {
    throw new WorktreeError(
      "Repository has uncommitted changes. Commit or stash changes before creating worktrees.",
      "DIRTY_REPO",
      { repoRoot, status: dirtyLines.join("\n") },
    );
  }
}

async function resolveGitRepoRoot(repoCwd: string, gitCommand: GitCommandExecutor): Promise<string> {
  try {
    const result = await gitCommand(["rev-parse", "--show-toplevel"], { cwd: repoCwd });
    return path.resolve(repoCwd, result.stdout.trim());
  } catch (error) {
    throw new WorktreeError(
      `Not a git repository: ${repoCwd}`,
      "NOT_GIT_REPO",
      { repoRoot: repoCwd, error: String(error) },
    );
  }
}

function resolveWorktreeTaskCwd(worktreePath: string, repoRoot: string, requestedCwd: string): string {
  const relativeCwd = path.relative(repoRoot, requestedCwd);
  if (!relativeCwd) return worktreePath;
  if (relativeCwd.startsWith("..") || path.isAbsolute(relativeCwd)) {
    throw new WorktreeError(
      `Requested cwd ${requestedCwd} is not inside git repository ${repoRoot}`,
      "CWD_OUTSIDE_REPO",
      { repoRoot, requestedCwd },
    );
  }
  return path.join(worktreePath, relativeCwd);
}

async function getCurrentBranch(repoRoot: string, gitCommand: GitCommandExecutor): Promise<string> {
  const result = await gitCommand(["rev-parse", "--abbrev-ref", "HEAD"], { cwd: repoRoot });
  return result.stdout.trim();
}

async function getCurrentCommit(repoRoot: string, gitCommand: GitCommandExecutor): Promise<string> {
  const result = await gitCommand(["rev-parse", "HEAD"], { cwd: repoRoot });
  return result.stdout.trim();
}

function buildWorktreePath(
  worktreeBaseDir: string,
  repoHash: string,
  runId: string,
  taskIndex: number,
  agentName: string,
): string {
  const safeAgentName = sanitizeAgentName(agentName);
  return path.join(worktreeBaseDir, repoHash, runId, `${taskIndex}-${safeAgentName}`);
}

function buildBranchName(runId: string, taskIndex: number, agentName: string): string {
  const safeAgentName = sanitizeAgentName(agentName);
  return `subagent/${runId}/${taskIndex}-${safeAgentName}`;
}

async function createWorktree(
  repoRoot: string,
  worktreePath: string,
  branchName: string,
  baseBranch: string,
  gitCommand: GitCommandExecutor,
): Promise<void> {
  await fs.promises.mkdir(path.dirname(worktreePath), { recursive: true, mode: 0o700 });

  try {
    await gitCommand(["worktree", "add", "-b", branchName, worktreePath, baseBranch], { cwd: repoRoot });
  } catch (error) {
    throw new WorktreeError(
      `Failed to create worktree at ${worktreePath}`,
      "WORKTREE_CREATE_FAILED",
      { repoRoot, worktreePath, branchName, error: String(error) },
    );
  }
}

function isOwnedWorktreePath(worktreePath: string, worktreeBaseDir: string): boolean {
  const normalized = path.resolve(worktreePath);
  const normalizedBase = path.resolve(worktreeBaseDir);
  return normalized.startsWith(normalizedBase + path.sep);
}

async function verifyWorktreeExists(worktreePath: string, repoRoot: string, gitCommand: GitCommandExecutor): Promise<boolean> {
  const listResult = await gitCommand(["worktree", "list", "--porcelain"], { cwd: repoRoot });
  const lines = listResult.stdout.split("\n");

  for (const line of lines) {
    if (line.startsWith("worktree ")) {
      const listedPath = line.substring("worktree ".length).trim();
      if (path.resolve(listedPath) === path.resolve(worktreePath)) {
        return true;
      }
    }
  }

  return false;
}

async function removeWorktree(
  worktreePath: string,
  branchName: string,
  repoRoot: string,
  gitCommand: GitCommandExecutor,
): Promise<void> {
  const exists = await verifyWorktreeExists(worktreePath, repoRoot, gitCommand);

  if (exists) {
    await gitCommand(["worktree", "remove", worktreePath, "--force"], { cwd: repoRoot });
  }

  try {
    await gitCommand(["branch", "-D", branchName], { cwd: repoRoot });
  } catch {
    // Branch might not exist or already deleted
  }
}

async function writeWorktreeManifest(
  worktree: WorktreeMetadata,
  artifactDir: string,
): Promise<string> {
  const manifestPath = path.join(artifactDir, `worktree-${worktree.taskIndex}.json`);
  await writeArtifactTextFile(manifestPath, `${JSON.stringify(worktree, null, 2)}\n`);
  return manifestPath;
}

function pathExistsError(error: unknown): error is NodeJS.ErrnoException {
  return Boolean(error && typeof error === "object" && "code" in error);
}

async function lstatIfExists(filePath: string): Promise<fs.Stats | undefined> {
  try {
    return await fs.promises.lstat(filePath);
  } catch (error) {
    if (pathExistsError(error) && error.code === "ENOENT") return undefined;
    throw error;
  }
}

function normalizeSyntheticPath(rawPath: string): string {
  if (typeof rawPath !== "string" || rawPath.includes("\0")) {
    throw new WorktreeError("Synthetic paths must be non-empty relative path strings.", "INVALID_SYNTHETIC_PATH", { path: rawPath });
  }

  const trimmed = rawPath.trim();
  const withoutDotPrefix = trimmed.replace(/\\/g, "/").replace(/^\.\/+/, "").replace(/\/+$/, "");
  const segments = withoutDotPrefix.split("/");
  const invalid =
    !withoutDotPrefix ||
    path.isAbsolute(trimmed) ||
    segments.some((segment) => segment === "" || segment === "." || segment === "..") ||
    withoutDotPrefix === ".git" ||
    withoutDotPrefix.startsWith(".git/");

  if (invalid) {
    throw new WorktreeError(
      `Invalid synthetic path "${rawPath}". Synthetic paths must be relative paths inside the worktree and must not target .git.`,
      "INVALID_SYNTHETIC_PATH",
      { path: rawPath },
    );
  }

  return segments.join("/");
}

function validateSyntheticPaths(paths: string[] | undefined): string[] {
  const normalized = (paths ?? []).map((item) => normalizeSyntheticPath(item));
  return Array.from(new Set(normalized));
}

function mergeSyntheticPaths(existing: string[] | undefined, next: string[] | undefined): string[] {
  return validateSyntheticPaths([...(existing ?? []), ...(next ?? [])]);
}

function isSyntheticPath(filePath: string, syntheticPaths: string[]): boolean {
  const normalized = filePath.trim().replace(/\\/g, "/").replace(/^\.\/+/, "").replace(/\/+$/, "");
  if (!normalized) return false;
  return syntheticPaths.some((syntheticPath) => normalized === syntheticPath || normalized.startsWith(`${syntheticPath}/`));
}

function diffPathspecArgs(syntheticPaths: string[]): string[] {
  if (syntheticPaths.length === 0) return [];
  return ["--", ".", ...syntheticPaths.map((syntheticPath) => `:(exclude,literal)${syntheticPath}`)];
}

function resolveWorktreeSetupMode(mode: WorktreeSetupMode | undefined): WorktreeSetupMode {
  if (mode === undefined) return "none";
  if (mode === "none" || mode === "node-modules") return mode;
  throw new WorktreeError(`Unsupported worktree setup mode: ${mode}`, "INVALID_SETUP_MODE", { mode });
}

function resolveSetupHookTimeout(timeoutMs: number | undefined): number {
  if (timeoutMs === undefined) return DEFAULT_WORKTREE_SETUP_HOOK_TIMEOUT_MS;
  if (!Number.isFinite(timeoutMs) || timeoutMs <= 0) {
    throw new WorktreeError(
      `worktreeSetupHookTimeoutMs must be a positive number of milliseconds. Received: ${timeoutMs}`,
      "INVALID_SETUP_HOOK_TIMEOUT",
      { timeoutMs },
    );
  }
  return Math.floor(timeoutMs);
}

async function setupHookRealpath(target: string, hookPath: string, kind: "repoRoot" | "hookPath"): Promise<string> {
  try {
    return await fs.promises.realpath(target);
  } catch (error) {
    throw new WorktreeError(
      `Failed to resolve real path for worktreeSetupHook ${hookPath}: ${target}`,
      "SETUP_HOOK_REALPATH_FAILED",
      { hookPath, target, kind, error: String(error) },
    );
  }
}

async function resolveSetupHookPath(hookPath: string, repoRoot: string): Promise<string> {
  const trimmed = hookPath.trim();
  if (!trimmed) {
    throw new WorktreeError("worktreeSetupHook must be a non-empty path.", "INVALID_SETUP_HOOK_PATH", { hookPath });
  }

  const resolved = path.isAbsolute(trimmed) ? path.resolve(trimmed) : path.resolve(repoRoot, trimmed);
  const [realRepoRoot, realHookPath] = await Promise.all([
    setupHookRealpath(repoRoot, hookPath, "repoRoot"),
    setupHookRealpath(resolved, hookPath, "hookPath"),
  ]);
  const relativeToRepo = path.relative(realRepoRoot, realHookPath);
  const outsideRepo = relativeToRepo.startsWith("..") || path.isAbsolute(relativeToRepo);
  if (outsideRepo) {
    throw new WorktreeError(
      `worktreeSetupHook ${hookPath} resolves outside git repository ${repoRoot}`,
      "SETUP_HOOK_OUTSIDE_REPO",
      { hookPath, resolved, realHookPath, repoRoot, realRepoRoot },
    );
  }

  return realHookPath;
}

export function resolveSetupHookSpawnCommand(
  hookPath: string,
  platform: NodeJS.Platform = process.platform,
  comSpec = process.env.ComSpec,
): { command: string; args: string[] } {
  if (platform === "win32" && /\.(?:cmd|bat)$/i.test(hookPath)) {
    return { command: comSpec || "cmd.exe", args: ["/d", "/s", "/c", `"${hookPath}"`] };
  }
  return { command: hookPath, args: [] };
}

async function linkNodeModules(repoRoot: string, worktreePath: string): Promise<{ nodeModulesLinked: true; syntheticPaths: string[] }> {
  const source = path.join(repoRoot, "node_modules");
  const destination = path.join(worktreePath, "node_modules");
  const sourceStat = await lstatIfExists(source);

  if (!sourceStat) {
    throw new WorktreeError(
      `worktreeSetup "node-modules" requested, but ${source} does not exist. Run dependency installation in the base checkout or use worktreeSetup: "none".`,
      "NODE_MODULES_NOT_FOUND",
      { source, destination },
    );
  }

  if (!sourceStat.isDirectory() && !sourceStat.isSymbolicLink()) {
    throw new WorktreeError(
      `worktreeSetup "node-modules" expected ${source} to be a directory or symlink.`,
      "NODE_MODULES_INVALID_SOURCE",
      { source, destination },
    );
  }

  const destinationStat = await lstatIfExists(destination);
  if (destinationStat) {
    throw new WorktreeError(
      `Cannot link node_modules into worktree because ${destination} already exists.`,
      "NODE_MODULES_DEST_EXISTS",
      { source, destination },
    );
  }

  /* v8 ignore next -- platform-specific branch; CI covers the current platform symlink type */
  await fs.promises.symlink(source, destination, process.platform === "win32" ? "junction" : "dir");
  return { nodeModulesLinked: true, syntheticPaths: ["node_modules"] };
}

/* v8 ignore start -- production subprocess adapter; unit tests inject setupHookRunner for deterministic orchestration */
type SetupHookChildProcess = ReturnType<typeof spawn>;

interface SetupHookTerminationState {
  sigkillTimer?: NodeJS.Timeout;
  terminated: boolean;
}

function terminateSetupHookProcess(child: SetupHookChildProcess, termination: SetupHookTerminationState): void {
  if (termination.terminated) return;
  termination.terminated = true;
  child.kill("SIGTERM");
  termination.sigkillTimer = setTimeout(() => child.kill("SIGKILL"), 1_000);
  termination.sigkillTimer.unref?.();
}

async function defaultSetupHookRunner(options: SetupHookRunOptions): Promise<SetupHookRunResult> {
  const startedAt = Date.now();
  if (options.signal?.aborted) {
    throw new WorktreeError(
      `Worktree setup hook ${options.hookPath} was aborted before it started.`,
      "SETUP_HOOK_ABORTED",
      { hookPath: options.hookPath, durationMs: Date.now() - startedAt },
    );
  }

  const hookCommand = resolveSetupHookSpawnCommand(options.hookPath);
  return await new Promise((resolve, reject) => {
    const child = spawn(hookCommand.command, hookCommand.args, {
      cwd: options.input.worktreePath,
      env: {
        ...process.env,
        PI_SUBAGENT_WORKTREE: "1",
        PI_SUBAGENT_RUN_ID: options.input.runId,
        PI_SUBAGENT_REPO_ROOT: options.input.repoRoot,
        PI_SUBAGENT_WORKTREE_PATH: options.input.worktreePath,
        PI_SUBAGENT_TASK_CWD: options.input.taskCwd,
        PI_SUBAGENT_TASK_INDEX: String(options.input.taskIndex),
        PI_SUBAGENT_AGENT_NAME: options.input.agentName,
      },
      stdio: ["pipe", "pipe", "pipe"],
    });

    let stdout = "";
    let stderr = "";
    let settled = false;
    let killedForTimeout = false;
    let killedForOutput = false;
    let killedForAbort = false;
    const termination: SetupHookTerminationState = { terminated: false };

    const killTimer = setTimeout(() => {
      killedForTimeout = true;
      terminateSetupHookProcess(child, termination);
    }, options.timeoutMs);

    const abortSetupHook = () => {
      killedForAbort = true;
      clearTimeout(killTimer);
      terminateSetupHookProcess(child, termination);
    };
    options.signal?.addEventListener("abort", abortSetupHook, { once: true });
    if (options.signal?.aborted) abortSetupHook();

    const settle = (callback: () => void) => {
      if (settled) return;
      settled = true;
      clearTimeout(killTimer);
      if (termination.sigkillTimer) clearTimeout(termination.sigkillTimer);
      options.signal?.removeEventListener("abort", abortSetupHook);
      callback();
    };

    const appendOutput = (target: "stdout" | "stderr", chunk: Buffer | string) => {
      if (killedForOutput) return;
      const text = String(chunk);
      if (target === "stdout") stdout += text;
      else stderr += text;

      if (Buffer.byteLength(stdout) + Buffer.byteLength(stderr) > MAX_SETUP_HOOK_OUTPUT_BYTES) {
        killedForOutput = true;
        clearTimeout(killTimer);
        terminateSetupHookProcess(child, termination);
      }
    };

    child.stdout?.setEncoding("utf-8");
    child.stderr?.setEncoding("utf-8");
    child.stdout?.on("data", (chunk) => appendOutput("stdout", chunk));
    child.stderr?.on("data", (chunk) => appendOutput("stderr", chunk));
    child.on("error", (error) => {
      settle(() => reject(new WorktreeError(
        `Failed to start worktree setup hook ${options.hookPath}: ${error.message}`,
        "SETUP_HOOK_START_FAILED",
        { hookPath: options.hookPath, error: error.message },
      )));
    });
    const handleStdinError = (error: NodeJS.ErrnoException) => {
      if (error.code === "EPIPE" || error.code === "ERR_STREAM_DESTROYED" || error.code === "ERR_STREAM_WRITE_AFTER_END") {
        return;
      }
      settle(() => reject(new WorktreeError(
        `Failed to write input to worktree setup hook ${options.hookPath}: ${error.message}`,
        "SETUP_HOOK_STDIN_FAILED",
        { hookPath: options.hookPath, error: error.message },
      )));
    };

    child.on("close", (code, signal) => {
      const durationMs = Date.now() - startedAt;
      settle(() => {
        if (killedForTimeout) {
          reject(new WorktreeError(
            `Worktree setup hook ${options.hookPath} timed out after ${options.timeoutMs}ms.`,
            "SETUP_HOOK_TIMEOUT",
            { hookPath: options.hookPath, timeoutMs: options.timeoutMs, stdout, stderr, signal, durationMs },
          ));
          return;
        }

        if (killedForOutput) {
          reject(new WorktreeError(
            `Worktree setup hook ${options.hookPath} exceeded ${MAX_SETUP_HOOK_OUTPUT_BYTES} bytes of output.`,
            "SETUP_HOOK_OUTPUT_TOO_LARGE",
            { hookPath: options.hookPath, stdout, stderr, signal, durationMs },
          ));
          return;
        }

        if (killedForAbort) {
          reject(new WorktreeError(
            `Worktree setup hook ${options.hookPath} was aborted.`,
            "SETUP_HOOK_ABORTED",
            { hookPath: options.hookPath, stdout, stderr, signal, durationMs },
          ));
          return;
        }

        if (code !== 0) {
          reject(new WorktreeError(
            `Worktree setup hook ${options.hookPath} failed with exit code ${code}.`,
            "SETUP_HOOK_FAILED",
            { hookPath: options.hookPath, exitCode: code, signal, stdout, stderr, durationMs },
          ));
          return;
        }

        resolve({ stdout, stderr, durationMs });
      });
    });

    if (child.stdin) {
      child.stdin.on("error", handleStdinError);
      try {
        child.stdin.end(`${JSON.stringify(options.input)}\n`);
      } catch (error) {
        handleStdinError(error as NodeJS.ErrnoException);
      }
    }
  });
}
/* v8 ignore stop */

function parseSetupHookOutput(stdout: string, hookPath: string): WorktreeSetupHookOutput {
  const trimmed = stdout.trim();
  if (!trimmed) return {};

  let parsed: unknown;
  try {
    parsed = JSON.parse(trimmed);
  } catch (error) {
    throw new WorktreeError(
      `Worktree setup hook ${hookPath} must write JSON to stdout.`,
      "SETUP_HOOK_INVALID_JSON",
      { hookPath, stdout, error: String(error) },
    );
  }

  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new WorktreeError(
      `Worktree setup hook ${hookPath} must write a JSON object to stdout.`,
      "SETUP_HOOK_INVALID_JSON",
      { hookPath, stdout },
    );
  }

  const output = parsed as { syntheticPaths?: unknown };
  if (output.syntheticPaths !== undefined) {
    if (!Array.isArray(output.syntheticPaths) || output.syntheticPaths.some((item) => typeof item !== "string")) {
      throw new WorktreeError(
        `Worktree setup hook ${hookPath} returned invalid syntheticPaths; expected an array of relative path strings.`,
        "SETUP_HOOK_INVALID_SYNTHETIC_PATHS",
        { hookPath, stdout },
      );
    }
  }

  return { syntheticPaths: output.syntheticPaths as string[] | undefined };
}

async function applyWorktreeSetup(
  worktree: PreparedWorktree,
  options: {
    repoRoot: string;
    setupMode: WorktreeSetupMode;
    setupHookPath?: string;
    setupHookTimeoutMs: number;
    setupHookRunner: SetupHookRunner;
    signal?: AbortSignal;
  },
): Promise<void> {
  if (options.setupMode === "node-modules") {
    const nodeModules = await linkNodeModules(options.repoRoot, worktree.worktreePath);
    worktree.nodeModulesLinked = nodeModules.nodeModulesLinked;
    worktree.syntheticPaths = mergeSyntheticPaths(worktree.syntheticPaths, nodeModules.syntheticPaths);
  }

  if (options.setupHookPath) {
    const hookResult = await options.setupHookRunner({
      hookPath: options.setupHookPath,
      timeoutMs: options.setupHookTimeoutMs,
      signal: options.signal,
      input: {
        runId: worktree.runId,
        repoRoot: worktree.repoRoot,
        worktreePath: worktree.worktreePath,
        taskCwd: worktree.taskCwd,
        taskIndex: worktree.taskIndex,
        agentName: worktree.agentName,
        branchName: worktree.branchName,
        baseBranch: worktree.baseBranch,
        baseCommit: worktree.baseCommit,
        syntheticPaths: worktree.syntheticPaths ?? [],
      },
    });
    const hookOutput = parseSetupHookOutput(hookResult.stdout, options.setupHookPath);
    worktree.setupHookPath = options.setupHookPath;
    worktree.setupHookDurationMs = hookResult.durationMs;
    if (hookResult.stderr) worktree.setupHookStderr = hookResult.stderr;
    worktree.syntheticPaths = mergeSyntheticPaths(worktree.syntheticPaths, hookOutput.syntheticPaths);
  }

  if (worktree.syntheticPaths?.length === 0) delete worktree.syntheticPaths;
}

export async function prepareParallelWorktrees(
  options: PrepareWorktreesOptions,
): Promise<PreparedWorktree[]> {
  /* v8 ignore next -- production default path; tests inject gitCommand for deterministic orchestration */
  const gitCommand = options.gitCommand ?? defaultGitCommand;
  const requestedCwd = path.resolve(options.repoRoot);
  if (!options.tasks.some((task) => task.writes === true)) return [];

  const setupMode = resolveWorktreeSetupMode(options.worktreeSetup);
  const setupHookTimeoutMs = resolveSetupHookTimeout(options.worktreeSetupHookTimeoutMs);
  const repoRoot = await resolveGitRepoRoot(requestedCwd, gitCommand);
  const setupHookPath = options.worktreeSetupHook ? await resolveSetupHookPath(options.worktreeSetupHook, repoRoot) : undefined;
  const setupHookRunner = options.setupHookRunner ?? defaultSetupHookRunner;
  await verifyCleanRepo(repoRoot, gitCommand);

  const baseBranch = await getCurrentBranch(repoRoot, gitCommand);
  const baseCommit = await getCurrentCommit(repoRoot, gitCommand);
  const repoHash = computeRepoHash(repoRoot);
  const worktreeBaseDir = options.worktreeBaseDir ?? DEFAULT_WORKTREE_BASE;

  const preparedWorktrees: PreparedWorktree[] = [];

  try {
    for (let taskIndex = 0; taskIndex < options.tasks.length; taskIndex++) {
      const task = options.tasks[taskIndex];

      // Only create worktrees for mutating tasks
      if (task.writes !== true) {
        continue;
      }

      // Reject explicit cwd override for mutating tasks
      if (task.cwd) {
        throw new WorktreeError(
          `Task ${taskIndex} (${task.agent}) has both writes: true and explicit cwd. Managed worktrees cannot use custom cwd.`,
          "CWD_OVERRIDE_REJECTED",
          { taskIndex, agent: task.agent, cwd: task.cwd },
        );
      }

      const branchName = buildBranchName(options.runId, taskIndex, task.agent);
      const worktreePath = buildWorktreePath(worktreeBaseDir, repoHash, options.runId, taskIndex, task.agent);
      const taskCwd = resolveWorktreeTaskCwd(worktreePath, repoRoot, requestedCwd);

      await createWorktree(repoRoot, worktreePath, branchName, baseBranch, gitCommand);

      const metadata: WorktreeMetadata = {
        worktreePath,
        worktreeBaseDir,
        branchName,
        taskIndex,
        agentName: task.agent,
        runId: options.runId,
        repoRoot,
        baseBranch,
        baseCommit,
        createdAt: new Date().toISOString(),
      };

      const preparedWorktree = {
        ...metadata,
        taskCwd,
      };
      preparedWorktrees.push(preparedWorktree);

      await applyWorktreeSetup(preparedWorktree, {
        repoRoot,
        setupMode,
        setupHookPath,
        setupHookTimeoutMs,
        setupHookRunner,
        signal: options.signal,
      });

      if (options.artifactDir) {
        await writeWorktreeManifest(preparedWorktree, options.artifactDir);
      }
    }
  } catch (error) {
    if (preparedWorktrees.length > 0) {
      try {
        await cleanupWorktrees({ worktrees: preparedWorktrees, gitCommand });
      } catch (rollbackCleanupError) {
        throw withRollbackCleanupError(error, rollbackCleanupError);
      }
    }
    throw error;
  }

  return preparedWorktrees;
}

export async function captureWorktreePatch(
  options: CaptureWorktreePatchOptions,
): Promise<{ patchPath: string; diffstatPath: string; manifestPath: string }> {
  /* v8 ignore next -- production default path; tests inject gitCommand for deterministic orchestration */
  const gitCommand = options.gitCommand ?? defaultGitCommand;
  const { worktree, artifactDir } = options;
  const syntheticPaths = validateSyntheticPaths(worktree.syntheticPaths);

  await fs.promises.mkdir(artifactDir, { recursive: true, mode: 0o700 });

  // Stage untracked files with git add -N (intent-to-add), excluding setup-created synthetic paths.
  // Use NUL-terminated porcelain so pathnames are literal: no C-quoting and no trimming of legitimate whitespace.
  const statusResult = await gitCommand(["status", "--porcelain=v1", "-z"], { cwd: worktree.worktreePath });
  const untrackedFiles = untrackedFilesFromPorcelainStatusZ(statusResult.stdout)
    .filter((filePath) => !isSyntheticPath(filePath, syntheticPaths));

  let intentToAddFiles: string[] = [];

  try {
    if (untrackedFiles.length > 0) {
      intentToAddFiles = untrackedFiles;
      await gitCommand(["add", "-N", "--", ...untrackedFiles], { cwd: worktree.worktreePath });
    }

    // Generate diff from the original base commit so committed child changes are preserved.
    const diffResult = await gitCommand(
      ["diff", worktree.baseCommit, "--binary", "--no-color", "--no-ext-diff", ...diffPathspecArgs(syntheticPaths)],
      { cwd: worktree.worktreePath },
    );

    // Generate diffstat from the original base commit so committed child changes are preserved.
    const diffstatResult = await gitCommand(
      ["diff", worktree.baseCommit, "--stat", "--no-color", ...diffPathspecArgs(syntheticPaths)],
      { cwd: worktree.worktreePath },
    );

    // Write artifacts
    const patchPath = path.join(artifactDir, `worktree-${worktree.taskIndex}.patch`);
    const diffstatPath = path.join(artifactDir, `worktree-${worktree.taskIndex}.diffstat.txt`);
    const manifestPath = path.join(artifactDir, `worktree-${worktree.taskIndex}.worktree.json`);

    await writeArtifactTextFile(patchPath, diffResult.stdout);
    await writeArtifactTextFile(diffstatPath, diffstatResult.stdout);
    await writeArtifactTextFile(manifestPath, `${JSON.stringify(worktree, null, 2)}\n`);

    return { patchPath, diffstatPath, manifestPath };
  } finally {
    if (intentToAddFiles.length > 0) {
      await gitCommand(["reset", "--", ...intentToAddFiles], { cwd: worktree.worktreePath });
    }
  }
}

export async function cleanupWorktree(options: CleanupWorktreeOptions): Promise<void> {
  /* v8 ignore next -- production default path; tests inject gitCommand for deterministic orchestration */
  const gitCommand = options.gitCommand ?? defaultGitCommand;
  const { worktree } = options;
  const worktreeBaseDir = worktree.worktreeBaseDir ?? DEFAULT_WORKTREE_BASE;

  if (!isOwnedWorktreePath(worktree.worktreePath, worktreeBaseDir)) {
    throw new WorktreeError(
      `Worktree path ${worktree.worktreePath} is not under managed worktree directory ${worktreeBaseDir}`,
      "UNOWNED_WORKTREE_PATH",
      { worktreePath: worktree.worktreePath, worktreeBaseDir },
    );
  }

  let manifestError: unknown;
  if (options.artifactDir) {
    try {
      await writeWorktreeManifest(worktree, options.artifactDir);
    } catch (error) {
      manifestError = error;
    }
  }

  try {
    await removeWorktree(worktree.worktreePath, worktree.branchName, worktree.repoRoot, gitCommand);
  } catch (removeError) {
    if (manifestError) {
      throw new WorktreeError(
        `Failed to write cleanup manifest and remove worktree ${worktree.worktreePath}`,
        "CLEANUP_FAILED",
        { manifestError: String(manifestError), removeError: String(removeError) },
      );
    }
    throw removeError;
  }

  if (manifestError) throw manifestError;
}

export async function cleanupWorktrees(options: CleanupWorktreesOptions): Promise<void> {
  const errors: Array<{ worktree: WorktreeMetadata; error: Error }> = [];

  for (const worktree of options.worktrees) {
    try {
      await cleanupWorktree({
        worktree,
        artifactDir: options.artifactDir,
        gitCommand: options.gitCommand,
      });
    } catch (error) {
      errors.push({ worktree, error: error as Error });
    }
  }

  if (errors.length > 0) {
    const errorMessages = errors
      .map((e) => `${e.worktree.worktreePath}: ${e.error.message}`)
      .join("\n");
    throw new WorktreeError(
      `Failed to cleanup ${errors.length} worktree(s):\n${errorMessages}`,
      "CLEANUP_PARTIAL_FAILURE",
      { errors },
    );
  }
}
