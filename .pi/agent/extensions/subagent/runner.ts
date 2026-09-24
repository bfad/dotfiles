import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { Message } from "@mariozechner/pi-ai";
import { withFileMutationQueue } from "@mariozechner/pi-coding-agent";
import { childPiEnv, withAgentClass } from "../../lib/child-pi-env.js";
import { isAgentThinkingLevel, type AgentConfig } from "./agents.js";
import { getFinalOutput } from "./output.js";
import type { ChildActivityCallback, ChildActivityUpdate, OnUpdateCallback, SingleResult, SubagentDetails, UsageStats } from "./types.js";

export const DEFAULT_HARD_TIMEOUT_MS = 30 * 60 * 1000;
export const DEFAULT_IDLE_TIMEOUT_MS = 5 * 60 * 1000;
export const DEFAULT_TERM_GRACE_MS = 5 * 1000;
export const MAX_ACTIVITY_PREVIEW_CHARS = 500;
export const MAX_ACTIVITY_PATH_CHARS = 300;

const AGENT_TEAMS_IDENTITY_AND_SPAWN_ENV_KEYS = [
  "PI_TEAM_NAME",
  "PI_TEAM_AGENT_NAME",
  "PI_TEAM_SPAWN_KIND",
  "PI_TEAM_SUBAGENT_ROLE",
  "PI_TEAM_SUBAGENT_FILE",
] as const;

const ANSI_ESCAPE_SEQUENCE_RE = /\u001b(?:\[[0-?]*[ -/]*[@-~]|\][^\u0007\u001b]*(?:\u0007|\u001b\\)?|[PX^_][\s\S]*?(?:\u001b\\|$)|[@-Z\\-_])/g;

export interface PiInvocation {
  command: string;
  args: string[];
}

export interface TempPromptFile {
  dir: string;
  filePath: string;
}

export interface RunnerLifecycleOptions {
  timeoutMs?: number;
  idleTimeoutMs?: number;
  termGraceMs?: number;
}

export interface RunnerOptions extends RunnerLifecycleOptions {
  onActivity?: ChildActivityCallback;
  /** Parent's live model, used when an agent's frontmatter `model` is `"inherit"`. */
  parentModel?: { provider: string; id: string };
  /** Call-time model override from the subagent tool params. Wins over frontmatter. Accepts `"inherit"`. */
  modelOverride?: string;
}

interface ResolvedRunnerLifecycleOptions {
  timeoutMs: number;
  idleTimeoutMs: number;
  termGraceMs: number;
}

export interface RunnerDependencies {
  spawn: typeof spawn;
  getPiInvocation: (args: string[]) => PiInvocation;
  writePromptToTempFile: (agentName: string, prompt: string) => Promise<TempPromptFile>;
  killProcess: (pid: number, signal?: NodeJS.Signals | 0) => boolean;
  platform: NodeJS.Platform;
}

type Timer = ReturnType<typeof setTimeout>;

export function emptyUsageStats(): UsageStats {
  return { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0, contextTokens: 0, turns: 0 };
}

// Standalone JSON-mode children are not Agent Teams members. Scrub the parent
// teammate's names and spawn metadata to prevent impersonation, but preserve
// PI_TEAM_ROLE for teammate-aware tools that must bypass prompts in headless mode.
// The scrubbed identity is replaced with the child's own agent class, so consumers
// see what this child IS instead of inheriting what its parent was.
function buildStandaloneSubagentEnv(
  parentEnv: NodeJS.ProcessEnv,
  agentName: string,
): NodeJS.ProcessEnv {
  const env = childPiEnv(parentEnv);
  for (const key of AGENT_TEAMS_IDENTITY_AND_SPAWN_ENV_KEYS) delete env[key];
  return withAgentClass(env, agentName);
}

export function resolveRunnerLifecycleOptions(options: RunnerLifecycleOptions = {}): ResolvedRunnerLifecycleOptions {
  return {
    timeoutMs: options.timeoutMs ?? DEFAULT_HARD_TIMEOUT_MS,
    idleTimeoutMs: options.idleTimeoutMs ?? DEFAULT_IDLE_TIMEOUT_MS,
    termGraceMs: options.termGraceMs ?? DEFAULT_TERM_GRACE_MS,
  };
}

export function resolveEffectiveModel(
  agent: AgentConfig,
  options: Pick<RunnerOptions, "parentModel" | "modelOverride"> = {},
): string | undefined {
  const modelOverride = options.modelOverride?.trim() || undefined;
  const requestedModel = modelOverride || agent.model;
  return requestedModel === "inherit"
    ? (options.parentModel ? `${options.parentModel.provider}/${options.parentModel.id}` : undefined)
    : requestedModel;
}

export async function writePromptToTempFile(agentName: string, prompt: string): Promise<TempPromptFile> {
  const tmpDir = await fs.promises.mkdtemp(path.join(os.tmpdir(), "pi-subagent-"));
  const safeName = agentName.replace(/[^\w.-]+/g, "_");
  const filePath = path.join(tmpDir, `prompt-${safeName}.md`);
  await withFileMutationQueue(filePath, async () => {
    await fs.promises.writeFile(filePath, prompt, { encoding: "utf-8", mode: 0o600 });
  });
  return { dir: tmpDir, filePath };
}

export function getPiInvocation(args: string[]): PiInvocation {
  const currentScript = process.argv[1];
  if (currentScript && fs.existsSync(currentScript)) {
    return { command: process.execPath, args: [currentScript, ...args] };
  }

  const execName = path.basename(process.execPath).toLowerCase();
  const isGenericRuntime = /^(node|bun)(\.exe)?$/.test(execName);
  if (!isGenericRuntime) {
    return { command: process.execPath, args };
  }

  return { command: "pi", args };
}

function armTimer(callback: () => void, delayMs: number): Timer {
  const timer = setTimeout(callback, delayMs);
  timer.unref();
  return timer;
}

function appendStderr(result: SingleResult, message: string): void {
  result.stderr += `${result.stderr ? "\n" : ""}${message}`;
}

function sanitizePreview(value: unknown, limit = MAX_ACTIVITY_PREVIEW_CHARS): string | undefined {
  if (typeof value !== "string") return undefined;
  const normalized = value
    .replace(ANSI_ESCAPE_SEQUENCE_RE, "")
    .replace(/\0/g, "")
    .replace(/\r\n/g, "\n")
    .replace(/\r/g, "\n")
    .trim();
  if (!normalized) return undefined;
  return normalized.length > limit ? `${normalized.slice(0, Math.max(0, limit - 1))}…` : normalized;
}

function isWindowsAbsolutePath(value: string): boolean {
  return /^[a-z]:[\\/]/i.test(value) || /^\\\\[^\\/\s]+[\\/][^\\/\s]+/.test(value);
}

function sanitizePathTarget(value: unknown): string | undefined {
  const preview = sanitizePreview(value, MAX_ACTIVITY_PATH_CHARS);
  if (!preview) return undefined;
  if (/\n/.test(preview)) return undefined;
  if (isWindowsAbsolutePath(preview)) return preview;
  if (/^[a-z][a-z0-9+.-]*:/i.test(preview)) return undefined;
  return preview;
}

function extractPathTarget(args: unknown): string | undefined {
  if (!args || typeof args !== "object" || Array.isArray(args)) return undefined;
  const record = args as Record<string, unknown>;
  const keys = ["path", "file_path", "filePath", "filepath", "cwd"];
  for (const key of keys) {
    const target = sanitizePathTarget(record[key]);
    if (target) return target;
  }
  return undefined;
}

function extractToolPreview(toolName: string, args: unknown): string | undefined {
  if (!args || typeof args !== "object" || Array.isArray(args)) return undefined;
  const record = args as Record<string, unknown>;
  if (toolName === "edit" && Array.isArray(record.edits)) return `${record.edits.length} edit(s)`;
  if (toolName === "write" && typeof record.content === "string") return `content chars=${record.content.length}`;
  return undefined;
}

function textPreviewFromMessage(message: Message): string | undefined {
  if (typeof message.content === "string") return sanitizePreview(message.content);
  if (!Array.isArray(message.content)) return undefined;
  const text = message.content
    .filter((part: any) => part?.type === "text" && typeof part.text === "string")
    .map((part: any) => part.text)
    .join("\n");
  return sanitizePreview(text);
}

function activityUpdatesFromAssistantMessage(message: Message, usage: UsageStats, model?: string): ChildActivityUpdate[] {
  const updates: ChildActivityUpdate[] = [];
  const text = textPreviewFromMessage(message);
  if (text) updates.push({ type: "message", text });

  const content = Array.isArray(message.content) ? message.content : [];
  for (const part of content) {
    if (part?.type !== "toolCall") continue;
    const toolName = sanitizePreview(part.name, 120) ?? "unknown";
    const toolCallId = sanitizePreview(part.id, 160);
    updates.push({
      type: "tool_started",
      toolName,
      ...(toolCallId ? { toolCallId } : {}),
      path: extractPathTarget(part.arguments),
      preview: extractToolPreview(toolName, part.arguments),
    });
  }

  updates.push({ type: "usage", usage: { ...usage }, ...(model ? { model } : {}) });
  return updates;
}

function activityUpdateFromToolResult(message: Message): ChildActivityUpdate {
  const toolMessage = message as Message & { toolName?: unknown; toolCallId?: unknown; isError?: unknown };
  const toolCallId = sanitizePreview(toolMessage.toolCallId, 160);
  return {
    type: "tool_finished",
    toolName: sanitizePreview(toolMessage.toolName, 120),
    ...(toolCallId ? { toolCallId } : {}),
    isError: toolMessage.isError === true,
  };
}

function emitActivity(onActivity: ChildActivityCallback | undefined, activity: ChildActivityUpdate): void {
  if (onActivity) onActivity(activity);
}

function processGroupTarget(pid: number, platform: NodeJS.Platform): number {
  return platform === "win32" ? pid : -pid;
}

function sendChildSignal(
  proc: ChildProcessWithoutNullStreams,
  deps: Pick<RunnerDependencies, "killProcess" | "platform">,
  signal: NodeJS.Signals,
): void {
  const pid = proc.pid;
  if (typeof pid === "number" && pid > 0) {
    try {
      deps.killProcess(processGroupTarget(pid, deps.platform), signal);
      return;
    } catch {
      // Fall back to ChildProcess.kill below.
    }
  }

  try {
    proc.kill(signal);
  } catch {
    // Process already exited or could not be signaled.
  }
}

function isChildStillActive(
  proc: ChildProcessWithoutNullStreams,
  deps: Pick<RunnerDependencies, "killProcess" | "platform">,
): boolean {
  const pid = proc.pid;
  if (typeof pid !== "number" || pid <= 0) return true;

  try {
    deps.killProcess(processGroupTarget(pid, deps.platform), 0);
    return true;
  } catch {
    return false;
  }
}

function terminateChild(
  proc: ChildProcessWithoutNullStreams,
  deps: Pick<RunnerDependencies, "killProcess" | "platform">,
  termGraceMs: number,
): Timer {
  sendChildSignal(proc, deps, "SIGTERM");
  return armTimer(() => {
    if (isChildStillActive(proc, deps)) sendChildSignal(proc, deps, "SIGKILL");
  }, termGraceMs);
}

export async function runSingleAgent(
  defaultCwd: string,
  agents: AgentConfig[],
  agentName: string,
  task: string,
  cwd: string | undefined,
  step: number | undefined,
  signal: AbortSignal | undefined,
  onUpdate: OnUpdateCallback | undefined,
  makeDetails: (results: SingleResult[]) => SubagentDetails,
  options: RunnerOptions = {},
  deps: Partial<RunnerDependencies> = {},
): Promise<SingleResult> {
  const { onActivity } = options;
  const runnerDeps: RunnerDependencies = {
    spawn,
    getPiInvocation,
    writePromptToTempFile,
    killProcess: (pid, signalToSend) => process.kill(pid, signalToSend),
    platform: process.platform,
    ...deps,
  };
  const lifecycle = resolveRunnerLifecycleOptions(options);
  const agent = agents.find((a) => a.name === agentName);

  if (!agent) {
    const available = agents.map((a) => `"${a.name}"`).join(", ") || "none";
    const message = `Unknown agent: "${agentName}". Available agents: ${available}.`;
    return {
      agent: agentName,
      agentSource: "unknown",
      task,
      exitCode: 1,
      messages: [],
      stderr: message,
      usage: emptyUsageStats(),
      errorMessage: message,
      step,
    };
  }

  const args: string[] = ["--mode", "json", "-p", "--no-session"];
  // Resolution: call-time modelOverride > agent frontmatter model. Either may be "inherit",
  // which resolves to the parent's live model. Blank overrides are treated as omitted so a
  // caller can't accidentally drop a pinned frontmatter model.
  const modelOverride = options.modelOverride?.trim() || undefined;
  const effectiveModel = resolveEffectiveModel(agent, options);
  if (effectiveModel) args.push("--model", effectiveModel);
  // Suffix detection is intentionally syntactic. The parent process has no child-model
  // registry, so it cannot mirror Pi's exact-model-first resolution for IDs ending in `:level`.
  const callTimeModelSuffix = modelOverride?.includes(":")
    ? modelOverride.split(":").at(-1)
    : undefined;
  const callTimeModelSetsThinking = isAgentThinkingLevel(callTimeModelSuffix);
  if (agent.thinking && !callTimeModelSetsThinking) args.push("--thinking", agent.thinking);
  if (agent.tools && agent.tools.length > 0) args.push("--tools", agent.tools.join(","));
  if (agent.excludeTools && agent.excludeTools.length > 0) args.push("--exclude-tools", agent.excludeTools.join(","));

  let tmpPromptDir: string | null = null;
  let tmpPromptPath: string | null = null;

  const currentResult: SingleResult = {
    agent: agentName,
    agentSource: agent.source,
    task,
    exitCode: -1,
    messages: [],
    stderr: "",
    usage: emptyUsageStats(),
    model: effectiveModel,
    step,
  };

  const emitUpdate = () => {
    if (onUpdate) {
      onUpdate({
        content: [{ type: "text", text: getFinalOutput(currentResult.messages) || "(running...)" }],
        details: makeDetails([currentResult]),
      });
    }
  };

  try {
    if (agent.systemPrompt.trim()) {
      const tmp = await runnerDeps.writePromptToTempFile(agent.name, agent.systemPrompt);
      tmpPromptDir = tmp.dir;
      tmpPromptPath = tmp.filePath;
      args.push("--append-system-prompt", tmpPromptPath);
    }

    args.push(`Task: ${task}`);
    let wasAborted = false;

    await new Promise<void>((resolve) => {
      const invocation = runnerDeps.getPiInvocation(args);
      let proc: ChildProcessWithoutNullStreams;
      try {
        proc = runnerDeps.spawn(invocation.command, invocation.args, {
          cwd: cwd ?? defaultCwd,
          detached: runnerDeps.platform !== "win32",
          shell: false,
          stdio: ["ignore", "pipe", "pipe"],
          env: buildStandaloneSubagentEnv(process.env, agentName),
        }) as unknown as ChildProcessWithoutNullStreams;
      } catch (error) {
        currentResult.exitCode = 1;
        currentResult.stopReason = "error";
        currentResult.errorMessage = `Spawn error: ${String(error)}`;
        appendStderr(currentResult, currentResult.errorMessage);
        resolve();
        return;
      }
      let buffer = "";
      let finished = false;
      let settled = false;
      let processClosed = false;
      let cleanupAbort = () => {};
      let hardTimer: Timer;
      let idleTimer: Timer;
      let terminationTimer: Timer | undefined;
      let terminationSettleTimer: Timer | undefined;
      let pendingAssistantTerminal: { stopReason: string; errorMessage?: string } | undefined;

      const cleanupListeners = () => {
        cleanupAbort();
        proc.stdout.removeListener("data", onStdoutData);
        proc.stderr.removeListener("data", onStderrData);
        proc.removeListener("close", onClose);
        proc.removeListener("error", onError);
      };

      const settle = () => {
        if (settled) return;
        settled = true;
        clearTimeout(hardTimer);
        clearTimeout(idleTimer);
        if (terminationTimer) clearTimeout(terminationTimer);
        if (terminationSettleTimer) clearTimeout(terminationSettleTimer);
        cleanupListeners();
        resolve();
      };

      const finish = (code: number, terminate: boolean) => {
        if (finished) return;
        finished = true;
        currentResult.exitCode = code;
        clearTimeout(hardTimer);
        clearTimeout(idleTimer);
        cleanupAbort();
        proc.stdout.removeListener("data", onStdoutData);
        proc.stderr.removeListener("data", onStderrData);
        if (terminate && !processClosed) {
          terminationTimer = terminateChild(proc, runnerDeps, lifecycle.termGraceMs);
          terminationSettleTimer = armTimer(settle, lifecycle.termGraceMs);
          return;
        }
        settle();
      };

      const finishPendingAssistantTerminal = (
        terminal: { stopReason: string; errorMessage?: string },
        terminate: boolean,
      ) => {
        const { stopReason, errorMessage } = terminal;
        pendingAssistantTerminal = undefined;
        currentResult.stopReason = stopReason;
        currentResult.errorMessage = errorMessage;
        finish(stopReason === "length" ? 0 : 1, terminate);
        emitUpdate();
      };

      const consumePendingAssistantDiagnostic = () => {
        const terminal = pendingAssistantTerminal;
        pendingAssistantTerminal = undefined;
        if (!terminal) return "";
        if (terminal.stopReason === "length") {
          return " Pending assistant response was truncated before recovery settled.";
        }
        const detail = terminal.errorMessage ? `: ${terminal.errorMessage}` : "";
        return ` Pending assistant ${terminal.stopReason} before recovery settled${detail}.`;
      };

      const failWithStopReason = (stopReason: string, message: string) => {
        pendingAssistantTerminal = undefined;
        currentResult.stopReason = stopReason;
        currentResult.errorMessage = message;
        appendStderr(currentResult, message);
        finish(1, true);
      };

      const resetIdleTimer = () => {
        clearTimeout(idleTimer);
        idleTimer = armTimer(() => {
          failWithStopReason("idleTimeout", `Subagent idle timeout after ${lifecycle.idleTimeoutMs}ms without output.`);
        }, lifecycle.idleTimeoutMs);
      };

      const processLine = (line: string) => {
        if (!line.trim()) return;
        let event: any;
        try {
          event = JSON.parse(line);
        } catch {
          return;
        }

        if (event.type === "message_end" && event.message) {
          const msg = event.message as Message;
          currentResult.messages.push(msg);

          if (msg.role === "assistant") {
            currentResult.usage.turns++;
            const usage = msg.usage;
            if (usage) {
              currentResult.usage.input += usage.input || 0;
              currentResult.usage.output += usage.output || 0;
              currentResult.usage.cacheRead += usage.cacheRead || 0;
              currentResult.usage.cacheWrite += usage.cacheWrite || 0;
              currentResult.usage.cost += usage.cost?.total || 0;
              currentResult.usage.contextTokens = usage.totalTokens || 0;
            }
            if (!currentResult.model && msg.model) currentResult.model = msg.model;

            for (const activity of activityUpdatesFromAssistantMessage(msg, currentResult.usage, currentResult.model)) {
              emitActivity(onActivity, activity);
            }

            // Pi can recover error and truncated responses after message_end. Let
            // its retry and compaction lifecycle settle before treating them as final.
            if (msg.stopReason === "error" || msg.stopReason === "length") {
              pendingAssistantTerminal = { stopReason: msg.stopReason, errorMessage: msg.errorMessage };
            } else {
              pendingAssistantTerminal = undefined;
              if (msg.stopReason) currentResult.stopReason = msg.stopReason;
              currentResult.errorMessage = msg.errorMessage;

              if (msg.stopReason && msg.stopReason !== "toolUse") {
                finish(msg.stopReason === "stop" ? 0 : 1, !processClosed);
              }
            }
          }
          emitUpdate();
        }

        if (event.type === "agent_end" && pendingAssistantTerminal && typeof event.willRetry !== "boolean") {
          finishPendingAssistantTerminal(pendingAssistantTerminal, !processClosed);
        }

        if (event.type === "agent_settled" && pendingAssistantTerminal) {
          finishPendingAssistantTerminal(pendingAssistantTerminal, !processClosed);
        }

        if (event.type === "tool_result_end" && event.message) {
          const msg = event.message as Message;
          currentResult.messages.push(msg);
          emitActivity(onActivity, activityUpdateFromToolResult(msg));
          emitUpdate();
        }
      };

      const flushBufferedStdout = () => {
        const lines = buffer.split("\n");
        buffer = "";
        for (const line of lines) processLine(line);
      };

      function onStdoutData(data: Buffer | string) {
        resetIdleTimer();
        buffer += data.toString();
        const lines = buffer.split("\n");
        buffer = lines.pop() || "";
        for (const line of lines) processLine(line);
      }

      function onStderrData(data: Buffer | string) {
        resetIdleTimer();
        const text = data.toString();
        currentResult.stderr += text;
        const preview = sanitizePreview(text);
        if (preview) emitActivity(onActivity, { type: "stderr", preview });
      }

      function onClose(code: number | null, signal: NodeJS.Signals | null) {
        processClosed = true;
        flushBufferedStdout();

        if (finished) {
          settle();
          return;
        }

        if (signal) {
          currentResult.stopReason = "signal";
          currentResult.errorMessage = `Subagent process closed after signal ${signal}.${consumePendingAssistantDiagnostic()}`;
          appendStderr(currentResult, currentResult.errorMessage);
          finish(1, false);
          return;
        }

        if (code === null) {
          currentResult.stopReason = "closed";
          currentResult.errorMessage = `Subagent process closed without an exit code.${consumePendingAssistantDiagnostic()}`;
          appendStderr(currentResult, currentResult.errorMessage);
          finish(1, false);
          return;
        }

        if (code !== 0) {
          currentResult.stopReason = "exit";
          currentResult.errorMessage = `Subagent process exited with code ${code}.${consumePendingAssistantDiagnostic()}`;
          appendStderr(currentResult, currentResult.errorMessage);
          finish(code, false);
          return;
        }
        if (pendingAssistantTerminal) {
          finishPendingAssistantTerminal(pendingAssistantTerminal, false);
          return;
        }
        finish(code, false);
      }

      function onError(error: Error) {
        if (finished) {
          settle();
          return;
        }
        currentResult.stopReason = "error";
        currentResult.errorMessage = `Spawn error: ${error.message}`;
        appendStderr(currentResult, currentResult.errorMessage);
        finish(1, false);
      }

      proc.stdout.on("data", onStdoutData);
      proc.stderr.on("data", onStderrData);
      proc.once("close", onClose);
      proc.once("error", onError);

      hardTimer = armTimer(() => {
        failWithStopReason("timeout", `Subagent hard timeout after ${lifecycle.timeoutMs}ms.`);
      }, lifecycle.timeoutMs);
      idleTimer = armTimer(() => {
        failWithStopReason("idleTimeout", `Subagent idle timeout after ${lifecycle.idleTimeoutMs}ms without output.`);
      }, lifecycle.idleTimeoutMs);

      if (signal) {
        const abortHandler = () => {
          wasAborted = true;
          currentResult.stopReason = "aborted";
          currentResult.errorMessage = "Subagent was aborted";
          appendStderr(currentResult, "Subagent was aborted");
          finish(1, true);
        };
        if (signal.aborted) abortHandler();
        else {
          signal.addEventListener("abort", abortHandler, { once: true });
          cleanupAbort = () => signal.removeEventListener("abort", abortHandler);
        }
      }
    });

    if (wasAborted) throw new Error("Subagent was aborted");
    return currentResult;
  } finally {
    if (tmpPromptPath)
      try {
        fs.unlinkSync(tmpPromptPath);
      } catch {
        /* ignore */
      }
    if (tmpPromptDir)
      try {
        fs.rmdirSync(tmpPromptDir);
      } catch {
        /* ignore */
      }
  }
}
