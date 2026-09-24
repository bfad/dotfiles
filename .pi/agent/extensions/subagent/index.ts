/**
 * Subagent Tool - Delegate tasks to specialized agents
 *
 * Spawns a separate `pi` process for each subagent invocation,
 * giving it an isolated context window.
 *
 * Supports three modes:
 *   - Single: { agent: "name", task: "..." }
 *   - Parallel: { tasks: [{ agent: "name", task: "..." }, ...] }
 *   - Chain: { chain: [{ agent: "name", task: "... {previous} ..." }, ...] }
 *
 * Uses JSON mode to capture structured output from subagents.
 */

import * as os from "node:os";
import * as path from "node:path";
import { StringEnum } from "@mariozechner/pi-ai";
import { type ExtensionAPI, getMarkdownTheme } from "@mariozechner/pi-coding-agent";
import { Container, Markdown, Spacer, Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import { type AgentConfig, discoverAgents } from "./agents.js";
import {
  findRejectedDisabledAgents,
  formatDisabledAgentsError,
  loadSubagentConfig,
  type RequestedAgentRef,
} from "./config.js";
import {
  DEFAULT_MAX_OUTPUT_CHARS,
  DEFAULT_SYNTHESIS_INPUT_CHARS,
  type ArtifactPlan,
  type OutputMode,
  type ReadPreview,
  type ResultArtifactPaths,
  buildReadPreviews,
  buildSynthesisTask,
  previewLimitForMode,
  resolveArtifactPlan,
  resolveMaxOutput,
  resultArtifactPaths,
  resultArtifactStatus,
  synthesisInputArtifactPath,
  truncateText,
  writeArtifactTextFile,
  writeResultArtifacts,
  writeRunArtifactIndex,
} from "./artifacts.js";
import { mapWithConcurrencyLimit } from "./concurrency.js";
import { aggregateUsage, formatUsageStats, getDisplayItems, getFinalOutput } from "./output.js";
import { ParallelWriteSafetyError, inferTaskMutability, resolveEffectiveTaskCwd, validateParallelTasks } from "./parallel-safety.js";
import { PatchApplyError, applyWorktreePatches, formatPatchApplyResult, type PatchApplyResult } from "./patch-apply.js";
import { formatDoctorReport, runSubagentDoctor } from "./doctor.js";
import {
  RunStateWriter,
  createRunTaskSummary,
  formatPruneStaleRunsResult,
  formatRecentRuns,
  formatRunStatus,
  listRecentRuns,
  pruneStaleRuns,
  readRunStatus,
  runStateForFinishedTasks,
} from "./run-state.js";
import { DEFAULT_HARD_TIMEOUT_MS, DEFAULT_IDLE_TIMEOUT_MS, emptyUsageStats, resolveEffectiveModel, runSingleAgent } from "./runner.js";
import {
  DEFAULT_WORKTREE_SETUP_HOOK_TIMEOUT_MS,
  WorktreeError,
  captureWorktreePatch,
  cleanupWorktree,
  prepareParallelWorktrees,
  type PreparedWorktree,
  type WorktreeSetupMode,
} from "./worktrees.js";
import type {
  AgentScope,
  ChildActivityCallback,
  DisplayItem,
  OnUpdateCallback,
  SingleResult,
  SubagentDetails,
  SynthesizeWith,
  WorktreeCleanupPolicy,
  WorktreeResultMetadata,
} from "./types.js";

const MAX_PARALLEL_TASKS = 8;
const MAX_CONCURRENCY = 4;
const COLLAPSED_ITEM_COUNT = 10;
const FILE_ONLY_SUMMARY_MAX_CHARS = 12_000;
const FILE_ONLY_CARD_PREVIEW_CHARS = 600;

function hasMeaningfulText(value: unknown): boolean {
  return typeof value === "string" && value.trim().length > 0;
}

function normalizeSubagentParams(params: {
  worktreeSetup?: WorktreeSetupMode;
  worktreeSetupHook?: string;
  worktreeSetupHookTimeoutMs?: number;
  worktreeCleanup?: WorktreeCleanupPolicy;
  synthesizeWith?: SynthesizeWith;
}): {
  worktreeSetup: WorktreeSetupMode;
  worktreeSetupHook?: string;
  worktreeSetupHookTimeoutMs?: number;
  worktreeCleanupPolicy: WorktreeCleanupPolicy;
  hasWorktreeSetupOptions: boolean;
  hasWorktreeCleanupOption: boolean;
  synthesizeWith?: SynthesizeWith;
} {
  const worktreeSetup = params.worktreeSetup ?? "none";
  const worktreeSetupHook = hasMeaningfulText(params.worktreeSetupHook)
    ? params.worktreeSetupHook
    : undefined;
  const worktreeSetupHookTimeoutMs = worktreeSetupHook
    ? params.worktreeSetupHookTimeoutMs
    : undefined;
  const isBlankDefaultSynthesizeWith =
    params.synthesizeWith !== undefined &&
    !hasMeaningfulText(params.synthesizeWith.agent) &&
    !hasMeaningfulText(params.synthesizeWith.task) &&
    !hasMeaningfulText(params.synthesizeWith.cwd) &&
    (params.synthesizeWith.maxInputChars === undefined ||
      params.synthesizeWith.maxInputChars === DEFAULT_SYNTHESIS_INPUT_CHARS);
  const synthesizeWith = isBlankDefaultSynthesizeWith ? undefined : params.synthesizeWith;

  return {
    worktreeSetup,
    worktreeSetupHook,
    worktreeSetupHookTimeoutMs,
    worktreeCleanupPolicy: params.worktreeCleanup ?? "always",
    hasWorktreeSetupOptions:
      worktreeSetup !== "none" ||
      worktreeSetupHook !== undefined ||
      worktreeSetupHookTimeoutMs !== undefined,
    hasWorktreeCleanupOption: params.worktreeCleanup !== undefined && params.worktreeCleanup !== "always",
    synthesizeWith,
  };
}

function capText(text: string, maxOutput: number): string {
  return truncateText(text, maxOutput).text;
}

function safeJsonPreview(value: unknown): string {
  try {
    return JSON.stringify(value) ?? String(value);
  } catch {
    return String(value);
  }
}

function liveResultLabel(mode: SubagentDetails["mode"], result: SingleResult, index: number): string {
  if (mode === "chain" && result.step) return `step ${result.step} [${result.agent}]`;
  if (mode === "parallel") return `worker ${index + 1} [${result.agent}]`;
  return `[${result.agent}]`;
}

function isLiveRunningResult(mode: SubagentDetails["mode"], result: SingleResult, index: number, total: number): boolean {
  if (result.exitCode === -1) return true;
  if (result.stopReason && result.stopReason !== "toolUse") return false;
  if (mode === "single") return true;
  if (mode === "chain") return index === total - 1;
  return false;
}

function formatLiveResultPreview(result: SingleResult, maxChars: number): string | undefined {
  if (result.errorMessage) return `error: ${capText(result.errorMessage, maxChars)}`;
  const displayItems = getDisplayItems(result.messages);
  const lastItem = displayItems[displayItems.length - 1];
  if (lastItem?.type === "text") return `text: ${capText(lastItem.text.trim(), maxChars)}`;
  if (lastItem?.type === "toolCall") {
    const args = Object.keys(lastItem.args).length > 0 ? ` ${safeJsonPreview(lastItem.args)}` : "";
    return `tool: ${capText(`${lastItem.name}${args}`, maxChars)}`;
  }
  const stderr = result.stderr.trim();
  if (stderr) return `stderr: ${capText(stderr, maxChars)}`;
  return undefined;
}

function relativeArtifactPath(result: SingleResult, filePath: string): string {
  if (!result.artifactDir) return filePath;
  const relativePath = path.relative(result.artifactDir, filePath);
  return relativePath && !relativePath.startsWith("..") && !path.isAbsolute(relativePath) ? relativePath : filePath;
}

function formatLiveArtifactPaths(result: SingleResult): string {
  const paths = [
    result.artifactResultPath ? `result=${relativeArtifactPath(result, result.artifactResultPath)}` : undefined,
    result.artifactTranscriptPath ? `transcript=${relativeArtifactPath(result, result.artifactTranscriptPath)}` : undefined,
    result.artifactStderrPath ? `stderr=${relativeArtifactPath(result, result.artifactStderrPath)}` : undefined,
    result.outputPath ? `output=${relativeArtifactPath(result, result.outputPath)}` : undefined,
    result.worktree?.patchPath ? `patch=${relativeArtifactPath(result, result.worktree.patchPath)}` : undefined,
    result.worktree?.diffstatPath ? `diffstat=${relativeArtifactPath(result, result.worktree.diffstatPath)}` : undefined,
    result.worktree?.manifestPath ? `worktree=${relativeArtifactPath(result, result.worktree.manifestPath)}` : undefined,
  ].filter(Boolean);
  return paths.join(", ");
}

function formatFileOnlyLiveUpdate(details: SubagentDetails, artifactPlan: ArtifactPlan, maxOutput: number): string {
  const limit = previewLimitForMode("file-only", maxOutput);
  const previewLimit = Math.max(1, Math.floor(limit / Math.max(details.results.length, 1)));
  const running = details.results.filter((result, index) =>
    isLiveRunningResult(details.mode, result, index, details.results.length)
  ).length;
  const done = details.results.length - running;
  const lines = [
    `Subagent ${details.mode} run ${details.runId ?? artifactPlan.runId}: ${done}/${details.results.length} done${running > 0 ? `, ${running} running` : ""}`,
  ];

  details.results.forEach((result, index) => {
    const status = isLiveRunningResult(details.mode, result, index, details.results.length)
      ? "running"
      : resultStatus(result);
    lines.push(`- ${liveResultLabel(details.mode, result, index)} ${status}`);
    const preview = formatLiveResultPreview(result, previewLimit);
    if (preview) lines.push(`  ${preview.replace(/\n/g, "\n  ")}`);
  });

  lines.push(`Artifacts: ${details.artifactDir ?? artifactPlan.artifactDir}`);
  details.results.forEach((result, index) => {
    const artifactPaths = formatLiveArtifactPaths(result);
    if (artifactPaths) lines.push(`- ${liveResultLabel(details.mode, result, index)} artifacts: ${artifactPaths}`);
  });

  return capText(lines.join("\n"), limit);
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function isAbortLikeError(error: unknown): boolean {
  return error instanceof Error && (error.name === "AbortError" || error.message === "Subagent was aborted");
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

function patchApplyActionDetails(result: PatchApplyResult): SubagentDetails["actionDetails"] {
  return {
    action: "apply",
    state: result.state,
    runId: result.runId,
    cwd: result.cwd,
    apply: result.apply,
    threeWay: result.threeWay,
    selected: result.selected,
    skipped: result.skipped,
    commands: result.commands,
    auditPath: result.auditPath,
  };
}

function patchApplyErrorActionDetails(
  error: unknown,
  cwd: string,
  params: { runId?: string; apply?: boolean; threeWay?: boolean },
): SubagentDetails["actionDetails"] {
  if (error instanceof PatchApplyError && isRecord(error.details) && error.details.action === "apply") {
    return error.details as unknown as SubagentDetails["actionDetails"];
  }

  return {
    action: "apply",
    state: "failed",
    ...(params.runId ? { runId: params.runId } : {}),
    cwd,
    ...(params.apply !== undefined ? { apply: params.apply } : {}),
    ...(params.threeWay !== undefined ? { threeWay: params.threeWay } : {}),
    error: {
      message: errorMessage(error),
      ...(error instanceof PatchApplyError ? { code: error.code, auditPath: error.auditPath, details: error.details } : {}),
    },
  };
}

function capUpdateContent(
  onUpdate: OnUpdateCallback | undefined,
  outputMode: OutputMode,
  maxOutput: number,
  artifactPlan: ArtifactPlan,
): OnUpdateCallback | undefined {
  if (!onUpdate) return undefined;
  const limit = previewLimitForMode(outputMode, maxOutput);
  return (partial) => {
    const content = outputMode === "file-only"
      ? [{
          type: "text" as const,
          text: partial.details
            ? formatFileOnlyLiveUpdate(partial.details, artifactPlan, maxOutput)
            : `Subagent running. Artifacts: ${artifactPlan.artifactDir}`,
        }]
      : partial.content.map((item) => item.type === "text" ? { ...item, text: capText(item.text, limit) } : item);
    const details = partial.details
      ? { ...partial.details, results: resultsForParentDetails(partial.details.results, outputMode) }
      : partial.details;
    onUpdate({ ...partial, content, details });
  };
}

function withArtifactInstructions(task: string, paths: ResultArtifactPaths, enabled: boolean): string {
  if (!enabled) return task;
  const assignedPath = paths.outputPath ?? paths.resultPath;
  return [
    task,
    "",
    "---",
    "Subagent artifact-output instructions from the parent:",
    "- Keep the final response concise and summarize what changed or what you found.",
    `- Assigned result path: ${assignedPath}`,
    "- The parent will save your final response and transcript as artifacts; write a separate detailed file only if the task explicitly needs one.",
  ].join("\n");
}

function resultStatus(result: SingleResult): "completed" | "failed" | "canceled" {
  return resultArtifactStatus(result);
}

function isCanceledResult(result: SingleResult): boolean {
  return resultStatus(result) === "canceled";
}

function isFailedResult(result: SingleResult): boolean {
  return resultStatus(result) === "failed";
}

function isUnsuccessfulResult(result: SingleResult): boolean {
  return resultStatus(result) !== "completed";
}

function successCount(results: SingleResult[]): number {
  return results.filter((r) => resultStatus(r) === "completed").length;
}

function withResultPhase(result: SingleResult, phase: NonNullable<SingleResult["phase"]>): SingleResult {
  return result.phase === phase ? result : { ...result, phase };
}

function withDefaultResultPhase(result: SingleResult): SingleResult {
  return result.phase ? result : withResultPhase(result, "worker");
}

function resultsForParentDetails(results: SingleResult[], outputMode: OutputMode): SingleResult[] {
  const phasedResults = results.map(withDefaultResultPhase);
  if (outputMode === "inline") return phasedResults;
  return phasedResults.map((result) => ({ ...result, messages: [] }));
}

function formatReadPreviewSection(readPreviews: ReadPreview[]): string {
  if (readPreviews.length === 0) return "";
  const lines = ["", "Requested reads:"];
  for (const preview of readPreviews) {
    if (preview.error) {
      lines.push(`- ${preview.requestedPath}: ${preview.error}`);
      continue;
    }
    lines.push(`- ${preview.requestedPath}: ${preview.path} (${preview.bytes ?? 0} bytes)`);
    if (preview.preview) lines.push(`  ${preview.preview.replace(/\n/g, "\n  ")}`);
    if (preview.truncated) lines.push("  [read preview truncated]");
  }
  return lines.join("\n");
}

function formatWorktreeCleanupPath(result: SingleResult): string | undefined {
  const worktree = result.worktree;
  if (!worktree) return undefined;

  const details = [`cleanup=${worktree.cleanupState}`, `policy=${worktree.cleanupPolicy}`];
  if (worktree.cleanupReason) details.push(worktree.cleanupReason);
  if (worktree.patchCaptureError) details.push(`patchCaptureError=${worktree.patchCaptureError}`);
  if (worktree.cleanupError) details.push(`cleanupError=${worktree.cleanupError}`);
  if (worktree.cleanupState === "kept") {
    details.push(`worktreePath=${worktree.worktreePath}`);
    details.push(`branch=${worktree.branchName}`);
  }
  return details.join("; ");
}

function formatArtifactPaths(result: SingleResult): string {
  const paths = [
    result.artifactResultPath ? `result=${result.artifactResultPath}` : undefined,
    result.artifactTranscriptPath ? `transcript=${result.artifactTranscriptPath}` : undefined,
    result.artifactStderrPath ? `stderr=${result.artifactStderrPath}` : undefined,
    result.outputPath ? `output=${result.outputPath}` : undefined,
    result.worktree?.patchPath ? `patch=${result.worktree.patchPath}` : undefined,
    result.worktree?.diffstatPath ? `diffstat=${result.worktree.diffstatPath}` : undefined,
    result.worktree?.manifestPath ? `worktree=${result.worktree.manifestPath}` : undefined,
    formatWorktreeCleanupPath(result),
  ].filter(Boolean);
  return paths.join(", ");
}

function worktreeMetadata(
  worktree: PreparedWorktree,
  cleanupPolicy: WorktreeCleanupPolicy,
  cleanupState: WorktreeResultMetadata["cleanupState"] = "kept",
  cleanupReason?: string,
): WorktreeResultMetadata {
  return {
    worktreePath: worktree.worktreePath,
    branchName: worktree.branchName,
    baseCommit: worktree.baseCommit,
    cleanupState,
    cleanupPolicy,
    ...(cleanupReason ? { cleanupReason } : {}),
    ...(worktree.nodeModulesLinked ? { nodeModulesLinked: true } : {}),
    ...(worktree.setupHookPath ? { setupHookPath: worktree.setupHookPath } : {}),
    ...(worktree.setupHookDurationMs !== undefined ? { setupHookDurationMs: worktree.setupHookDurationMs } : {}),
    ...(worktree.setupHookStderr ? { setupHookStderr: worktree.setupHookStderr } : {}),
    ...(worktree.syntheticPaths?.length ? { syntheticPaths: worktree.syntheticPaths } : {}),
  };
}

function cleanupDecision(
  result: SingleResult,
  policy: WorktreeCleanupPolicy,
): { shouldCleanup: boolean; reason: string } {
  if (policy === "never") {
    return { shouldCleanup: false, reason: "worktreeCleanup=never keeps the managed worktree and branch" };
  }
  if (policy === "on-success" && isUnsuccessfulResult(result)) {
    return { shouldCleanup: false, reason: `child ${resultStatus(result)} and policy on-success keeps the worktree for recovery` };
  }
  if (policy === "on-success") {
    return { shouldCleanup: true, reason: "child succeeded and policy on-success removes after patch capture" };
  }
  return { shouldCleanup: true, reason: "policy always removes after patch capture" };
}

async function finalizeWorktreeResult(
  result: SingleResult,
  worktree: PreparedWorktree | undefined,
  artifactDir: string,
  cleanupPolicy: WorktreeCleanupPolicy,
): Promise<SingleResult> {
  if (!worktree) return result;

  const metadata = worktreeMetadata(worktree, cleanupPolicy, "kept");
  try {
    const captured = await captureWorktreePatch({ worktree, artifactDir });
    metadata.patchPath = captured.patchPath;
    metadata.diffstatPath = captured.diffstatPath;
    metadata.manifestPath = captured.manifestPath;
  } catch (error) {
    metadata.cleanupReason = "patch capture failed; kept worktree for recovery";
    metadata.patchCaptureError = errorMessage(error);
    return { ...result, worktree: metadata };
  }

  const decision = cleanupDecision(result, cleanupPolicy);
  metadata.cleanupReason = decision.reason;
  if (!decision.shouldCleanup) return { ...result, worktree: metadata };

  try {
    await cleanupWorktree({ worktree, artifactDir });
    metadata.cleanupState = "removed";
  } catch (error) {
    metadata.cleanupState = "failed";
    metadata.cleanupReason = `${decision.reason}; cleanup attempted but failed`;
    metadata.cleanupError = errorMessage(error);
  }

  return { ...result, worktree: metadata };
}

function worktreeErrorResult(
  task: { agent: string; task: string },
  paths: ResultArtifactPaths,
  error: unknown,
  signal: AbortSignal | undefined,
): SingleResult {
  return {
    agent: task.agent,
    agentSource: "unknown",
    phase: "worker",
    task: task.task,
    exitCode: 1,
    messages: [],
    stderr: errorMessage(error),
    usage: emptyUsageStats(),
    stopReason: signal?.aborted ? "aborted" : "error",
    errorMessage: errorMessage(error),
    artifactDir: paths.artifactDir,
    artifactResultPath: paths.resultPath,
    artifactTranscriptPath: paths.transcriptPath,
    artifactStderrPath: paths.stderrPath,
    artifactSummaryPath: paths.summaryPath,
    outputPath: paths.outputPath,
  };
}

function synthesisFailureResult(
  synthesizeWith: SynthesizeWith,
  task: string,
  paths: ResultArtifactPaths,
  error: unknown,
  stopReason: "error" | "aborted",
): SingleResult {
  const message = errorMessage(error);
  return {
    agent: synthesizeWith.agent,
    agentSource: "unknown",
    phase: "synthesis",
    task,
    exitCode: 1,
    messages: [],
    stderr: message,
    usage: emptyUsageStats(),
    stopReason,
    errorMessage: message,
    artifactDir: paths.artifactDir,
    artifactResultPath: paths.resultPath,
    artifactTranscriptPath: paths.transcriptPath,
    artifactStderrPath: paths.stderrPath,
    artifactSummaryPath: paths.summaryPath,
  };
}

async function cleanupPreparedWorktreesAfterError(worktrees: PreparedWorktree[], artifactDir: string): Promise<void> {
  await Promise.all(
    worktrees.map((worktree) => cleanupWorktree({ worktree, artifactDir }).catch(() => {})),
  );
}

function formatCount(value: number): string {
  return value.toLocaleString("en-US");
}

function countSummaryLines(text: string): number {
  if (text.length === 0) return 0;
  return text.split(/\r\n|\r|\n/).length;
}

function fallbackOutputSummary(result: SingleResult, previewLimit: number): NonNullable<SingleResult["outputSummary"]> {
  const output = getFinalOutput(result.messages) || "";
  const preview = output.slice(0, previewLimit);
  return {
    path: result.artifactResultPath ?? "",
    bytes: Buffer.byteLength(output, "utf-8"),
    chars: output.length,
    lines: countSummaryLines(output),
    sha256: "",
    preview,
    previewChars: preview.length,
    truncated: output.length > preview.length,
    omittedChars: Math.max(0, output.length - preview.length),
  };
}

interface ArtifactSummaryCard {
  pathLines: string[];
  previewLines: string[];
}

function artifactPathLine(label: string, value: string | undefined): string | undefined {
  return value ? `    ${label}: ${value}` : undefined;
}

function formatArtifactCard(
  mode: "single" | "parallel" | "chain",
  result: SingleResult,
  index: number,
  previewLimit: number,
): ArtifactSummaryCard {
  const summary = result.outputSummary ?? fallbackOutputSummary(result, previewLimit);
  const preview = summary.preview.slice(0, previewLimit);
  const cardOmittedChars = Math.max(0, summary.chars - preview.length);
  const cardPreviewTruncated = cardOmittedChars > 0;
  const status = resultStatus(result);
  const phase = result.phase ?? "worker";
  const lineLabel = summary.lines === 1 ? "line" : "lines";
  const stepNote = mode === "chain" && result.step ? ` (step ${result.step} [${result.agent}] ${status})` : "";
  const synthesisNote = phase === "synthesis" ? ` ([${result.agent} synthesis] ${status})` : "";
  const cleanupPath = formatWorktreeCleanupPath(result);
  const pathLines = [
    `[${index + 1}] ${result.agent} / ${phase} — ${status} — ${formatCount(summary.chars)} chars, ${formatCount(summary.lines)} ${lineLabel}, ${formatCount(summary.bytes)} bytes${stepNote}${synthesisNote}`,
    artifactPathLine("full", summary.path || result.artifactResultPath),
    artifactPathLine("transcript", result.artifactTranscriptPath),
    artifactPathLine("stderr", result.artifactStderrPath),
    artifactPathLine("summary", result.artifactSummaryPath),
    artifactPathLine("output", result.outputPath),
    artifactPathLine("patch", result.worktree?.patchPath),
    artifactPathLine("diffstat", result.worktree?.diffstatPath),
    artifactPathLine("worktree", result.worktree?.manifestPath),
    cleanupPath ? `    worktree cleanup: ${cleanupPath}` : undefined,
    `    card preview truncated: ${cardPreviewTruncated}, original: ${formatCount(summary.chars)} chars, omitted from card preview: ${formatCount(cardOmittedChars)} chars`,
    result.errorMessage ? `    error: ${result.errorMessage}` : undefined,
  ].filter((line): line is string => Boolean(line));

  const previewLines = preview
    ? [`    preview: ${preview.replace(/\n/g, "\n    ")}`]
    : [];

  return { pathLines, previewLines };
}

function fileOnlySummaryHeader(
  label: string,
  artifactPlan: ArtifactPlan,
  noRerunSubject = "Full outputs",
  extraLines: string[] = [],
): string[] {
  return [
    label,
    `Artifacts: ${artifactPlan.artifactDir}`,
    `Index: ${path.join(artifactPlan.artifactDir, "index.md")}`,
    ...extraLines,
    `${noRerunSubject} are already saved; do not rerun solely to recover truncated preview text. Read the artifact paths below or the index instead.`,
  ];
}

function appendBudgetedLines(lines: string[], candidateLines: string[], extraBudget: number): number | undefined {
  const candidateLength = candidateLines.join("\n").length + 1;
  if (candidateLength > extraBudget) return undefined;
  lines.push(...candidateLines);
  return extraBudget - candidateLength;
}

function formatBoundedArtifactSummary(
  headerLines: string[],
  cards: ArtifactSummaryCard[],
  readPreviews: ReadPreview[],
): string {
  const pathOnlyLines = [
    ...headerLines,
    ...cards.flatMap((card) => card.pathLines),
  ];
  let extraBudget = FILE_ONLY_SUMMARY_MAX_CHARS - pathOnlyLines.join("\n").length;
  const lines = [...headerLines];
  let omittedPreviews = 0;

  for (const card of cards) {
    lines.push(...card.pathLines);
    if (card.previewLines.length === 0) continue;
    const remaining = appendBudgetedLines(lines, card.previewLines, extraBudget);
    if (remaining === undefined) {
      omittedPreviews += 1;
      continue;
    }
    extraBudget = remaining;
  }

  if (omittedPreviews > 0) {
    const cardLabel = omittedPreviews === 1 ? "card" : "cards";
    const remaining = appendBudgetedLines(
      lines,
      ["", `Card previews omitted for ${omittedPreviews} ${cardLabel} to keep parent output bounded; read the full paths above.`],
      extraBudget,
    );
    if (remaining !== undefined) extraBudget = remaining;
  }

  const readSection = formatReadPreviewSection(readPreviews);
  if (readSection) {
    const remaining = appendBudgetedLines(lines, readSection.split("\n"), extraBudget);
    if (remaining !== undefined) {
      extraBudget = remaining;
    } else {
      appendBudgetedLines(
        lines,
        ["", "Requested read previews omitted to keep parent output bounded."],
        extraBudget,
      );
    }
  }

  return lines.join("\n");
}

function formatFileOnlySummary(
  mode: "single" | "parallel" | "chain",
  artifactPlan: ArtifactPlan,
  results: SingleResult[],
  readPreviews: ReadPreview[],
  maxOutput: number,
): string {
  const previewLimit = Math.min(previewLimitForMode("file-only", maxOutput), FILE_ONLY_CARD_PREVIEW_CHARS);
  const cards = results.map((result, index) => formatArtifactCard(mode, result, index, previewLimit));
  return formatBoundedArtifactSummary(
    fileOnlySummaryHeader(
      `Subagent ${mode} run ${artifactPlan.runId}: ${successCount(results)}/${results.length} succeeded`,
      artifactPlan,
    ),
    cards,
    readPreviews,
  );
}

function formatUnsuccessfulResultsLine(results: SingleResult[], maxOutput: number): string {
  const unsuccessful = results.filter(isUnsuccessfulResult);
  if (unsuccessful.length === 0) return "";
  const summaries = unsuccessful.map((result) => {
    const message = result.errorMessage || result.stderr || getFinalOutput(result.messages) || resultStatus(result);
    return `${result.agent} (${resultStatus(result)}): ${capText(message, maxOutput)}`;
  });
  return `\nUnsuccessful workers: ${summaries.join("; ")}`;
}

function formatSynthesisFileOnlySummary(
  artifactPlan: ArtifactPlan,
  workerResults: SingleResult[],
  synthesisResult: SingleResult,
  readPreviews: ReadPreview[],
  maxOutput: number,
  synthesisInputPath?: string,
): string {
  const previewLimit = Math.min(previewLimitForMode("file-only", maxOutput), FILE_ONLY_CARD_PREVIEW_CHARS);
  const synthesisCard = formatArtifactCard("parallel", synthesisResult, workerResults.length, previewLimit);
  synthesisCard.pathLines = ["Synthesis result:", ...synthesisCard.pathLines];
  const workerCards = workerResults.map((result, index) => formatArtifactCard("parallel", result, index, previewLimit));
  if (workerCards.length > 0) workerCards[0].pathLines = ["Worker artifacts:", ...workerCards[0].pathLines];

  return formatBoundedArtifactSummary(
    fileOnlySummaryHeader(
      `Subagent parallel synthesis run ${artifactPlan.runId}: synthesis ${resultStatus(synthesisResult)}; workers ${successCount(workerResults)}/${workerResults.length} succeeded`,
      artifactPlan,
      "Synthesis and worker outputs",
      synthesisInputPath ? [`Synthesis input: ${synthesisInputPath}`] : [],
    ),
    [synthesisCard, ...workerCards],
    readPreviews,
  );
}

async function writeFinalArtifactIndex(
  mode: "single" | "parallel" | "chain",
  artifactPlan: ArtifactPlan,
  results: SingleResult[],
  readPreviews: ReadPreview[],
  state: "succeeded" | "failed" | "canceled",
  warnings?: string[],
): Promise<void> {
  await writeRunArtifactIndex({
    plan: artifactPlan,
    mode,
    results: results.map(withDefaultResultPhase),
    state,
    readPreviews,
    warnings,
  });
}

function appendArtifactSummary(
  text: string,
  artifactPlan: ArtifactPlan,
  results: SingleResult[],
  readPreviews: ReadPreview[],
): string {
  const lines = [text.trimEnd(), "", `Artifacts: ${artifactPlan.artifactDir}`];
  for (const result of results) {
    const paths = formatArtifactPaths(result);
    if (paths) lines.push(`- [${result.agent}] ${paths}`);
  }
  return `${lines.join("\n")}${formatReadPreviewSection(readPreviews)}`;
}

function initialRunTasks(
  mode: "single" | "parallel" | "chain",
  artifactPlan: ArtifactPlan,
  params: any,
) {
  if (mode === "chain") {
    return params.chain.map((step: { agent: string; task: string; cwd?: string }, index: number) =>
      createRunTaskSummary(step.agent, step.task, index, {
        cwd: step.cwd ?? artifactPlan.cwd,
        step: index + 1,
        artifactDir: artifactPlan.artifactDir,
      }),
    );
  }
  if (mode === "parallel") {
    const workerTasks = params.tasks.map((task: { agent: string; task: string; cwd?: string }, index: number) =>
      createRunTaskSummary(task.agent, task.task, index, {
        cwd: task.cwd ?? artifactPlan.cwd,
        artifactDir: artifactPlan.artifactDir,
      }),
    );
    if (params.synthesizeWith) {
      const synthesizeWith = params.synthesizeWith as SynthesizeWith;
      const synthesisCwd = synthesizeWith.cwd ?? artifactPlan.cwd;
      workerTasks.push(
        createRunTaskSummary(synthesizeWith.agent, synthesizeWith.task, workerTasks.length, {
          cwd: synthesisCwd,
          artifactDir: artifactPlan.artifactDir,
        }),
      );
    }
    return workerTasks;
  }
  return [
    createRunTaskSummary(params.agent, params.task, 0, {
      cwd: params.cwd ?? artifactPlan.cwd,
      artifactDir: artifactPlan.artifactDir,
    }),
  ];
}

function finalRunStateFromResults(
  tracker: RunStateWriter,
  results: SingleResult[],
): "succeeded" | "failed" | "canceled" {
  const taskState = runStateForFinishedTasks(tracker.currentStatus.tasks);
  if (taskState === "canceled" || results.some(isCanceledResult)) return "canceled";
  if (taskState === "failed" || results.some(isFailedResult)) return "failed";
  return "succeeded";
}

async function finishRunFromResults(tracker: RunStateWriter, results: SingleResult[]): Promise<void> {
  await tracker.finish(finalRunStateFromResults(tracker, results));
}

async function failTrackedRun(tracker: RunStateWriter, error: unknown, signal: AbortSignal | undefined): Promise<void> {
  await tracker.finish(signal?.aborted ? "canceled" : "failed", error);
}

function captureParentSessionId(ctx: unknown): string | undefined {
  try {
    const value = (ctx as { sessionManager?: { getSessionId?: () => unknown } })
      .sessionManager
      ?.getSessionId?.();
    return typeof value === "string" && value.trim() ? value : undefined;
  } catch {
    return undefined;
  }
}

function liveActivityCallback(tracker: RunStateWriter, index: number): ChildActivityCallback {
  return (activity) => {
    void tracker.childActivity(index, activity).catch(() => {});
  };
}

function formatModelMetadata(
  model: string | undefined,
  themeFg: (color: any, text: string) => string,
): string {
  return model ? themeFg("muted", ` · ${model}`) : "";
}

function formatToolCall(
	toolName: string,
	args: Record<string, unknown>,
	themeFg: (color: any, text: string) => string,
): string {
	const shortenPath = (p: string) => {
		const home = os.homedir();
		return p.startsWith(home) ? `~${p.slice(home.length)}` : p;
	};

	switch (toolName) {
		case "bash": {
			const command = (args.command as string) || "...";
			const preview = command.length > 60 ? `${command.slice(0, 60)}...` : command;
			return themeFg("muted", "$ ") + themeFg("toolOutput", preview);
		}
		case "read": {
			const rawPath = (args.file_path || args.path || "...") as string;
			const filePath = shortenPath(rawPath);
			const offset = args.offset as number | undefined;
			const limit = args.limit as number | undefined;
			let text = themeFg("accent", filePath);
			if (offset !== undefined || limit !== undefined) {
				const startLine = offset ?? 1;
				const endLine = limit !== undefined ? startLine + limit - 1 : "";
				text += themeFg("warning", `:${startLine}${endLine ? `-${endLine}` : ""}`);
			}
			return themeFg("muted", "read ") + text;
		}
		case "write": {
			const rawPath = (args.file_path || args.path || "...") as string;
			const filePath = shortenPath(rawPath);
			const content = (args.content || "") as string;
			const lines = content.split("\n").length;
			let text = themeFg("muted", "write ") + themeFg("accent", filePath);
			if (lines > 1) text += themeFg("dim", ` (${lines} lines)`);
			return text;
		}
		case "edit": {
			const rawPath = (args.file_path || args.path || "...") as string;
			return themeFg("muted", "edit ") + themeFg("accent", shortenPath(rawPath));
		}
		case "ls": {
			const rawPath = (args.path || ".") as string;
			return themeFg("muted", "ls ") + themeFg("accent", shortenPath(rawPath));
		}
		case "find": {
			const pattern = (args.pattern || "*") as string;
			const rawPath = (args.path || ".") as string;
			return themeFg("muted", "find ") + themeFg("accent", pattern) + themeFg("dim", ` in ${shortenPath(rawPath)}`);
		}
		case "grep": {
			const pattern = (args.pattern || "") as string;
			const rawPath = (args.path || ".") as string;
			return (
				themeFg("muted", "grep ") +
				themeFg("accent", `/${pattern}/`) +
				themeFg("dim", ` in ${shortenPath(rawPath)}`)
			);
		}
		default: {
			const argsStr = JSON.stringify(args);
			const preview = argsStr.length > 50 ? `${argsStr.slice(0, 50)}...` : argsStr;
			return themeFg("accent", toolName) + themeFg("dim", ` ${preview}`);
		}
	}
}

const TaskItem = Type.Object({
	agent: Type.String({ description: "Name of the agent to invoke" }),
	task: Type.String({ description: "Task to delegate to the agent" }),
	cwd: Type.Optional(Type.String({ description: "Working directory for the agent process" })),
	writes: Type.Optional(
		Type.Boolean({ description: "Declare this parallel task's write intent. writes:false is honored only for declared read/search-only agents unless allowParallelWrites is true; default/unknown/mutating agents stay guarded." }),
	),
});

const SynthesizeWithItem = Type.Object({
  agent: Type.String({ description: "Name of the synthesizer agent to invoke after all parallel workers complete" }),
  task: Type.String({ description: "Synthesis task. Use {results} placeholder for formatted worker outputs, or omit to append them automatically" }),
  cwd: Type.Optional(Type.String({ description: "Working directory for the synthesizer agent process" })),
  maxInputChars: Type.Optional(
    Type.Number({ description: `Maximum characters for formatted worker outputs. Default: ${DEFAULT_SYNTHESIS_INPUT_CHARS}` }),
  ),
});

const ChainItem = Type.Object({
	agent: Type.String({ description: "Name of the agent to invoke" }),
	task: Type.String({ description: "Task with optional {previous} placeholder for prior output" }),
	cwd: Type.Optional(Type.String({ description: "Working directory for the agent process" })),
});

const AgentScopeSchema = StringEnum(["user", "project", "both"] as const, {
	description: 'Which agent directories to use. Default: "user". Use "both" to include project-local agents.',
	default: "user",
});

const OutputModeSchema = StringEnum(["inline", "file-only"] as const, {
  description: 'How much child output to return to the parent. Default: "inline".',
  default: "inline",
});

const WorktreeCleanupPolicySchema = StringEnum(["always", "on-success", "never"] as const, {
  description:
    'Cleanup policy for worktree: true parallel mode. Default: "always" captures patches then removes worktrees. "on-success" keeps failed child worktrees. "never" keeps all managed worktrees/branches.',
  default: "always",
});

const ActionSchema = StringEnum(["run", "status", "doctor", "prune", "apply"] as const, {
  description: 'Action to perform. Default: "run" delegates work; "status" reads durable run state scoped to the tool cwd; "doctor" runs diagnostics; "prune" previews or removes stale run-state scoped to the tool cwd; "apply" checks or applies captured worktree patch artifacts.',
  default: "run",
});

const WorktreeSetupSchema = StringEnum(["none", "node-modules"] as const, {
  description: 'Optional setup for managed worktrees. "node-modules" symlinks the base checkout node_modules into each mutating worktree. Requires worktree: true.',
  default: "none",
});

const SubagentParams = Type.Object({
  action: Type.Optional(ActionSchema),
  runId: Type.Optional(Type.String({ description: "Run ID to inspect for status, prune/apply target metadata, or check/apply when action is apply." })),
  limit: Type.Optional(Type.Number({ description: "Maximum recent runs to list for status. Default: 10." })),
  olderThanDays: Type.Optional(Type.Number({ description: "Prune runs older than this many days. Default: 14." })),
  dryRun: Type.Optional(Type.Boolean({ description: "Preview prune actions without deleting anything. Default: true.", default: true })),
  includeRunning: Type.Optional(Type.Boolean({ description: "Allow prune to remove stale runs still marked running. Default: false.", default: false })),
  pruneWorktrees: Type.Optional(Type.Boolean({ description: "Attempt cleanup of managed worktrees/branches recorded in stale run status. Default: true.", default: true })),
  taskIds: Type.Optional(
    Type.Array(Type.String(), {
      description: 'action: "apply" selector: task IDs from status.json, e.g. task-01. Mutually exclusive with taskIndexes and all.',
    }),
  ),
  taskIndexes: Type.Optional(
    Type.Array(Type.Number(), {
      description: 'action: "apply" selector: zero-based task indexes from status.json. Mutually exclusive with taskIds and all.',
    }),
  ),
  all: Type.Optional(
    Type.Boolean({
      description: 'action: "apply" selector: select all tasks with captured worktree patches. Mutually exclusive with taskIds and taskIndexes.',
      default: false,
    }),
  ),
  apply: Type.Optional(
    Type.Boolean({
      description: 'action: "apply" mode: false checks patches only; true applies them after all checks pass. Default: false.',
      default: false,
    }),
  ),
  threeWay: Type.Optional(
    Type.Boolean({
      description: 'action: "apply" mode: pass --3way to git apply check/apply. Default: false.',
      default: false,
    }),
  ),
	agent: Type.Optional(Type.String({ description: "Name of the agent to invoke (for single mode)" })),
	task: Type.Optional(Type.String({ description: "Task to delegate (for single mode)" })),
	tasks: Type.Optional(Type.Array(TaskItem, { description: "Array of {agent, task} for parallel execution" })),
	chain: Type.Optional(Type.Array(ChainItem, { description: "Array of {agent, task} for sequential execution" })),
	synthesizeWith: Type.Optional(SynthesizeWithItem),
	agentScope: Type.Optional(AgentScopeSchema),
	confirmProjectAgents: Type.Optional(
		Type.Boolean({ description: "Prompt before running project-local agents. Default: true.", default: true }),
	),
	allowParallelWrites: Type.Optional(
		Type.Boolean({
			description: "Allow mutating parallel agents in the same git checkout and let explicit writes:false opt out of conservative mutating classifications. Default: false.",
			default: false,
		}),
	),
  worktree: Type.Optional(
    Type.Boolean({
      description: "Create managed git worktrees for mutating parallel tasks. V1 supports parallel mode only. Default: false.",
      default: false,
    }),
  ),
  worktreeSetup: Type.Optional(WorktreeSetupSchema),
  worktreeSetupHook: Type.Optional(
    Type.String({
      description:
        "Repo-relative executable setup hook for each managed mutating worktree. Receives JSON on stdin and may return { syntheticPaths: string[] } JSON on stdout. Requires worktree: true.",
    }),
  ),
  worktreeSetupHookTimeoutMs: Type.Optional(
    Type.Number({
      description: `Timeout for worktreeSetupHook in milliseconds. Default: ${DEFAULT_WORKTREE_SETUP_HOOK_TIMEOUT_MS}. Requires worktree: true.`,
    }),
  ),
  worktreeCleanup: Type.Optional(WorktreeCleanupPolicySchema),
  cwd: Type.Optional(Type.String({ description: "Working directory for child execution and apply target checkout. Status and prune are scoped to the current tool context and ignore this parameter." })),
  output: Type.Optional(
    Type.String({
      description:
        "Optional artifact output path relative to cwd. Single mode may use a file path; parallel/chain require a directory path.",
    }),
  ),
  outputMode: Type.Optional(OutputModeSchema),
  reads: Type.Optional(
    Type.Array(Type.String(), {
      description:
        "Parent-facing paths to preview in the final summary. This is not a permission model for the child agent.",
    }),
  ),
  maxOutput: Type.Optional(
    Type.Number({
      description: `Maximum inline child output/preview characters per result. Default: ${DEFAULT_MAX_OUTPUT_CHARS}.`,
    }),
  ),
  timeoutMs: Type.Optional(
    Type.Number({ description: `Hard timeout per subagent run in milliseconds. Default: ${DEFAULT_HARD_TIMEOUT_MS}.` }),
  ),
  idleTimeoutMs: Type.Optional(
    Type.Number({
      description: `Idle timeout per subagent run in milliseconds with no stdout/stderr output. Default: ${DEFAULT_IDLE_TIMEOUT_MS}.`,
    }),
  ),
  model: Type.Optional(
    Type.String({
      description: 'Model override for this run, e.g. "openai/gpt-5.6-luna:xhigh". Pass "inherit" to use the parent session\'s live model. Wins over the agent\'s frontmatter model; a recognized terminal thinking suffix also wins over the agent\'s frontmatter thinking. Blank values are treated as omitted.',
    }),
  ),
});

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "subagent",
		label: "Subagent",
		description: [
			"Delegate tasks to specialized subagents with isolated context.",
			'Actions: run (default), status, doctor, prune, or apply; status/prune are scoped to the current tool cwd.',
			"Modes: single (agent + task), parallel (tasks array), chain (sequential with {previous} placeholder).",
			"Parallel mode rejects mutating workers sharing one git checkout unless allowParallelWrites is true.",
      "writes:false is honored only for declared read/search-only agents unless allowParallelWrites is true.",
      "Set worktree: true to create managed git worktrees for mutating parallel workers; worktreeSetup can prep dependencies and worktreeCleanup controls whether finished worktrees are removed or kept.",
			"Optional synthesizeWith for parallel mode: run one synthesizer agent after workers complete to reduce/aggregate their outputs.",
			'Default agent scope is "user" (from ~/.pi/agent/agents).',
			'To enable project-local agents in .pi/agents, set agentScope: "both" (or "project").',
		].join(" "),
		parameters: SubagentParams,

		async execute(_toolCallId, params, signal, onUpdate, ctx) {
      const action: "run" | "status" | "doctor" | "prune" | "apply" = params.action ?? "run";
			const agentScope: AgentScope = params.agentScope ?? "user";
      const toolCwd = path.resolve(ctx.cwd);
      const actionDetails: SubagentDetails = { mode: "single", agentScope, projectAgentsDir: null, results: [] };

      if (action === "status") {
        if (params.runId?.trim()) {
          try {
            const status = await readRunStatus(toolCwd, params.runId.trim());
            return { content: [{ type: "text" as const, text: formatRunStatus(status) }], details: actionDetails };
          } catch (error) {
            return {
              content: [{ type: "text" as const, text: error instanceof Error ? error.message : String(error) }],
              details: actionDetails,
              isError: true,
            };
          }
        }

        const recent = await listRecentRuns(toolCwd, params.limit);
        return { content: [{ type: "text" as const, text: formatRecentRuns(recent) }], details: actionDetails };
      }

      if (action === "prune") {
        const result = await pruneStaleRuns({
          cwd: toolCwd,
          olderThanDays: params.olderThanDays,
          dryRun: params.dryRun,
          includeRunning: params.includeRunning,
          pruneWorktrees: params.pruneWorktrees,
        });
        return {
          content: [{ type: "text" as const, text: formatPruneStaleRunsResult(result) }],
          details: actionDetails,
          isError: result.failedCount > 0,
        };
      }

      if (action === "doctor") {
        const report = await runSubagentDoctor({ cwd: toolCwd, agentScope });
        return {
          content: [{ type: "text" as const, text: formatDoctorReport(report) }],
          details: actionDetails,
          isError: !report.ok,
        };
      }

      if (action === "apply") {
        const applyCwd = resolveEffectiveTaskCwd(toolCwd, params.cwd);
        try {
          const result = await applyWorktreePatches({
            cwd: applyCwd,
            runId: params.runId,
            taskIds: params.taskIds,
            taskIndexes: params.taskIndexes,
            all: params.all,
            apply: params.apply,
            threeWay: params.threeWay,
          });
          return {
            content: [{ type: "text" as const, text: formatPatchApplyResult(result) }],
            details: { ...actionDetails, actionDetails: patchApplyActionDetails(result) },
          };
        } catch (error) {
          return {
            content: [{ type: "text" as const, text: error instanceof Error ? error.message : String(error) }],
            details: { ...actionDetails, actionDetails: patchApplyErrorActionDetails(error, applyCwd, params) },
            isError: true,
          };
        }
      }

			const confirmProjectAgents = params.confirmProjectAgents ?? true;
      const parentSessionId = captureParentSessionId(ctx);
      const outputMode: OutputMode = params.outputMode ?? "inline";
      const maxOutput = resolveMaxOutput(params.maxOutput);
      const lifecycleOptions = {
        timeoutMs: params.timeoutMs,
        idleTimeoutMs: params.idleTimeoutMs,
        parentModel: ctx.model,
        modelOverride: params.model,
      };
      const worktreeMode = params.worktree ?? false;
      const normalizedParams = normalizeSubagentParams(params);
      const {
        worktreeSetup,
        worktreeSetupHook,
        worktreeSetupHookTimeoutMs,
        worktreeCleanupPolicy,
        hasWorktreeSetupOptions,
        hasWorktreeCleanupOption,
        synthesizeWith,
      } = normalizedParams;

			const hasChain = (params.chain?.length ?? 0) > 0;
			const hasTasks = (params.tasks?.length ?? 0) > 0;
			const hasSingle = Boolean(params.agent && params.task);
			const hasSynthesize = Boolean(synthesizeWith);
			const modeCount = Number(hasChain) + Number(hasTasks) + Number(hasSingle);
      const runCwd = resolveEffectiveTaskCwd(toolCwd, params.cwd);
      const subagentConfig = loadSubagentConfig(toolCwd);
      const discovery = discoverAgents(runCwd, agentScope);
			const agents = discovery.agents;
      const disabledSet = new Set(subagentConfig.disabledAgents);
      const enabledAgents = agents.filter((agent) => !disabledSet.has(agent.name));
      let activeArtifactPlan: ArtifactPlan | undefined;
      type DetailsMetadata = Partial<Pick<SubagentDetails, "workflow" | "synthesisResultIndex" | "rejectedAgents">>;
      const mergeWarnings = (warnings?: string[]): string[] => [
        ...subagentConfig.warnings,
        ...(warnings ?? []),
      ];

			const makeDetails =
				(mode: "single" | "parallel" | "chain") =>
				(results: SingleResult[], warnings?: string[], metadata?: DetailsMetadata): SubagentDetails => {
          const mergedWarnings = mergeWarnings(warnings);
          return {
					mode,
					agentScope,
					projectAgentsDir: discovery.projectAgentsDir,
					results: results.map(withDefaultResultPhase),
            outputMode,
            ...(activeArtifactPlan ? { runId: activeArtifactPlan.runId, artifactDir: activeArtifactPlan.artifactDir } : {}),
            ...(mergedWarnings.length > 0 ? { warnings: mergedWarnings } : {}),
            ...(subagentConfig.disabledAgents.length > 0 ? { disabledAgents: subagentConfig.disabledAgents } : {}),
            ...(metadata ?? {}),
				  };
        };
      const makeParentDetails =
        (mode: "single" | "parallel" | "chain", warnings?: string[], metadata?: DetailsMetadata) =>
        (results: SingleResult[]): SubagentDetails =>
          makeDetails(mode)(resultsForParentDetails(results, outputMode), warnings, metadata);

      if (hasWorktreeSetupOptions && !worktreeMode) {
        return {
          content: [
            {
              type: "text",
              text: "worktreeSetup, worktreeSetupHook, and worktreeSetupHookTimeoutMs require worktree: true.",
            },
          ],
          details: makeDetails("single")([]),
          isError: true,
        };
      }

      if (hasSynthesize && !hasTasks) {
        return {
          content: [{ type: "text", text: "synthesizeWith is only valid with tasks (parallel mode). Remove synthesizeWith or use tasks: [...]." }],
          details: makeDetails("single")([]),
          isError: true,
        };
      }

			if (modeCount !== 1) {
				const available = enabledAgents.map((a) => `${a.name} (${a.source})`).join(", ") || "none";
				return {
					content: [
						{
							type: "text",
							text: `Invalid parameters. Provide exactly one mode.\nAvailable agents: ${available}`,
						},
					],
					details: makeDetails("single")([]),
          isError: true,
				};
			}

      const mode: "single" | "parallel" | "chain" = hasChain ? "chain" : hasTasks ? "parallel" : "single";
      if (worktreeMode && mode !== "parallel") {
        return {
          content: [
            {
              type: "text",
              text: `Managed worktrees are only supported in parallel mode. Current mode: ${mode}. Remove worktree: true or use tasks: [...] parallel mode.`,
            },
          ],
          details: makeDetails(mode)([]),
          isError: true,
        };
      }
      if (!worktreeMode && hasWorktreeCleanupOption) {
        return {
          content: [
            {
              type: "text",
              text: "worktreeCleanup only applies when worktree: true is enabled for parallel mode.",
            },
          ],
          details: makeDetails(mode)([]),
          isError: true,
        };
      }

      if (worktreeMode && worktreeSetup !== "none" && worktreeSetup !== "node-modules") {
        return {
          content: [{ type: "text", text: `Unsupported worktreeSetup: ${String(worktreeSetup)}` }],
          details: makeDetails(mode)([]),
          isError: true,
        };
      }

      const resolvedTasks = params.tasks?.map((task: { agent: string; task: string; cwd?: string; writes?: boolean }) => ({
        ...task,
        cwd: resolveEffectiveTaskCwd(runCwd, task.cwd),
      }));
      const resolvedChain = params.chain?.map((step: { agent: string; task: string; cwd?: string }) => ({
        ...step,
        cwd: resolveEffectiveTaskCwd(runCwd, step.cwd),
      }));
      const resolvedSynthesizeWith = synthesizeWith
        ? { ...synthesizeWith, cwd: resolveEffectiveTaskCwd(runCwd, synthesizeWith.cwd) }
        : undefined;
      if (resolvedTasks && resolvedTasks.length > MAX_PARALLEL_TASKS)
        return {
          content: [
            {
              type: "text",
              text: `Too many parallel tasks (${resolvedTasks.length}). Max is ${MAX_PARALLEL_TASKS}.`,
            },
          ],
          details: makeDetails("parallel")([]),
          isError: true,
        };

      let artifactPlan: ArtifactPlan;
      try {
        artifactPlan = resolveArtifactPlan(runCwd, mode, params.output);
      } catch (error) {
        return {
          content: [{ type: "text", text: error instanceof Error ? error.message : String(error) }],
          details: makeDetails(mode)([]),
          isError: true,
        };
      }

      const requestedAgents: RequestedAgentRef[] = [];
      if (hasSingle && params.agent) requestedAgents.push({ name: params.agent, location: "agent" });
      resolvedTasks?.forEach((task, index) => requestedAgents.push({ name: task.agent, location: `tasks[${index}].agent` }));
      resolvedChain?.forEach((step, index) => requestedAgents.push({ name: step.agent, location: `chain[${index}].agent` }));
      if (resolvedSynthesizeWith) requestedAgents.push({ name: resolvedSynthesizeWith.agent, location: "synthesizeWith.agent" });

      if (subagentConfig.errors.length > 0) {
        return {
          content: [{ type: "text", text: `Invalid subagent.disabledAgents configuration:\n${subagentConfig.errors.join("\n")}` }],
          details: makeDetails(mode)([]),
          isError: true,
        };
      }

      const rejectedAgents = findRejectedDisabledAgents(requestedAgents, subagentConfig.disabledAgents);
      if (rejectedAgents.length > 0) {
        return {
          content: [{ type: "text", text: formatDisabledAgentsError(rejectedAgents, subagentConfig.disabledAgents) }],
          details: makeDetails(mode)([], undefined, { rejectedAgents }),
          isError: true,
        };
      }

      activeArtifactPlan = artifactPlan;
      const artifactPromptMode = outputMode === "file-only" || Boolean(params.output);

			if (agentScope === "project" || agentScope === "both") {
				const requestedAgentNames = new Set(requestedAgents.map((ref) => ref.name));

				const projectAgentsRequested = Array.from(requestedAgentNames)
					.map((name) => enabledAgents.find((a) => a.name === name))
					.filter((a): a is AgentConfig => a?.source === "project");

				if (confirmProjectAgents && projectAgentsRequested.length > 0) {
					const names = projectAgentsRequested.map((a) => a.name).join(", ");
					const dir = discovery.projectAgentsDir ?? "(unknown)";
          if (!ctx.hasUI) {
            return {
              content: [
                {
                  type: "text",
                  text: `Project-local agents require confirmation, but this subagent call is running without UI. Agents: ${names}. Source: ${dir}. Re-run with confirmProjectAgents: false only for trusted repositories.`,
                },
              ],
              details: makeDetails(mode)([]),
              isError: true,
            };
          }

					const ok = await ctx.ui.confirm(
						"Run project-local agents?",
						`Agents: ${names}\nSource: ${dir}\n\nProject agents are repo-controlled. Only continue for trusted repositories.`,
					);
					if (!ok)
						return {
							content: [{ type: "text", text: "Canceled: project-local agents not approved." }],
							details: makeDetails(mode)([]),
						};
				}
			}

			if (resolvedChain && resolvedChain.length > 0) {
        const tracker = await RunStateWriter.start({
          runId: artifactPlan.runId,
          mode: "chain",
          cwd: artifactPlan.cwd,
          agentScope,
          artifactDir: artifactPlan.artifactDir,
          tasks: initialRunTasks("chain", artifactPlan, { ...params, chain: resolvedChain }),
          warnings: subagentConfig.warnings,
          disabledAgents: subagentConfig.disabledAgents,
          parentSessionId,
        });
				const results: SingleResult[] = [];
				let previousOutput = "";

        try {
					for (let i = 0; i < resolvedChain.length; i++) {
						const step = resolvedChain[i];
            const paths = resultArtifactPaths(artifactPlan, step.agent, i + 1);
						const taskWithContext = step.task.replace(/\{previous\}/g, previousOutput);
            const taskForChild = withArtifactInstructions(taskWithContext, paths, artifactPromptMode);
            await tracker.childStarted(i, step.cwd);

						// Create update callback that includes all previous results
						const chainUpdate: OnUpdateCallback | undefined = onUpdate
							? (partial) => {
									// Combine completed results with current streaming result
									const currentResult = partial.details?.results[0];
									if (currentResult) {
										const allResults = [
                      ...results,
                      {
                        ...currentResult,
                        task: taskWithContext,
                        artifactDir: paths.artifactDir,
                        artifactResultPath: paths.resultPath,
                        artifactTranscriptPath: paths.transcriptPath,
                        artifactStderrPath: paths.stderrPath,
                        artifactSummaryPath: paths.summaryPath,
                      },
                    ];
                    const capped = capUpdateContent(onUpdate, outputMode, maxOutput, artifactPlan);
                    capped?.({
                      content: partial.content,
                      details: makeDetails("chain")(allResults),
                    });
									}
								}
							: undefined;

						let result = await runSingleAgent(
							step.cwd,
							enabledAgents,
							step.agent,
							taskForChild,
							undefined,
							i + 1,
							signal,
							chainUpdate,
							makeDetails("chain"),
              { ...lifecycleOptions, onActivity: liveActivityCallback(tracker, i) },
						);
            result = { ...result, task: taskWithContext };
            result = await writeResultArtifacts(result, paths);
            await tracker.artifactWritten(i, result);
            await tracker.childFinished(i, result);
						results.push(result);

						if (isUnsuccessfulResult(result)) {
							const errorMsg =
								result.errorMessage || result.stderr || getFinalOutput(result.messages) || "(no output)";
              const readPreviews = await buildReadPreviews(artifactPlan.cwd, params.reads, previewLimitForMode(outputMode, maxOutput));
              const finalState = finalRunStateFromResults(tracker, results);
              await writeFinalArtifactIndex("chain", artifactPlan, results, readPreviews, finalState);
              await finishRunFromResults(tracker, results);
              const text = outputMode === "file-only"
                ? formatFileOnlySummary("chain", artifactPlan, results, readPreviews, maxOutput)
                : appendArtifactSummary(
                    `Chain stopped at step ${i + 1} (${step.agent}): ${capText(errorMsg, maxOutput)}`,
                    artifactPlan,
                    results,
                    readPreviews,
                  );
							return {
								content: [{ type: "text", text }],
								details: makeParentDetails("chain")(results),
								isError: true,
							};
						}
						previousOutput = getFinalOutput(result.messages);
					}
          const readPreviews = await buildReadPreviews(artifactPlan.cwd, params.reads, previewLimitForMode(outputMode, maxOutput));
          const finalState = finalRunStateFromResults(tracker, results);
          await writeFinalArtifactIndex("chain", artifactPlan, results, readPreviews, finalState);
          await finishRunFromResults(tracker, results);
          const finalOutput = getFinalOutput(results[results.length - 1].messages) || "(no output)";
          const text = outputMode === "file-only"
            ? formatFileOnlySummary("chain", artifactPlan, results, readPreviews, maxOutput)
            : appendArtifactSummary(capText(finalOutput, maxOutput), artifactPlan, results, readPreviews);
					return {
						content: [{ type: "text", text }],
						details: makeParentDetails("chain")(results),
					};
        } catch (error) {
          await failTrackedRun(tracker, error, signal);
          throw error;
        }
			}

			if (resolvedTasks && resolvedTasks.length > 0) {
        const taskMutabilities = resolvedTasks.map((task) =>
          inferTaskMutability(enabledAgents.find((agent) => agent.name === task.agent), task.writes, params.allowParallelWrites ?? false),
        );

        let preparedWorktrees: PreparedWorktree[] = [];
        let tracker: RunStateWriter | undefined;
        if (worktreeMode) {
          for (let index = 0; index < resolvedTasks.length; index++) {
            const task = resolvedTasks[index];
            if (taskMutabilities[index].writes && task.cwd !== runCwd) {
              return {
                content: [
                  {
                    type: "text",
                    text: `Task ${index + 1} (${task.agent}) is mutating and has cwd=${task.cwd}. worktree: true manages cwd for mutating parallel tasks; remove task.cwd or run without worktree isolation.`,
                  },
                ],
                details: makeDetails("parallel")([]),
                isError: true,
              };
            }
          }

          tracker = await RunStateWriter.start({
            runId: artifactPlan.runId,
            mode: "parallel",
            cwd: artifactPlan.cwd,
            agentScope,
            artifactDir: artifactPlan.artifactDir,
            tasks: initialRunTasks("parallel", artifactPlan, { ...params, tasks: resolvedTasks, synthesizeWith: resolvedSynthesizeWith }),
            warnings: subagentConfig.warnings,
            disabledAgents: subagentConfig.disabledAgents,
            parentSessionId,
            deferArtifactStatusMirror: true,
          });

          try {
            preparedWorktrees = await prepareParallelWorktrees({
              runId: artifactPlan.runId,
              repoRoot: runCwd,
              artifactDir: artifactPlan.artifactDir,
              worktreeSetup,
              worktreeSetupHook,
              worktreeSetupHookTimeoutMs,
              signal,
              tasks: resolvedTasks.map((task, index) => ({
                agent: task.agent,
                task: task.task,
                writes: taskMutabilities[index].writes,
              })),
            });
            await tracker.enableArtifactStatusMirror();
          } catch (error) {
            if (preparedWorktrees.length > 0) await cleanupPreparedWorktreesAfterError(preparedWorktrees, artifactPlan.artifactDir);
            await failTrackedRun(tracker, error, signal).catch(() => {});
            const message = error instanceof WorktreeError ? error.message : error instanceof Error ? error.message : String(error);
            return {
              content: [{ type: "text", text: `Worktree preparation failed: ${message}` }],
              details: makeDetails("parallel")([]),
              isError: true,
            };
          }
        }

				let parallelSafetyWarnings: string[] = [];
        if (!worktreeMode) {
					try {
						const parallelSafety = await validateParallelTasks({
							defaultCwd: runCwd,
							tasks: resolvedTasks,
							agents: enabledAgents,
							allowParallelWrites: params.allowParallelWrites ?? false,
						});
						parallelSafetyWarnings = parallelSafety.warnings;
					} catch (error) {
						if (error instanceof ParallelWriteSafetyError) {
							return {
								content: [{ type: "text", text: error.message }],
								details: makeDetails("parallel")([]),
								isError: true,
							};
						}
						throw error;
					}
        }

        const parallelWorkflowMetadata = (synthesisResultIndex?: number): DetailsMetadata | undefined =>
          resolvedSynthesizeWith
            ? {
                workflow: "parallel-synthesis",
                ...(synthesisResultIndex !== undefined ? { synthesisResultIndex } : {}),
              }
            : undefined;
				const makeParallelDetails = (results: SingleResult[], synthesisResultIndex?: number) =>
					makeDetails("parallel")(results, parallelSafetyWarnings, parallelWorkflowMetadata(synthesisResultIndex));
        const makeParallelParentDetails = (results: SingleResult[], synthesisResultIndex?: number) =>
          makeParentDetails("parallel", parallelSafetyWarnings, parallelWorkflowMetadata(synthesisResultIndex))(results);
        const worktreesByTaskIndex = new Map(preparedWorktrees.map((worktree) => [worktree.taskIndex, worktree]));
        const tasksWithEffectiveCwd = resolvedTasks.map((task, index) => ({
          ...task,
          cwd: worktreesByTaskIndex.get(index)?.taskCwd ?? task.cwd,
        }));
        const activeTracker = tracker ?? await RunStateWriter.start({
          runId: artifactPlan.runId,
          mode: "parallel",
          cwd: artifactPlan.cwd,
          agentScope,
          artifactDir: artifactPlan.artifactDir,
          tasks: initialRunTasks("parallel", artifactPlan, { ...params, tasks: tasksWithEffectiveCwd, synthesizeWith: resolvedSynthesizeWith }),
          warnings: mergeWarnings(parallelSafetyWarnings),
          disabledAgents: subagentConfig.disabledAgents,
          parentSessionId,
        });
        tracker = activeTracker;

				// Track all results for streaming updates
				const allResults: SingleResult[] = new Array(resolvedTasks.length);
        const parallelPaths = resolvedTasks.map((t, index) => resultArtifactPaths(artifactPlan, t.agent, index + 1));

				// Initialize placeholder results
				for (let i = 0; i < resolvedTasks.length; i++) {
          const paths = parallelPaths[i];
          const worktree = worktreesByTaskIndex.get(i);
          const agent = enabledAgents.find((candidate) => candidate.name === resolvedTasks[i].agent);
          const model = agent ? resolveEffectiveModel(agent, lifecycleOptions) : undefined;
					allResults[i] = {
						agent: resolvedTasks[i].agent,
						agentSource: "unknown",
            phase: "worker",
						task: resolvedTasks[i].task,
						exitCode: -1, // -1 = still running
						messages: [],
						stderr: "",
						usage: emptyUsageStats(),
            ...(model ? { model } : {}),
            artifactDir: paths.artifactDir,
            artifactResultPath: paths.resultPath,
            artifactTranscriptPath: paths.transcriptPath,
            artifactStderrPath: paths.stderrPath,
            artifactSummaryPath: paths.summaryPath,
            ...(worktree ? { worktree: worktreeMetadata(worktree, worktreeCleanupPolicy) } : {}),
					};
				}

				const emitParallelUpdate = () => {
					if (onUpdate) {
						const running = allResults.filter((r) => r.exitCode === -1).length;
						const done = allResults.filter((r) => r.exitCode !== -1).length;
            const capped = capUpdateContent(onUpdate, outputMode, maxOutput, artifactPlan);
            capped?.({
							content: [
								{ type: "text", text: `Parallel: ${done}/${allResults.length} done, ${running} running... Artifacts: ${artifactPlan.artifactDir}` },
							],
              details: makeParallelDetails([...allResults]),
						});
					}
				};

        const claimedWorktreeTaskIndexes = new Set<number>();
        try {
					const results = await mapWithConcurrencyLimit(resolvedTasks, MAX_CONCURRENCY, async (t, index) => {
            const paths = parallelPaths[index];
            const taskForChild = withArtifactInstructions(t.task, paths, artifactPromptMode);
            const worktree = worktreesByTaskIndex.get(index);
            if (worktree) claimedWorktreeTaskIndexes.add(index);
            const effectiveCwd = worktree?.taskCwd ?? t.cwd;
            let childWasStartedInWorktree = false;
            let finalizedWorktreeResult: SingleResult | undefined;
						try {
              await activeTracker.childStarted(index, effectiveCwd);
              childWasStartedInWorktree = Boolean(worktree);
							let result = await runSingleAgent(
								effectiveCwd,
								enabledAgents,
								t.agent,
								taskForChild,
								undefined,
								undefined,
								signal,
								// Per-task update callback
								(partial) => {
									if (partial.details?.results[0]) {
										const partialResult = partial.details.results[0];
                    allResults[index] = {
                      ...partialResult,
                      phase: "worker",
                      exitCode: partialResult.stopReason && partialResult.stopReason !== "toolUse" ? partialResult.exitCode : -1,
                      task: t.task,
                      artifactDir: paths.artifactDir,
                      artifactResultPath: paths.resultPath,
                      artifactTranscriptPath: paths.transcriptPath,
                      artifactStderrPath: paths.stderrPath,
                      artifactSummaryPath: paths.summaryPath,
                      ...(worktree ? { worktree: worktreeMetadata(worktree, worktreeCleanupPolicy) } : {}),
                    };
										emitParallelUpdate();
									}
								},
								makeParallelDetails,
                  { ...lifecycleOptions, onActivity: liveActivityCallback(activeTracker, index) },
							);
              result = withResultPhase({ ...result, task: t.task }, "worker");
              result = await finalizeWorktreeResult(result, worktree, artifactPlan.artifactDir, worktreeCleanupPolicy);
              finalizedWorktreeResult = result;
              result = await writeResultArtifacts(result, paths);
              finalizedWorktreeResult = result;
              await activeTracker.artifactWritten(index, result);
              await activeTracker.childFinished(index, result);
							allResults[index] = result;
							emitParallelUpdate();
							return result;
            } catch (error) {
              if (worktree && childWasStartedInWorktree) {
                try {
                  let failedResult = finalizedWorktreeResult
                    ? {
                        ...finalizedWorktreeResult,
                        exitCode: 1,
                        stopReason: signal?.aborted ? "aborted" : "error",
                        errorMessage: finalizedWorktreeResult.errorMessage
                          ? `${finalizedWorktreeResult.errorMessage}; post-run bookkeeping failed: ${errorMessage(error)}`
                          : errorMessage(error),
                      }
                    : await finalizeWorktreeResult(
                        worktreeErrorResult(t, paths, error, signal),
                        worktree,
                        artifactPlan.artifactDir,
                        worktreeCleanupPolicy,
                      );
                  failedResult = await writeResultArtifacts(failedResult, paths);
                  await activeTracker.artifactWritten(index, failedResult);
                  await activeTracker.childFinished(index, failedResult);
                  allResults[index] = failedResult;
                  emitParallelUpdate();
                } catch {
                  // Preserve the original child/run failure; best-effort recovery metadata can fail separately.
                }
              } else if (worktree) {
                await cleanupPreparedWorktreesAfterError([worktree], artifactPlan.artifactDir);
              }
              throw error;
            }
					});

          // Run optional synthesis step
          const synthesizeWith = resolvedSynthesizeWith as SynthesizeWith | undefined;
          let synthesisResult: SingleResult | undefined;
          let synthesisSkipReason: string | undefined;
          let synthesisResultIndex: number | undefined;
          let synthesisInputPath: string | undefined;
          if (synthesizeWith) {
            const synthesisCwd = synthesizeWith.cwd ?? runCwd;
            const synthesisIndex = results.length;
            const synthesisPaths = resultArtifactPaths(artifactPlan, synthesizeWith.agent, synthesisIndex + 1);
            const hasSuccessfulWorker = results.some((r) => !isUnsuccessfulResult(r));

            if (!hasSuccessfulWorker) {
              synthesisSkipReason = "Synthesis skipped because no parallel workers completed successfully.";
              await activeTracker.childSkipped(synthesisIndex, synthesisSkipReason);
            } else if (signal?.aborted) {
              let synth = synthesisFailureResult(
                synthesizeWith,
                synthesizeWith.task,
                synthesisPaths,
                "Synthesis canceled before start because the run was aborted.",
                "aborted",
              );
              synth = await writeResultArtifacts(synth, synthesisPaths);
              await activeTracker.artifactWritten(synthesisIndex, synth);
              await activeTracker.childFinished(synthesisIndex, synth);
              synthesisResult = synth;
              synthesisResultIndex = synthesisIndex;
              results.push(synth);
            } else {
              const synthesisTask = buildSynthesisTask(synthesizeWith.task, results, synthesizeWith.maxInputChars);
              const synthesisTaskForChild = withArtifactInstructions(synthesisTask, synthesisPaths, artifactPromptMode);
              synthesisInputPath = synthesisInputArtifactPath(artifactPlan);
              await writeArtifactTextFile(synthesisInputPath, synthesisTaskForChild);

              await activeTracker.childStarted(synthesisIndex, synthesisCwd);

              let synth: SingleResult;
              try {
                synth = await runSingleAgent(
                  synthesisCwd,
                  enabledAgents,
                  synthesizeWith.agent,
                  synthesisTaskForChild,
                  undefined,
                  undefined,
                  signal,
                  onUpdate ? (partial) => {
                    if (partial.details?.results[0]) {
                      onUpdate({
                        content: [{ type: "text", text: `Synthesis running... Artifacts: ${artifactPlan.artifactDir}` }],
                        details: makeParallelParentDetails([
                          ...results,
                          {
                            ...partial.details.results[0],
                            phase: "synthesis",
                            task: synthesisTask,
                            artifactDir: synthesisPaths.artifactDir,
                            artifactResultPath: synthesisPaths.resultPath,
                            artifactTranscriptPath: synthesisPaths.transcriptPath,
                            artifactStderrPath: synthesisPaths.stderrPath,
                            artifactSummaryPath: synthesisPaths.summaryPath,
                          },
                        ], synthesisIndex),
                      });
                    }
                  } : undefined,
                  makeParallelDetails,
                  { ...lifecycleOptions, onActivity: liveActivityCallback(activeTracker, synthesisIndex) },
                );
                synth = withResultPhase({ ...synth, task: synthesisTask }, "synthesis");
              } catch (error) {
                const stopReason = signal?.aborted || isAbortLikeError(error) ? "aborted" : "error";
                synth = synthesisFailureResult(synthesizeWith, synthesisTask, synthesisPaths, error, stopReason);
              }

              synth = await writeResultArtifacts(synth, synthesisPaths);
              await activeTracker.artifactWritten(synthesisIndex, synth);
              await activeTracker.childFinished(synthesisIndex, synth);
              synthesisResult = synth;
              synthesisResultIndex = synthesisIndex;
              results.push(synth);
            }
          }

          const readPreviews = await buildReadPreviews(artifactPlan.cwd, params.reads, previewLimitForMode(outputMode, maxOutput));
          const finalState = finalRunStateFromResults(activeTracker, results);
          await writeFinalArtifactIndex(
            "parallel",
            artifactPlan,
            results,
            readPreviews,
            finalState,
            parallelSafetyWarnings,
          );
          await finishRunFromResults(activeTracker, results);
          const hasUnsuccessfulResult = results.some(isUnsuccessfulResult);
          const summaries = results.map((r) => {
            const detail = isUnsuccessfulResult(r)
              ? r.errorMessage || r.stderr.trim() || getFinalOutput(r.messages) || resultStatus(r)
              : getFinalOutput(r.messages) || "(no output)";
            return `[${r.agent}] ${resultStatus(r)}:\n${capText(detail, maxOutput)}`;
          });
          const workerResults = synthesisResultIndex === undefined
            ? results
            : results.filter((_result, index) => index !== synthesisResultIndex);
          const details = makeParallelParentDetails(results, synthesisResultIndex);
          if (hasUnsuccessfulResult && results.some((result) => !isUnsuccessfulResult(result))) details.partialFailure = true;
          const synthesisOutput = synthesisResult
            ? getFinalOutput(synthesisResult.messages) || synthesisResult.errorMessage || "(no output)"
            : undefined;
          const baseText = outputMode === "file-only"
            ? synthesisResult
              ? formatSynthesisFileOnlySummary(artifactPlan, workerResults, synthesisResult, readPreviews, maxOutput, synthesisInputPath)
              : formatFileOnlySummary("parallel", artifactPlan, results, readPreviews, maxOutput)
            : appendArtifactSummary(
                synthesisResult
                  ? `Synthesis (${synthesisResult.agent}):\n${capText(synthesisOutput ?? "(no output)", maxOutput)}\n\nWorker results: ${successCount(workerResults)}/${workerResults.length} succeeded${formatUnsuccessfulResultsLine(workerResults, maxOutput)}`
                  : `Parallel: ${successCount(results)}/${results.length} succeeded\n\n${summaries.join("\n\n")}`,
                artifactPlan,
                results,
                readPreviews,
              );
          const warningText = parallelSafetyWarnings.length > 0 ? `${parallelSafetyWarnings.join("\n\n")}\n\n` : "";
          const skipText = synthesisSkipReason ? `\n\n${synthesisSkipReason}` : "";
          return {
            content: [{ type: "text", text: `${warningText}${baseText}${skipText}` }],
            details,
            ...(hasUnsuccessfulResult ? { isError: true } : {}),
          };
        } catch (error) {
          const unstartedWorktrees = preparedWorktrees.filter((worktree) => !claimedWorktreeTaskIndexes.has(worktree.taskIndex));
          if (unstartedWorktrees.length > 0) await cleanupPreparedWorktreesAfterError(unstartedWorktrees, artifactPlan.artifactDir);
          await failTrackedRun(activeTracker, error, signal);
          throw error;
        }
			}

			if (params.agent && params.task) {
        const tracker = await RunStateWriter.start({
          runId: artifactPlan.runId,
          mode: "single",
          cwd: artifactPlan.cwd,
          agentScope,
          artifactDir: artifactPlan.artifactDir,
          tasks: initialRunTasks("single", artifactPlan, { ...params, cwd: runCwd }),
          warnings: subagentConfig.warnings,
          disabledAgents: subagentConfig.disabledAgents,
          parentSessionId,
        });
        const paths = resultArtifactPaths(artifactPlan, params.agent);
        const taskForChild = withArtifactInstructions(params.task, paths, artifactPromptMode);
        try {
          await tracker.childStarted(0, runCwd);
					let result = await runSingleAgent(
						runCwd,
						enabledAgents,
						params.agent,
						taskForChild,
						undefined,
						undefined,
						signal,
            capUpdateContent(onUpdate, outputMode, maxOutput, artifactPlan),
						makeDetails("single"),
            { ...lifecycleOptions, onActivity: liveActivityCallback(tracker, 0) },
					);
          result = { ...result, task: params.task };
          result = await writeResultArtifacts(result, paths);
          await tracker.artifactWritten(0, result);
          await tracker.childFinished(0, result);
          const readPreviews = await buildReadPreviews(artifactPlan.cwd, params.reads, previewLimitForMode(outputMode, maxOutput));
          const finalState = finalRunStateFromResults(tracker, [result]);
          await writeFinalArtifactIndex("single", artifactPlan, [result], readPreviews, finalState);
          await finishRunFromResults(tracker, [result]);
					if (isUnsuccessfulResult(result)) {
						const errorMsg =
							result.errorMessage || result.stderr || getFinalOutput(result.messages) || "(no output)";
            const text = outputMode === "file-only"
              ? formatFileOnlySummary("single", artifactPlan, [result], readPreviews, maxOutput)
              : appendArtifactSummary(
                  `Agent ${result.stopReason || "failed"}: ${capText(errorMsg, maxOutput)}`,
                  artifactPlan,
                  [result],
                  readPreviews,
                );
						return {
							content: [{ type: "text", text }],
							details: makeParentDetails("single")([result]),
							isError: true,
						};
					}
          const finalOutput = getFinalOutput(result.messages) || "(no output)";
          const text = outputMode === "file-only"
            ? formatFileOnlySummary("single", artifactPlan, [result], readPreviews, maxOutput)
            : appendArtifactSummary(capText(finalOutput, maxOutput), artifactPlan, [result], readPreviews);
					return {
						content: [{ type: "text", text }],
						details: makeParentDetails("single")([result]),
					};
        } catch (error) {
          await failTrackedRun(tracker, error, signal);
          throw error;
        }
			}

			const available = enabledAgents.map((a) => `${a.name} (${a.source})`).join(", ") || "none";
			return {
				content: [{ type: "text", text: `Invalid parameters. Available agents: ${available}` }],
				details: makeDetails("single")([]),
        isError: true,
			};
		},

		renderCall(args, theme, _context) {
			const scope: AgentScope = args.agentScope ?? "user";
      if (args.action === "status") {
        const target = args.runId ? ` ${args.runId}` : " recent";
        return new Text(theme.fg("toolTitle", theme.bold("subagent ")) + theme.fg("accent", `status${target}`), 0, 0);
      }
      if (args.action === "doctor") {
        return new Text(theme.fg("toolTitle", theme.bold("subagent ")) + theme.fg("accent", "doctor") + theme.fg("muted", ` [${scope}]`), 0, 0);
      }
      if (args.action === "prune") {
        const dryRun = args.dryRun === false ? "apply" : "dry-run";
        return new Text(theme.fg("toolTitle", theme.bold("subagent ")) + theme.fg("accent", `prune ${dryRun}`), 0, 0);
      }
      if (args.action === "apply") {
        const target = args.runId ? ` ${args.runId}` : "";
        const mode = args.apply ? "apply" : "check";
        return new Text(theme.fg("toolTitle", theme.bold("subagent ")) + theme.fg("accent", `${mode}${target}`), 0, 0);
      }
			if (args.chain && args.chain.length > 0) {
				let text =
					theme.fg("toolTitle", theme.bold("subagent ")) +
					theme.fg("accent", `chain (${args.chain.length} steps)`) +
					theme.fg("muted", ` [${scope}]`);
				for (let i = 0; i < Math.min(args.chain.length, 3); i++) {
					const step = args.chain[i];
					// Clean up {previous} placeholder for display
					const cleanTask = step.task.replace(/\{previous\}/g, "").trim();
					const preview = cleanTask.length > 40 ? `${cleanTask.slice(0, 40)}...` : cleanTask;
					text +=
						"\n  " +
						theme.fg("muted", `${i + 1}.`) +
						" " +
						theme.fg("accent", step.agent) +
						theme.fg("dim", ` ${preview}`);
				}
				if (args.chain.length > 3) text += `\n  ${theme.fg("muted", `... +${args.chain.length - 3} more`)}`;
				return new Text(text, 0, 0);
			}
			if (args.tasks && args.tasks.length > 0) {
				let text =
					theme.fg("toolTitle", theme.bold("subagent ")) +
					theme.fg("accent", `parallel (${args.tasks.length} tasks)`) +
					theme.fg("muted", ` [${scope}]`);
				for (const t of args.tasks.slice(0, 3)) {
					const preview = t.task.length > 40 ? `${t.task.slice(0, 40)}...` : t.task;
					text += `\n  ${theme.fg("accent", t.agent)}${theme.fg("dim", ` ${preview}`)}`;
				}
				if (args.tasks.length > 3) text += `\n  ${theme.fg("muted", `... +${args.tasks.length - 3} more`)}`;
				return new Text(text, 0, 0);
			}
			const agentName = args.agent || "...";
			const preview = args.task ? (args.task.length > 60 ? `${args.task.slice(0, 60)}...` : args.task) : "...";
			let text =
				theme.fg("toolTitle", theme.bold("subagent ")) +
				theme.fg("accent", agentName) +
				theme.fg("muted", ` [${scope}]`);
			text += `\n  ${theme.fg("dim", preview)}`;
			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded }, theme, _context) {
			const details = result.details as SubagentDetails | undefined;
			if (!details || details.results.length === 0) {
				const text = result.content[0];
				return new Text(text?.type === "text" ? text.text : "(no output)", 0, 0);
			}

      if (details.outputMode === "file-only") {
        const text = result.content[0];
        return new Text(text?.type === "text" ? text.text : "(no output)", 0, 0);
      }

			const mdTheme = getMarkdownTheme();

			const renderDisplayItems = (items: DisplayItem[], limit?: number) => {
				const toShow = limit ? items.slice(-limit) : items;
				const skipped = limit && items.length > limit ? items.length - limit : 0;
				let text = "";
				if (skipped > 0) text += theme.fg("muted", `... ${skipped} earlier items\n`);
				for (const item of toShow) {
					if (item.type === "text") {
						const preview = expanded ? item.text : item.text.split("\n").slice(0, 3).join("\n");
						text += `${theme.fg("toolOutput", preview)}\n`;
					} else {
						text += `${theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme))}\n`;
					}
				}
				return text.trimEnd();
			};

			if (details.mode === "single" && details.results.length === 1) {
				const r = details.results[0];
				const isRunning = r.exitCode === -1;
				const isError = !isRunning && (r.exitCode !== 0 || r.stopReason === "error" || r.stopReason === "aborted");
				const icon = isRunning
					? theme.fg("warning", "⏳")
					: isError
						? theme.fg("error", "✗")
						: theme.fg("success", "✓");
				const displayItems = getDisplayItems(r.messages);
				const finalOutput = getFinalOutput(r.messages);
				const emptyOutputText = isRunning ? "(running...)" : "(no output)";

				if (expanded) {
					const container = new Container();
					let header = `${icon} ${theme.fg("toolTitle", theme.bold(r.agent))}${theme.fg("muted", ` (${r.agentSource})`)}${formatModelMetadata(r.model, theme.fg.bind(theme))}`;
					if (isError && r.stopReason) header += ` ${theme.fg("error", `[${r.stopReason}]`)}`;
					container.addChild(new Text(header, 0, 0));
					if (isError && r.errorMessage)
						container.addChild(new Text(theme.fg("error", `Error: ${r.errorMessage}`), 0, 0));
					container.addChild(new Spacer(1));
					container.addChild(new Text(theme.fg("muted", "─── Task ───"), 0, 0));
					container.addChild(new Text(theme.fg("dim", r.task), 0, 0));
					container.addChild(new Spacer(1));
					container.addChild(new Text(theme.fg("muted", "─── Output ───"), 0, 0));
					if (displayItems.length === 0 && !finalOutput) {
						container.addChild(new Text(theme.fg("muted", emptyOutputText), 0, 0));
					} else {
						for (const item of displayItems) {
							if (item.type === "toolCall")
								container.addChild(
									new Text(
										theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme)),
										0,
										0,
									),
								);
						}
						if (finalOutput) {
							container.addChild(new Spacer(1));
							container.addChild(new Markdown(finalOutput.trim(), 0, 0, mdTheme));
						}
					}
					const usageStr = formatUsageStats(r.usage);
					if (usageStr) {
						container.addChild(new Spacer(1));
						container.addChild(new Text(theme.fg("dim", usageStr), 0, 0));
					}
					return container;
				}

				let text = `${icon} ${theme.fg("toolTitle", theme.bold(r.agent))}${theme.fg("muted", ` (${r.agentSource})`)}${formatModelMetadata(r.model, theme.fg.bind(theme))}`;
				if (isError && r.stopReason) text += ` ${theme.fg("error", `[${r.stopReason}]`)}`;
				if (isError && r.errorMessage) text += `\n${theme.fg("error", `Error: ${r.errorMessage}`)}`;
				else if (displayItems.length === 0) text += `\n${theme.fg("muted", emptyOutputText)}`;
				else {
					text += `\n${renderDisplayItems(displayItems, COLLAPSED_ITEM_COUNT)}`;
					if (displayItems.length > COLLAPSED_ITEM_COUNT) text += `\n${theme.fg("muted", "(Ctrl+O to expand)")}`;
				}
				const usageStr = formatUsageStats(r.usage);
				if (usageStr) text += `\n${theme.fg("dim", usageStr)}`;
				return new Text(text, 0, 0);
			}

			if (details.mode === "chain") {
				const runningCount = details.results.filter((r) => r.exitCode === -1).length;
				const successCount = details.results.filter((r) => r.exitCode === 0).length;
				const failureCount = details.results.length - runningCount - successCount;
				const icon = runningCount > 0
					? theme.fg("warning", "⏳")
					: failureCount > 0
						? theme.fg("error", "✗")
						: theme.fg("success", "✓");

				if (expanded) {
					const container = new Container();
					container.addChild(
						new Text(
							icon +
								" " +
								theme.fg("toolTitle", theme.bold("chain ")) +
								theme.fg("accent", `${successCount}/${details.results.length} steps`),
							0,
							0,
						),
					);

					for (const r of details.results) {
						const rIcon =
							r.exitCode === -1
								? theme.fg("warning", "⏳")
								: r.exitCode === 0
									? theme.fg("success", "✓")
									: theme.fg("error", "✗");
						const displayItems = getDisplayItems(r.messages);
						const finalOutput = getFinalOutput(r.messages);

						container.addChild(new Spacer(1));
						container.addChild(
							new Text(
								`${theme.fg("muted", `─── Step ${r.step}: `) + theme.fg("accent", r.agent)}${formatModelMetadata(r.model, theme.fg.bind(theme))} ${rIcon}`,
								0,
								0,
							),
						);
						container.addChild(new Text(theme.fg("muted", "Task: ") + theme.fg("dim", r.task), 0, 0));

						// Show tool calls
						for (const item of displayItems) {
							if (item.type === "toolCall") {
								container.addChild(
									new Text(
										theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme)),
										0,
										0,
									),
								);
							}
						}

						// Show final output as markdown
						if (finalOutput) {
							container.addChild(new Spacer(1));
							container.addChild(new Markdown(finalOutput.trim(), 0, 0, mdTheme));
						} else if (r.exitCode === -1 && displayItems.length === 0) {
							container.addChild(new Text(theme.fg("muted", "(running...)"), 0, 0));
						}

						const stepUsage = formatUsageStats(r.usage);
						if (stepUsage) container.addChild(new Text(theme.fg("dim", stepUsage), 0, 0));
					}

					const usageStr = formatUsageStats(aggregateUsage(details.results));
					if (usageStr) {
						container.addChild(new Spacer(1));
						container.addChild(new Text(theme.fg("dim", `Total: ${usageStr}`), 0, 0));
					}
					return container;
				}

				// Collapsed view
				let text =
					icon +
					" " +
					theme.fg("toolTitle", theme.bold("chain ")) +
					theme.fg("accent", `${successCount}/${details.results.length} steps`);
				for (const r of details.results) {
					const rIcon =
						r.exitCode === -1
							? theme.fg("warning", "⏳")
							: r.exitCode === 0
								? theme.fg("success", "✓")
								: theme.fg("error", "✗");
					const displayItems = getDisplayItems(r.messages);
					text += `\n\n${theme.fg("muted", `─── Step ${r.step}: `)}${theme.fg("accent", r.agent)}${formatModelMetadata(r.model, theme.fg.bind(theme))} ${rIcon}`;
					if (displayItems.length === 0)
						text += `\n${theme.fg("muted", r.exitCode === -1 ? "(running...)" : "(no output)")}`;
					else text += `\n${renderDisplayItems(displayItems, 5)}`;
				}
				const usageStr = formatUsageStats(aggregateUsage(details.results));
				if (usageStr) text += `\n\n${theme.fg("dim", `Total: ${usageStr}`)}`;
				text += `\n${theme.fg("muted", "(Ctrl+O to expand)")}`;
				return new Text(text, 0, 0);
			}

			if (details.mode === "parallel") {
				const running = details.results.filter((r) => r.exitCode === -1).length;
				const successCount = details.results.filter((r) => r.exitCode === 0).length;
				const failCount = details.results.filter((r) => r.exitCode > 0).length;
				const isRunning = running > 0;
				const icon = isRunning
					? theme.fg("warning", "⏳")
					: failCount > 0
						? theme.fg("warning", "◐")
						: theme.fg("success", "✓");
				const status = isRunning
					? `${successCount + failCount}/${details.results.length} done, ${running} running`
					: `${successCount}/${details.results.length} tasks`;

				if (expanded && !isRunning) {
					const container = new Container();
					container.addChild(
						new Text(
							`${icon} ${theme.fg("toolTitle", theme.bold("parallel "))}${theme.fg("accent", status)}`,
							0,
							0,
						),
					);

					for (const r of details.results) {
						const rIcon = r.exitCode === 0 ? theme.fg("success", "✓") : theme.fg("error", "✗");
						const displayItems = getDisplayItems(r.messages);
						const finalOutput = getFinalOutput(r.messages);

						container.addChild(new Spacer(1));
						container.addChild(
							new Text(`${theme.fg("muted", "─── ") + theme.fg("accent", r.agent)}${formatModelMetadata(r.model, theme.fg.bind(theme))} ${rIcon}`, 0, 0),
						);
						container.addChild(new Text(theme.fg("muted", "Task: ") + theme.fg("dim", r.task), 0, 0));

						// Show tool calls
						for (const item of displayItems) {
							if (item.type === "toolCall") {
								container.addChild(
									new Text(
										theme.fg("muted", "→ ") + formatToolCall(item.name, item.args, theme.fg.bind(theme)),
										0,
										0,
									),
								);
							}
						}

						// Show final output as markdown
						if (finalOutput) {
							container.addChild(new Spacer(1));
							container.addChild(new Markdown(finalOutput.trim(), 0, 0, mdTheme));
						}

						const taskUsage = formatUsageStats(r.usage);
						if (taskUsage) container.addChild(new Text(theme.fg("dim", taskUsage), 0, 0));
					}

					const usageStr = formatUsageStats(aggregateUsage(details.results));
					if (usageStr) {
						container.addChild(new Spacer(1));
						container.addChild(new Text(theme.fg("dim", `Total: ${usageStr}`), 0, 0));
					}
					return container;
				}

				// Collapsed view (or still running)
				let text = `${icon} ${theme.fg("toolTitle", theme.bold("parallel "))}${theme.fg("accent", status)}`;
				for (const r of details.results) {
					const rIcon =
						r.exitCode === -1
							? theme.fg("warning", "⏳")
							: r.exitCode === 0
								? theme.fg("success", "✓")
								: theme.fg("error", "✗");
					const displayItems = getDisplayItems(r.messages);
					text += `\n\n${theme.fg("muted", "─── ")}${theme.fg("accent", r.agent)}${formatModelMetadata(r.model, theme.fg.bind(theme))} ${rIcon}`;
					if (displayItems.length === 0)
						text += `\n${theme.fg("muted", r.exitCode === -1 ? "(running...)" : "(no output)")}`;
					else text += `\n${renderDisplayItems(displayItems, 5)}`;
				}
				if (!isRunning) {
					const usageStr = formatUsageStats(aggregateUsage(details.results));
					if (usageStr) text += `\n\n${theme.fg("dim", `Total: ${usageStr}`)}`;
				}
				if (!expanded) text += `\n${theme.fg("muted", "(Ctrl+O to expand)")}`;
				return new Text(text, 0, 0);
			}

			const text = result.content[0];
			return new Text(text?.type === "text" ? text.text : "(no output)", 0, 0);
		},
	});
}
