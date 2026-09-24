import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as path from "node:path";
import { withFileMutationQueue } from "@mariozechner/pi-coding-agent";
import { getFinalOutput } from "./output.js";
import { defaultRunStateDir } from "./run-paths.js";
import type { ResultOutputSummary, SingleResult } from "./types.js";

export type SubagentRunMode = "single" | "parallel" | "chain";
export type OutputMode = "inline" | "file-only";
export type OutputPathKind = "file" | "directory";

export const DEFAULT_MAX_OUTPUT_CHARS = 2_000;
export const DEFAULT_SYNTHESIS_INPUT_CHARS = 20_000;
export const FILE_ONLY_PREVIEW_CHARS = 800;
export const SYNTHESIS_INPUT_FILENAME = "synthesis-input.md";
export const RESULT_OUTPUT_PREVIEW_CHARS = FILE_ONLY_PREVIEW_CHARS;

export interface TruncatedText {
  text: string;
  truncated: boolean;
  originalChars: number;
  omittedChars: number;
}

export interface ArtifactPlan {
  runId: string;
  cwd: string;
  artifactDir: string;
  outputPath?: string;
  outputKind?: OutputPathKind;
}

export interface ResultArtifactPaths {
  artifactDir: string;
  resultPath: string;
  transcriptPath: string;
  stderrPath: string;
  summaryPath: string;
  outputPath?: string;
}

export interface ReadPreview {
  requestedPath: string;
  path?: string;
  preview?: string;
  truncated?: boolean;
  bytes?: number;
  error?: string;
}

export function createRunId(now = new Date(), random = Math.random()): string {
  const timestamp = now.toISOString().replace(/[-:]/g, "").replace(/\.\d{3}Z$/, "Z");
  const suffixSpace = 36 ** 8;
  const normalizedRandom = Number.isFinite(random) ? Math.min(Math.max(random, 0), 1 - Number.EPSILON) : 0;
  const suffix = Math.floor(normalizedRandom * suffixSpace + Number.EPSILON)
    .toString(36)
    .padStart(8, "0");
  return `${timestamp}-${suffix}`;
}

function resolvePositiveInteger(value: number | undefined, defaultValue: number): number {
  if (typeof value !== "number" || !Number.isFinite(value) || value <= 0) return defaultValue;
  return Math.floor(value);
}

export function resolveMaxOutput(maxOutput?: number): number {
  return resolvePositiveInteger(maxOutput, DEFAULT_MAX_OUTPUT_CHARS);
}

export function resolveMaxSynthesisInput(maxInputChars?: number): number {
  return resolvePositiveInteger(maxInputChars, DEFAULT_SYNTHESIS_INPUT_CHARS);
}

export function previewLimitForMode(outputMode: OutputMode, maxOutput: number): number {
  return outputMode === "file-only" ? Math.min(maxOutput, FILE_ONLY_PREVIEW_CHARS) : maxOutput;
}

export function truncateText(text: string, maxChars: number): TruncatedText {
  const limit = Math.max(0, Math.floor(maxChars));
  const originalChars = text.length;
  if (originalChars <= limit) {
    return { text, truncated: false, originalChars, omittedChars: 0 };
  }

  const omittedChars = originalChars - limit;
  const marker = `\n\n[truncated ${omittedChars} chars; see artifact for full output]`;
  if (limit <= marker.length) {
    return { text: text.slice(0, limit), truncated: true, originalChars, omittedChars };
  }

  return {
    text: `${text.slice(0, limit - marker.length)}${marker}`,
    truncated: true,
    originalChars,
    omittedChars,
  };
}

function pathIsWithinRoot(root: string, candidate: string): boolean {
  const relative = path.relative(root, candidate);
  return relative === "" || (!relative.startsWith("..") && !path.isAbsolute(relative));
}

function lstatExistingPath(filePath: string): fs.Stats | undefined {
  try {
    return fs.lstatSync(filePath);
  } catch (error) {
    const code = (error as NodeJS.ErrnoException).code;
    if (code === "ENOENT" || code === "ENOTDIR") return undefined;
    throw error;
  }
}

function nearestExistingPath(filePath: string): string {
  let current = filePath;
  while (!lstatExistingPath(current)) {
    current = path.dirname(current);
  }
  return current;
}

function realpathForBoundary(filePath: string, requestedPath: string): string {
  try {
    return fs.realpathSync.native(filePath);
  } catch {
    throw new Error(`Path must stay within cwd: ${requestedPath}`);
  }
}

function countLines(text: string): number {
  if (text.length === 0) return 0;
  return text.split(/\r\n|\r|\n/).length;
}

export function summarizeResultOutput(
  output: string,
  outputPath: string,
  previewChars = RESULT_OUTPUT_PREVIEW_CHARS,
): ResultOutputSummary {
  const limit = typeof previewChars === "number" && Number.isFinite(previewChars)
    ? Math.max(0, Math.floor(previewChars))
    : RESULT_OUTPUT_PREVIEW_CHARS;
  const preview = output.slice(0, limit);
  return {
    path: outputPath,
    bytes: Buffer.byteLength(output, "utf-8"),
    chars: output.length,
    lines: countLines(output),
    sha256: crypto.createHash("sha256").update(output, "utf-8").digest("hex"),
    preview,
    previewChars: preview.length,
    truncated: output.length > preview.length,
    omittedChars: Math.max(0, output.length - preview.length),
  };
}

export function resolvePathWithinCwd(cwd: string, requestedPath: string): string {
  const root = path.resolve(cwd);
  const resolved = path.resolve(root, requestedPath);
  if (!pathIsWithinRoot(root, resolved)) {
    throw new Error(`Path must stay within cwd: ${requestedPath}`);
  }

  const rootReal = realpathForBoundary(root, requestedPath);
  const nearestReal = realpathForBoundary(nearestExistingPath(resolved), requestedPath);
  if (!pathIsWithinRoot(rootReal, nearestReal)) {
    throw new Error(`Path must stay within cwd: ${requestedPath}`);
  }
  return resolved;
}

export function classifyOutputPath(
  requestedPath: string,
  resolvedPath: string,
  missingPathDefault: OutputPathKind = "directory",
): OutputPathKind {
  try {
    const stat = fs.statSync(resolvedPath);
    return stat.isDirectory() ? "directory" : "file";
  } catch {
    // Fall back to caller syntax for paths that do not exist yet.
  }

  if (/[\\/]$/.test(requestedPath)) return "directory";
  return path.extname(path.basename(requestedPath)) ? "file" : missingPathDefault;
}

export function resolveArtifactPlan(
  cwd: string,
  mode: SubagentRunMode,
  output?: string,
  runId = createRunId(),
): ArtifactPlan {
  const root = path.resolve(cwd);
  const defaultArtifactDir = defaultRunStateDir(runId);

  if (!output?.trim()) {
    return { runId, cwd: root, artifactDir: defaultArtifactDir };
  }

  const outputPath = resolvePathWithinCwd(root, output);
  const outputKind = classifyOutputPath(output, outputPath, mode === "single" ? "file" : "directory");
  if (mode !== "single" && outputKind === "file") {
    throw new Error(
      `For ${mode} mode, output must be a directory path. Received file-looking output: ${output}`,
    );
  }

  if (outputKind === "directory") {
    return { runId, cwd: root, artifactDir: outputPath, outputPath, outputKind };
  }

  return { runId, cwd: root, artifactDir: defaultArtifactDir, outputPath, outputKind };
}

export function safeArtifactName(agent: string, sequence?: number): string {
  const safeAgent = agent.replace(/[^\w.-]+/g, "_").replace(/^_+|_+$/g, "") || "agent";
  if (sequence === undefined) return "result";
  return `${String(sequence).padStart(2, "0")}-${safeAgent}`;
}

export function resultArtifactPaths(plan: ArtifactPlan, agent: string, sequence?: number): ResultArtifactPaths {
  const name = safeArtifactName(agent, sequence);
  return {
    artifactDir: plan.artifactDir,
    resultPath: path.join(plan.artifactDir, `${name}.md`),
    transcriptPath: path.join(plan.artifactDir, `${name}.transcript.json`),
    stderrPath: path.join(plan.artifactDir, `${name}.stderr.txt`),
    summaryPath: path.join(plan.artifactDir, `${name}.summary.json`),
    outputPath: sequence === undefined ? plan.outputKind === "file" ? plan.outputPath : undefined : undefined,
  };
}

export function synthesisInputArtifactPath(plan: Pick<ArtifactPlan, "artifactDir">): string {
  return path.join(plan.artifactDir, SYNTHESIS_INPUT_FILENAME);
}

export async function writeArtifactTextFile(filePath: string, content: string): Promise<void> {
  await fs.promises.mkdir(path.dirname(filePath), { recursive: true, mode: 0o700 });
  await withFileMutationQueue(filePath, async () => {
    await fs.promises.writeFile(filePath, content, { encoding: "utf-8", mode: 0o600 });
  });
}

export async function writeResultArtifacts(result: SingleResult, paths: ResultArtifactPaths): Promise<SingleResult> {
  const output = getFinalOutput(result.messages) || "";
  await writeArtifactTextFile(paths.resultPath, output);

  const enriched: SingleResult = {
    ...result,
    artifactDir: paths.artifactDir,
    artifactResultPath: paths.resultPath,
    artifactTranscriptPath: paths.transcriptPath,
    artifactStderrPath: paths.stderrPath,
    artifactSummaryPath: paths.summaryPath,
    outputPath: paths.outputPath,
    outputSummary: summarizeResultOutput(output, paths.resultPath),
  };

  await writeArtifactTextFile(paths.transcriptPath, `${JSON.stringify(result.messages, null, 2)}\n`);
  await writeArtifactTextFile(paths.stderrPath, result.stderr || "");
  await writeArtifactTextFile(paths.summaryPath, `${JSON.stringify(resultSummary(enriched), null, 2)}\n`);
  if (paths.outputPath) await writeArtifactTextFile(paths.outputPath, output);

  return enriched;
}

function resultSummary(result: SingleResult): Record<string, unknown> {
  return {
    agent: result.agent,
    agentSource: result.agentSource,
    phase: result.phase,
    task: result.task,
    step: result.step,
    exitCode: result.exitCode,
    stopReason: result.stopReason,
    errorMessage: result.errorMessage,
    model: result.model,
    usage: result.usage,
    artifactDir: result.artifactDir,
    artifactResultPath: result.artifactResultPath,
    artifactTranscriptPath: result.artifactTranscriptPath,
    artifactStderrPath: result.artifactStderrPath,
    artifactSummaryPath: result.artifactSummaryPath,
    outputPath: result.outputPath,
    outputSummary: result.outputSummary,
    worktree: result.worktree,
  };
}

export interface RunArtifactIndexOptions {
  plan: ArtifactPlan;
  mode: SubagentRunMode;
  results: SingleResult[];
  state?: string;
  readPreviews?: ReadPreview[];
  warnings?: string[];
}

export interface RunArtifactIndexPaths {
  indexPath: string;
  manifestPath: string;
}

export function runArtifactIndexPaths(artifactDir: string): RunArtifactIndexPaths {
  return {
    indexPath: path.join(artifactDir, "index.md"),
    manifestPath: path.join(artifactDir, "manifest.json"),
  };
}

export type ResultArtifactStatus = "completed" | "failed" | "canceled";

export function resultArtifactStatus(result: Pick<SingleResult, "exitCode" | "stopReason">): ResultArtifactStatus {
  if (result.stopReason === "aborted") return "canceled";
  return result.exitCode !== 0 || result.stopReason === "error" ? "failed" : "completed";
}

function indentBlock(text: string, prefix = "  "): string {
  return text.split("\n").map((line) => `${prefix}${line}`).join("\n");
}

function formatIndexResultCard(result: SingleResult, index: number): string {
  const title = result.phase ? `${result.agent} (${result.phase})` : result.agent;
  const lines = [
    `### ${index + 1}. ${title}`,
    `- Status: ${resultArtifactStatus(result)}`,
    `- Task: ${result.task}`,
  ];

  if (result.outputSummary) {
    const summary = result.outputSummary;
    lines.push(`- Output: ${summary.path}`);
    lines.push(`- Stats: ${summary.chars} chars, ${summary.lines} lines, ${summary.bytes} bytes, sha256=${summary.sha256}`);
    lines.push(`- Truncated: ${summary.truncated}${summary.truncated ? `, omitted ${summary.omittedChars} chars` : ""}`);
    lines.push("- Preview:");
    lines.push(indentBlock(summary.preview || "(empty)"));
  } else {
    lines.push(`- Output: ${result.artifactResultPath ?? "(metadata unavailable)"}`);
    lines.push("- Stats: (metadata unavailable)");
  }

  if (result.errorMessage) lines.push(`- Error: ${result.errorMessage}`);
  if (result.artifactTranscriptPath) lines.push(`- Transcript: ${result.artifactTranscriptPath}`);
  if (result.artifactStderrPath) lines.push(`- Stderr: ${result.artifactStderrPath}`);
  if (result.artifactSummaryPath) lines.push(`- Summary: ${result.artifactSummaryPath}`);
  if (result.outputPath) lines.push(`- Output copy: ${result.outputPath}`);
  if (result.worktree?.patchPath) lines.push(`- Patch: ${result.worktree.patchPath}`);
  if (result.worktree?.diffstatPath) lines.push(`- Diffstat: ${result.worktree.diffstatPath}`);
  if (result.worktree?.manifestPath) lines.push(`- Worktree manifest: ${result.worktree.manifestPath}`);
  return lines.join("\n");
}

function formatReadPreviewsForIndex(readPreviews: ReadPreview[] | undefined): string[] {
  if (!readPreviews?.length) return [];
  const lines = ["", "## Requested reads"];
  for (const preview of readPreviews) {
    if (preview.error) {
      lines.push(`- ${preview.requestedPath}: ${preview.error}`);
      continue;
    }
    lines.push(`- ${preview.requestedPath}: ${preview.path} (${preview.bytes ?? 0} bytes)`);
    if (preview.preview) lines.push(indentBlock(preview.preview));
    if (preview.truncated) lines.push("  [read preview truncated]");
  }
  return lines;
}

function formatWarningsForIndex(warnings: string[] | undefined): string[] {
  if (!warnings?.length) return [];
  return ["", "## Warnings", ...warnings.map((warning) => `- ${warning}`)];
}

function formatRunArtifactIndex(options: RunArtifactIndexOptions, paths: RunArtifactIndexPaths): string {
  const lines = [
    `# Subagent run ${options.plan.runId}`,
    "",
    `- Mode: ${options.mode}`,
    `- Cwd: ${options.plan.cwd}`,
    `- State: ${options.state ?? "unknown"}`,
    `- Artifacts: ${options.plan.artifactDir}`,
    `- Manifest: ${paths.manifestPath}`,
    "",
    "## Results",
  ];
  if (options.results.length === 0) {
    lines.push("(none)");
  } else {
    lines.push(...options.results.map(formatIndexResultCard).join("\n\n").split("\n"));
  }
  lines.push(...formatReadPreviewsForIndex(options.readPreviews));
  lines.push(...formatWarningsForIndex(options.warnings));
  return `${lines.join("\n")}\n`;
}

function runArtifactManifest(options: RunArtifactIndexOptions, paths: RunArtifactIndexPaths): Record<string, unknown> {
  return {
    runId: options.plan.runId,
    mode: options.mode,
    cwd: options.plan.cwd,
    state: options.state ?? "unknown",
    artifactDir: options.plan.artifactDir,
    indexPath: paths.indexPath,
    manifestPath: paths.manifestPath,
    warnings: options.warnings ?? [],
    readPreviews: options.readPreviews ?? [],
    results: options.results.map((result, index) => ({
      index: index + 1,
      status: resultArtifactStatus(result),
      ...resultSummary(result),
    })),
  };
}

export async function writeRunArtifactIndex(options: RunArtifactIndexOptions): Promise<RunArtifactIndexPaths> {
  const paths = runArtifactIndexPaths(options.plan.artifactDir);
  await writeArtifactTextFile(paths.indexPath, formatRunArtifactIndex(options, paths));
  await writeArtifactTextFile(paths.manifestPath, `${JSON.stringify(runArtifactManifest(options, paths), null, 2)}\n`);
  return paths;
}

export async function buildReadPreviews(
  cwd: string,
  reads: string[] | undefined,
  maxChars: number,
): Promise<ReadPreview[]> {
  if (!reads || reads.length === 0) return [];

  const previews: ReadPreview[] = [];
  for (const requestedPath of reads) {
    try {
      const resolvedPath = resolvePathWithinCwd(cwd, requestedPath);
      const content = await fs.promises.readFile(resolvedPath, "utf-8");
      const stat = await fs.promises.stat(resolvedPath);
      const truncated = truncateText(content, maxChars);
      previews.push({
        requestedPath,
        path: resolvedPath,
        preview: truncated.text,
        truncated: truncated.truncated,
        bytes: stat.size,
      });
    } catch (error) {
      previews.push({ requestedPath, error: error instanceof Error ? error.message : String(error) });
    }
  }
  return previews;
}

function formatSynthesisArtifactPaths(result: SingleResult): string {
  return [
    result.artifactResultPath ? `result=${result.artifactResultPath}` : undefined,
    result.artifactTranscriptPath ? `transcript=${result.artifactTranscriptPath}` : undefined,
    result.artifactStderrPath ? `stderr=${result.artifactStderrPath}` : undefined,
    result.artifactSummaryPath ? `summary=${result.artifactSummaryPath}` : undefined,
    result.outputPath ? `output=${result.outputPath}` : undefined,
    result.worktree?.patchPath ? `patch=${result.worktree.patchPath}` : undefined,
    result.worktree?.diffstatPath ? `diffstat=${result.worktree.diffstatPath}` : undefined,
    result.worktree?.manifestPath ? `worktree=${result.worktree.manifestPath}` : undefined,
  ].filter(Boolean).join(", ");
}

function synthesisResultState(result: SingleResult): "canceled" | "failed" | "succeeded" {
  const status = resultArtifactStatus(result);
  return status === "completed" ? "succeeded" : status;
}

function synthesisOutput(result: SingleResult): string {
  return getFinalOutput(result.messages) || result.stderr || result.errorMessage || "(no output)";
}

function truncateSynthesisExcerpt(text: string, maxChars: number): TruncatedText {
  const limit = Math.max(0, Math.floor(maxChars));
  const originalChars = text.length;
  if (originalChars <= limit) return { text, truncated: false, originalChars, omittedChars: 0 };
  return {
    text: text.slice(0, limit),
    truncated: true,
    originalChars,
    omittedChars: originalChars - limit,
  };
}

function boundedMetadataValue(value: string, maxChars = 1_000): string {
  const truncated = truncateSynthesisExcerpt(value, maxChars);
  if (!truncated.truncated) return truncated.text;
  return `${truncated.text} [omitted ${truncated.omittedChars} chars]`;
}

interface SynthesisWorkerInput {
  index: number;
  result: SingleResult;
  state: "canceled" | "failed" | "succeeded";
  artifactPaths: string;
  output: string;
}

function synthesisWorkers(results: SingleResult[]): SynthesisWorkerInput[] {
  return results.map((result, index) => ({
    index,
    result,
    state: synthesisResultState(result),
    artifactPaths: formatSynthesisArtifactPaths(result) || "(none)",
    output: synthesisOutput(result),
  }));
}

function fairExcerptLimits(outputs: string[], totalBudget: number): number[] {
  const limits = outputs.map(() => 0);
  let remaining = Math.max(0, Math.floor(totalBudget));
  const active = new Set(outputs.map((_output, index) => index).filter((index) => outputs[index].length > 0));

  while (remaining > 0 && active.size > 0) {
    const share = Math.max(1, Math.floor(remaining / active.size));
    let spent = 0;
    for (const index of Array.from(active)) {
      const needed = outputs[index].length - limits[index];
      const grant = Math.min(needed, share, remaining);
      limits[index] += grant;
      remaining -= grant;
      spent += grant;
      if (limits[index] >= outputs[index].length) active.delete(index);
      if (remaining === 0) break;
    }
  }

  return limits;
}

function formatSynthesisWorkerSection(worker: SynthesisWorkerInput, excerptLimit: number): string {
  const result = worker.result;
  const excerpt = truncateSynthesisExcerpt(worker.output, excerptLimit);
  const lines = [
    `### Worker ${worker.index + 1}: ${result.agent}`,
    `- State: ${worker.state}`,
    `- Task: ${boundedMetadataValue(result.task)}`,
    `- Artifacts: ${worker.artifactPaths}`,
    `- Output excerpt: ${excerpt.text.length}/${excerpt.originalChars} chars; truncated=${excerpt.truncated}; omittedChars=${excerpt.omittedChars}`,
  ];

  if (result.errorMessage) lines.push(`- Error: ${boundedMetadataValue(result.errorMessage)}`);
  if (result.stderr && worker.state !== "succeeded") lines.push(`- Stderr: ${boundedMetadataValue(result.stderr)}`);
  if (excerpt.text.length > 0) lines.push("", excerpt.text);
  return lines.join("\n");
}

function formatSynthesisSections(workers: SynthesisWorkerInput[], excerptLimits: number[]): string {
  const sections = [
    [
      "## Parallel worker outputs",
      "Every worker is represented. Output excerpts are fair-shared and may be truncated; use artifact paths for omitted content.",
    ].join("\n"),
  ];
  sections.push(...workers.map((worker, index) => formatSynthesisWorkerSection(worker, excerptLimits[index])));
  return sections.join("\n\n---\n\n");
}

function capSynthesisInput(text: string, limit: number): string {
  const cappedLimit = Math.max(0, Math.floor(limit));
  if (text.length <= cappedLimit) return text;

  const marker = "\n\n[truncated to maxInputChars]";
  if (cappedLimit <= marker.length) return text.slice(0, cappedLimit);
  return `${text.slice(0, cappedLimit - marker.length)}${marker}`;
}

function fitSynthesisInputToLimit(workers: SynthesisWorkerInput[], excerptLimits: number[], limit: number): string {
  let formatted = formatSynthesisSections(workers, excerptLimits);
  while (formatted.length > limit && excerptLimits.some((value) => value > 0)) {
    const overage = formatted.length - limit;
    const largestExcerpt = Math.max(...excerptLimits);
    const index = excerptLimits.findIndex((value) => value === largestExcerpt);
    excerptLimits[index] = Math.max(0, largestExcerpt - Math.max(1, overage));
    formatted = formatSynthesisSections(workers, excerptLimits);
  }
  return capSynthesisInput(formatted, limit);
}

export function buildSynthesisTask(task: string, results: SingleResult[], maxInputChars?: number): string {
  const formattedInput = formatSynthesisInput(results, maxInputChars);
  if (!task.includes("{results}")) return `${task}\n\n---\n\n${formattedInput}`;

  let insertedResults = false;
  return task.replace(/\{results\}/g, () => {
    if (insertedResults) return "[worker outputs omitted: already inserted above]";
    insertedResults = true;
    return formattedInput;
  });
}

export function formatSynthesisInput(results: SingleResult[], maxInputChars?: number): string {
  const workers = synthesisWorkers(results);
  if (workers.length === 0) return "## Parallel worker outputs\n\n(no workers)";

  const limit = resolveMaxSynthesisInput(maxInputChars);
  const zeroExcerptLimits = workers.map(() => 0);
  const fixedMetadataLength = formatSynthesisSections(workers, zeroExcerptLimits).length;
  const excerptBudget = Math.max(0, limit - fixedMetadataLength);
  const excerptLimits = fairExcerptLimits(workers.map((worker) => worker.output), excerptBudget);
  return fitSynthesisInputToLimit(workers, excerptLimits, limit);
}
