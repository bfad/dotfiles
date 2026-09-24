import type { AgentToolResult } from "@mariozechner/pi-agent-core";
import type { Message } from "@mariozechner/pi-ai";

export type AgentScope = "user" | "project" | "both";
export type AgentSource = "package" | "user" | "project";
export type ResultAgentSource = AgentSource | "unknown";
export type WorktreeCleanupPolicy = "always" | "on-success" | "never";
export type WorktreeCleanupState = "removed" | "kept" | "failed";
export type ResultPhase = "worker" | "synthesis";
export type SubagentWorkflow = "parallel-synthesis";

export interface UsageStats {
  input: number;
  output: number;
  cacheRead: number;
  cacheWrite: number;
  cost: number;
  contextTokens: number;
  turns: number;
}

export type ChildActivityType = "message" | "tool_started" | "tool_finished" | "usage" | "stderr";

export interface ChildActivityUpdate {
  type: ChildActivityType;
  text?: string;
  toolName?: string;
  toolCallId?: string;
  path?: string;
  preview?: string;
  isError?: boolean;
  usage?: UsageStats;
  model?: string;
}

export type ChildActivityCallback = (activity: ChildActivityUpdate) => void;

export interface ResultOutputSummary {
  path: string;
  bytes: number;
  chars: number;
  lines: number;
  sha256: string;
  preview: string;
  previewChars: number;
  truncated: boolean;
  omittedChars: number;
}

export interface WorktreeResultMetadata {
  worktreePath: string;
  branchName: string;
  baseCommit: string;
  cleanupState: WorktreeCleanupState;
  cleanupPolicy: WorktreeCleanupPolicy;
  cleanupReason?: string;
  patchCaptureError?: string;
  cleanupError?: string;
  patchPath?: string;
  diffstatPath?: string;
  manifestPath?: string;
  nodeModulesLinked?: boolean;
  setupHookPath?: string;
  setupHookDurationMs?: number;
  setupHookStderr?: string;
  syntheticPaths?: string[];
}

export interface SingleResult {
  agent: string;
  agentSource: ResultAgentSource;
  phase?: ResultPhase;
  task: string;
  exitCode: number;
  messages: Message[];
  stderr: string;
  usage: UsageStats;
  model?: string;
  stopReason?: string;
  errorMessage?: string;
  step?: number;
  artifactDir?: string;
  artifactResultPath?: string;
  artifactTranscriptPath?: string;
  artifactStderrPath?: string;
  artifactSummaryPath?: string;
  outputPath?: string;
  outputSummary?: ResultOutputSummary;
  worktree?: WorktreeResultMetadata;
}

export type SubagentApplyState = "checked" | "applied" | "applied_but_audit_failed" | "failed" | "no_changes";

export interface SubagentApplyActionDetails {
  action: "apply";
  state: SubagentApplyState;
  runId?: string;
  cwd?: string;
  apply?: boolean;
  threeWay?: boolean;
  selected?: unknown[];
  skipped?: unknown[];
  commands?: unknown[];
  auditPath?: string;
  audit?: unknown;
  error?: {
    message: string;
    code?: string;
    auditPath?: string;
    details?: unknown;
  };
}

export type SubagentActionDetails = SubagentApplyActionDetails;

export interface SubagentDetails {
  mode: "single" | "parallel" | "chain";
  agentScope: AgentScope;
  projectAgentsDir: string | null;
  results: SingleResult[];
  outputMode?: "inline" | "file-only";
  runId?: string;
  artifactDir?: string;
  warnings?: string[];
  disabledAgents?: string[];
  rejectedAgents?: Array<{ name: string; location: string }>;
  partialFailure?: boolean;
  workflow?: SubagentWorkflow;
  synthesisResultIndex?: number;
  actionDetails?: SubagentActionDetails;
}

export type DisplayItem =
  | { type: "text"; text: string }
  | { type: "toolCall"; name: string; args: Record<string, any> };

export type OnUpdateCallback = (partial: AgentToolResult<SubagentDetails>) => void;

export interface SubagentTaskItem {
  agent: string;
  task: string;
  cwd?: string;
  writes?: boolean;
}

export interface SynthesizeWith {
  agent: string;
  task: string;
  cwd?: string;
  maxInputChars?: number;
}
