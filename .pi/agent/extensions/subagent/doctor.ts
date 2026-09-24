import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { getAgentDir } from "@mariozechner/pi-coding-agent";
import { type AgentDiscoveryResult, discoverAgents, discoverBundledAgents } from "./agents.js";
import { loadSubagentConfig, type SubagentConfig } from "./config.js";
import { recoverableWorktrees, listRecentRuns, type RecentRunsResult } from "./run-state.js";
import { getPiInvocation, type PiInvocation } from "./runner.js";
import type { AgentScope } from "./types.js";

const EXPECTED_BUNDLED_ROLES = ["oracle", "planner", "reviewer", "scout", "worker"];
const DEFAULT_PI_STARTUP_TIMEOUT_MS = 2_000;

export type DoctorCheckStatus = "ok" | "warn" | "error" | "info";

export interface DoctorCheck {
  name: string;
  status: DoctorCheckStatus;
  message: string;
  details?: Record<string, unknown>;
}

export interface DoctorReport {
  ok: boolean;
  cwd: string;
  agentScope: AgentScope;
  checks: DoctorCheck[];
}

export interface DoctorOptions {
  cwd: string;
  agentScope: AgentScope;
}

export interface DoctorDependencies {
  discoverAgents: (cwd: string, scope: AgentScope) => AgentDiscoveryResult;
  discoverBundledAgents: typeof discoverBundledAgents;
  getAgentDir: () => string;
  getPiInvocation: (args: string[]) => PiInvocation;
  listRecentRuns: (limit?: number) => Promise<RecentRunsResult>;
  loadSubagentConfig: (cwd: string) => SubagentConfig;
  spawn: typeof spawn;
  env: NodeJS.ProcessEnv;
  piStartupTimeoutMs: number;
}

export function defaultDoctorDependencies(): DoctorDependencies {
  return {
    discoverAgents,
    discoverBundledAgents,
    getAgentDir,
    getPiInvocation,
    listRecentRuns,
    loadSubagentConfig,
    spawn,
    env: process.env,
    piStartupTimeoutMs: DEFAULT_PI_STARTUP_TIMEOUT_MS,
  };
}

export async function runSubagentDoctor(
  options: DoctorOptions,
  partialDeps: Partial<DoctorDependencies> = {},
): Promise<DoctorReport> {
  const deps = { ...defaultDoctorDependencies(), ...partialDeps };
  const checks: DoctorCheck[] = [];

  const discovery = deps.discoverAgents(options.cwd, options.agentScope);
  const sourceCounts = countBy(discovery.agents.map((agent) => agent.source));
  checks.push({
    name: "agent-discovery",
    status: discovery.agents.length > 0 ? "ok" : "warn",
    message: discovery.agents.length > 0
      ? `Discovered ${discovery.agents.length} agent(s) for scope "${options.agentScope}".`
      : `No agents discovered for scope "${options.agentScope}".`,
    details: {
      sources: sourceCounts,
      projectAgentsDir: discovery.projectAgentsDir,
      agents: discovery.agents.map((agent) => ({ name: agent.name, source: agent.source, filePath: agent.filePath })),
    },
  });

  const bundledNames = new Set(deps.discoverBundledAgents().map((agent) => agent.name));
  const missingRoles = EXPECTED_BUNDLED_ROLES.filter((role) => !bundledNames.has(role));
  checks.push({
    name: "bundled-roles",
    status: missingRoles.length === 0 ? "ok" : "error",
    message: missingRoles.length === 0
      ? `Bundled roles available: ${EXPECTED_BUNDLED_ROLES.join(", ")}.`
      : `Missing bundled role(s): ${missingRoles.join(", ")}.`,
    details: { expected: EXPECTED_BUNDLED_ROLES, missing: missingRoles },
  });

  checks.push(checkDisabledAgents(deps.loadSubagentConfig(options.cwd)));
  checks.push(checkPiPackageDir(deps.env.PI_PACKAGE_DIR));

  const agentDir = deps.getAgentDir();
  const conflicts = findLikelySubagentPackageConflicts(agentDir);
  const diagnosticConflicts = conflicts.map(redactPackageSourceForDiagnostics);
  checks.push({
    name: "package-conflicts",
    status: conflicts.length > 0 ? "warn" : "ok",
    message: conflicts.length > 0
      ? `Found package setting(s) that look like OSS pi-subagents: ${diagnosticConflicts.join(", ")}.`
      : "No likely OSS pi-subagents package conflicts found in settings.json.",
    details: { settingsPath: path.join(agentDir, "settings.json"), conflicts: diagnosticConflicts },
  });

  checks.push(await checkRecoverableWorktrees(deps));
  checks.push(await checkPiStartup(deps));

  return {
    ok: checks.every((check) => check.status !== "error"),
    cwd: path.resolve(options.cwd),
    agentScope: options.agentScope,
    checks,
  };
}

export function checkDisabledAgents(config: SubagentConfig): DoctorCheck {
  const disabledText = config.disabledAgents.length > 0
    ? `Disabled agents: ${config.disabledAgents.join(", ")}.`
    : "No subagent.disabledAgents configured.";
  const warningText = config.warnings.length > 0 ? ` Warnings: ${config.warnings.join("; ")}` : "";

  if (config.errors.length > 0) {
    return {
      name: "disabled-agents",
      status: "error",
      message: `Invalid subagent.disabledAgents: ${config.errors.join("; ")}.${warningText}`,
      details: { ...config },
    };
  }

  if (config.warnings.length > 0) {
    return {
      name: "disabled-agents",
      status: "warn",
      message: `${disabledText}${warningText}`,
      details: { ...config },
    };
  }

  return {
    name: "disabled-agents",
    status: config.disabledAgents.length > 0 ? "ok" : "info",
    message: disabledText,
    details: { ...config },
  };
}

export function checkPiPackageDir(piPackageDir: string | undefined): DoctorCheck {
  if (!piPackageDir) {
    return {
      name: "PI_PACKAGE_DIR",
      status: "info",
      message: "PI_PACKAGE_DIR is not set; child Pi will use its normal package resolution.",
    };
  }

  let stat: fs.Stats;
  try {
    stat = fs.statSync(piPackageDir);
  } catch (error) {
    return {
      name: "PI_PACKAGE_DIR",
      status: "error",
      message: `PI_PACKAGE_DIR is set but does not exist: ${piPackageDir}`,
      details: { error: String(error) },
    };
  }

  if (!stat.isDirectory()) {
    return {
      name: "PI_PACKAGE_DIR",
      status: "error",
      message: `PI_PACKAGE_DIR is not a directory: ${piPackageDir}`,
    };
  }

  const plausibleMarkers = ["package.json", "extensions", "skills", "prompts", "themes"];
  const presentMarkers = plausibleMarkers.filter((marker) => fs.existsSync(path.join(piPackageDir, marker)));
  if (presentMarkers.length === 0) {
    return {
      name: "PI_PACKAGE_DIR",
      status: "warn",
      message: `PI_PACKAGE_DIR exists but does not look like a Pi package directory: ${piPackageDir}`,
      details: { expectedAnyOf: plausibleMarkers },
    };
  }

  return {
    name: "PI_PACKAGE_DIR",
    status: "ok",
    message: `PI_PACKAGE_DIR exists and looks plausible: ${piPackageDir}`,
    details: { markers: presentMarkers },
  };
}

export async function checkRecoverableWorktrees(
  deps: Pick<DoctorDependencies, "listRecentRuns">,
): Promise<DoctorCheck> {
  try {
    const recent = await deps.listRecentRuns(50);
    const worktrees = recoverableWorktrees(recent.statuses);
    return {
      name: "worktree-recovery",
      status: worktrees.length > 0 ? "warn" : "ok",
      message: worktrees.length > 0
        ? `Found ${worktrees.length} managed worktree(s) still marked kept/failed. Run action: "prune" after reviewing patches, or clean them manually.`
        : "No recent managed worktrees need recovery cleanup.",
      details: {
        worktrees,
        warnings: recent.warnings,
      },
    };
  } catch (error) {
    return {
      name: "worktree-recovery",
      status: "warn",
      message: `Could not inspect recent run-state for worktree recovery: ${error instanceof Error ? error.message : String(error)}`,
    };
  }
}

export async function checkPiStartup(deps: Pick<DoctorDependencies, "getPiInvocation" | "spawn" | "piStartupTimeoutMs">): Promise<DoctorCheck> {
  const invocation = deps.getPiInvocation(["--version"]);
  return new Promise((resolve) => {
    let child: ChildProcessWithoutNullStreams;
    let stdout = "";
    let stderr = "";
    let settled = false;

    const settle = (check: DoctorCheck) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      resolve(check);
    };

    const timer = setTimeout(() => {
      try {
        child?.kill("SIGTERM");
      } catch {
        // Child may already be gone.
      }
      settle({
        name: "pi-startup",
        status: "error",
        message: `Child Pi startup check timed out after ${deps.piStartupTimeoutMs}ms: ${formatInvocation(invocation)}`,
      });
    }, deps.piStartupTimeoutMs);
    timer.unref?.();

    try {
      child = deps.spawn(invocation.command, invocation.args, {
        stdio: ["ignore", "pipe", "pipe"],
        shell: false,
      }) as unknown as ChildProcessWithoutNullStreams;
    } catch (error) {
      settle({
        name: "pi-startup",
        status: "error",
        message: `Could not spawn child Pi: ${error instanceof Error ? error.message : String(error)}`,
        details: { invocation: formatInvocation(invocation) },
      });
      return;
    }

    child.stdout.on("data", (chunk: Buffer | string) => {
      stdout += chunk.toString();
    });
    child.stderr.on("data", (chunk: Buffer | string) => {
      stderr += chunk.toString();
    });
    child.once("error", (error: Error) => {
      settle({
        name: "pi-startup",
        status: "error",
        message: `Child Pi startup failed: ${error.message}`,
        details: { invocation: formatInvocation(invocation), stderr: stderr.trim() },
      });
    });
    child.once("close", (code: number | null) => {
      const output = `${stdout}${stderr}`.trim();
      settle({
        name: "pi-startup",
        status: code === 0 ? "ok" : "error",
        message: code === 0
          ? `Child Pi started successfully with ${formatInvocation(invocation)}${output ? ` (${firstLine(output)})` : ""}.`
          : `Child Pi startup exited with code ${code ?? "unknown"}: ${formatInvocation(invocation)}`,
        details: { invocation: formatInvocation(invocation), stdout: stdout.trim(), stderr: stderr.trim() },
      });
    });
  });
}

export function redactPackageSourceForDiagnostics(source: string): string {
  return source.replace(/\b([a-z][a-z0-9+.-]*:\/\/)([^/?#\s@]+)@/gi, "$1[redacted]@");
}

export function findLikelySubagentPackageConflicts(agentDir: string): string[] {
  const settingsPath = path.join(agentDir, "settings.json");
  let settings: unknown;
  try {
    settings = JSON.parse(fs.readFileSync(settingsPath, "utf-8"));
  } catch {
    return [];
  }

  if (!settings || typeof settings !== "object" || Array.isArray(settings)) return [];
  const packages = (settings as { packages?: unknown }).packages;
  if (!Array.isArray(packages)) return [];

  const conflicts: string[] = [];
  for (const entry of packages) {
    let source: string | undefined;
    if (typeof entry === "string") {
      source = entry;
    } else if (entry && typeof entry === "object") {
      const entrySource = (entry as { source?: unknown }).source;
      if (typeof entrySource === "string") source = entrySource;
    }
    if (!source) continue;
    const normalized = source.toLowerCase();
    if (normalized.includes("pi-subagents") || /(^|[/@])subagents($|[/.#?])/.test(normalized)) {
      conflicts.push(source);
    }
  }
  return conflicts;
}

export function formatDoctorReport(report: DoctorReport): string {
  const lines = [
    `Subagent doctor: ${report.ok ? "ok" : "issues found"}`,
    `Cwd: ${report.cwd}`,
    `Agent scope: ${report.agentScope}`,
  ];

  for (const check of report.checks) {
    lines.push(`- ${iconForStatus(check.status)} ${check.name}: ${check.message}`);
  }
  return lines.join("\n");
}

function countBy(values: string[]): Record<string, number> {
  const counts: Record<string, number> = {};
  for (const value of values) counts[value] = (counts[value] ?? 0) + 1;
  return counts;
}

function formatInvocation(invocation: PiInvocation): string {
  return [invocation.command, ...invocation.args].join(" ");
}

function firstLine(text: string): string {
  return text.split(/\r?\n/)[0];
}

function iconForStatus(status: DoctorCheckStatus): string {
  switch (status) {
    case "ok":
      return "✓";
    case "warn":
      return "!";
    case "error":
      return "✗";
    case "info":
      return "i";
  }
}
