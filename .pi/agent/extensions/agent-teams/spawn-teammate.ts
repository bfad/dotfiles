import { execSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import { resolveSpawnCwd } from "./spawn-cwd.js";
import type { AgentConfig } from "./agents.js";
import type { ResolvedTeammateModel } from "./model-validation.js";
import { validateTeammateModel } from "./model-validation.js";
import { selectTeammateAnchor } from "./pane-anchor.js";
import { createPaneEnvHandoff } from "./pane-env.js";
import { createPaneLaunchHandoff } from "./pane-launcher.js";
import type { PaneManager } from "./pane-manager.js";
import { RpcTeammate } from "./rpc-teammate.js";
import { makeExitCleanup, runSpawnTransaction, type RpcRef } from "./spawn-lifecycle.js";
import { shellEscape } from "./shared-utils.js";
import * as team from "./team.js";
import type { MemberConfig } from "./types.js";
import { buildInitialAssignmentPrompt, initialAssignmentKickoffMessage, writeAgentPromptFile } from "./prompts.js";
import { buildTeammateToolAllowlist } from "./tool-allowlist.js";
import { EXTRA_ARGS_ENV, parseExtraArgs } from "./extra-args.js";
import { appendPaneChildPiEnv } from "./child-pi-env.js";
import {
  PI_TEAM_INSTANCE_ID_ENV,
  PI_TEAM_SESSION_DIR_ENV,
  privateSessionDir,
  resolveAgentDir,
} from "./private-sessions.js";

export interface SpawnTeammateOptions {
  requestedName: string;
  task: string;
  teamName?: string;
  model?: string;
  cwd?: string;
  agentName?: string;
  agentDef?: AgentConfig;
  spawnKind: "teammate" | "subagent";
  subagentRoleFile?: string;
}

export type SpawnTeammateResult =
  | {
      ok: true;
      teamName: string;
      member: MemberConfig;
      name: string;
      effectiveModel: string | undefined;
      taskSummary: string;
      rpcMode: boolean;
      identityNote: string;
    }
  | { ok: false; text: string };

export interface SpawnTeammateDeps {
  paneManager: PaneManager | null;
  rpcTeammates: Map<string, RpcTeammate>;
  ensureTeam(name?: string): string;
  getAgentRoster(): AgentConfig[];
  getCurrentTeamName(): string | null;
  nonce?: () => string;
  createPaneEnvHandoff?: typeof createPaneEnvHandoff;
  createPaneLaunchHandoff?: typeof createPaneLaunchHandoff;
  registerPaneEnvHandoff(instanceId: string, cleanup: () => void): void;
  cleanupPaneEnvHandoff(instanceId: string): void;
  updateTeammateStatus(): void;
  requestOverlayRender(): void;
  emitTeamSpawn(
    teamName: string,
    name: string,
    model: string | null | undefined,
    task: string,
    cwd: string,
    spawnedAt: number,
    instanceId: string,
    sessionDir: string,
  ): void;
  emitTeamRemove(teamName: string, name: string): void;
}

function sanitizeTeammateName(name: string): string {
  const safeName = name.replace(/[^a-zA-Z0-9_-]/g, "_");
  return safeName || "teammate";
}

export function summarizeTask(task: string): string {
  return task.length > 120
    ? task.slice(0, 120).replace(/\n.*/s, "") + "…"
    : task.replace(/\n.*/s, "");
}

function buildSubagentEnv(options: SpawnTeammateOptions, agentName: string | undefined, safeName: string): Record<string, string> {
  if (options.spawnKind !== "subagent") return {};

  return {
    PI_TEAM_SPAWN_KIND: "subagent",
    PI_TEAM_SUBAGENT_ROLE: agentName ?? safeName,
    PI_TEAM_SUBAGENT_FILE: options.subagentRoleFile ?? options.agentDef?.filePath ?? "",
  };
}

function agentIdentityNote(agentDef: AgentConfig | undefined, effectiveModel: string | undefined): string {
  return agentDef
    ? ` Loaded agent identity (model: ${effectiveModel ?? "inherited"}, ${agentDef.systemPrompt.length} chars prompt).`
    : "";
}

function tryWriteSystemPromptFile(name: string, prompt: string | undefined): string | undefined {
  if (!prompt?.trim()) return undefined;
  try {
    return writeAgentPromptFile(name, prompt);
  } catch {
    return undefined;
  }
}

interface SystemPromptFiles {
  paths: string[];
  initialAssignmentLoaded: boolean;
}

function buildSystemPromptFiles(options: SpawnTeammateOptions, safeName: string, agentDef: AgentConfig | undefined): SystemPromptFiles {
  const paths: string[] = [];
  const identityPromptPath = tryWriteSystemPromptFile(agentDef?.name ?? safeName, agentDef?.systemPrompt);
  if (identityPromptPath) paths.push(identityPromptPath);

  const assignmentPromptPath = options.spawnKind === "teammate"
    ? tryWriteSystemPromptFile(`${safeName}-initial-assignment`, buildInitialAssignmentPrompt(options.task))
    : undefined;
  if (assignmentPromptPath) paths.push(assignmentPromptPath);

  return { paths, initialAssignmentLoaded: Boolean(assignmentPromptPath) };
}

function initialMailboxMessage(options: SpawnTeammateOptions, initialAssignmentLoaded: boolean): string {
  if (initialAssignmentLoaded) return initialAssignmentKickoffMessage();
  return options.task;
}

export async function spawnTeammate(
  options: SpawnTeammateOptions,
  deps: SpawnTeammateDeps,
  ctx: any,
): Promise<SpawnTeammateResult> {
  const safeName = sanitizeTeammateName(options.requestedName);
  let extraArgs: string[];
  try {
    extraArgs = parseExtraArgs(process.env[EXTRA_ARGS_ENV]);
  } catch (error) {
    return {
      ok: false,
      text: error instanceof Error ? error.message : `Invalid ${EXTRA_ARGS_ENV}.`,
    };
  }

  // Agent identity lookup. team_spawn still uses teammate name as role name;
  // subagent passes its role definition explicitly while keeping a unique instance name.
  const agentDef = options.agentDef ?? deps.getAgentRoster().find((a) => a.name === safeName);
  const agentName = options.agentName ?? agentDef?.name;

  // Model: caller's choice wins, then agent definition, then inherit.
  const requestedModel = options.model ?? agentDef?.model;
  const modelValidation = validateTeammateModel(requestedModel, ctx?.modelRegistry);
  if (!modelValidation.ok) return { ok: false, text: modelValidation.text };

  const resolvedModel = modelValidation.model;
  const effectiveModel = resolvedModel?.cliModel;

  const teamName = deps.ensureTeam(options.teamName);
  const config = team.readConfig(teamName);
  if (!config) {
    return {
      ok: false,
      text: `Failed to create or read team config for "${teamName}". Try again.`,
    };
  }

  // Update lead status on first spawn
  if (config.members.length === 1) {
    ctx.ui.setStatus("agent-teams", `@team_lead [${teamName}]`);
  }

  if (config.members.some((m) => m.name === safeName) || deps.rpcTeammates.has(safeName)) {
    return { ok: false, text: `Teammate @${safeName} already exists.` };
  }

  let spawnCwd: string;
  try {
    spawnCwd = resolveSpawnCwd(options.cwd, config.cwd, ctx.cwd ?? process.cwd());
  } catch (error) {
    return {
      ok: false,
      text: error instanceof Error ? error.message : "Invalid spawn cwd.",
    };
  }

  const systemPromptFiles = buildSystemPromptFiles(options, safeName, agentDef);
  const initialMessage = initialMailboxMessage(options, systemPromptFiles.initialAssignmentLoaded);
  const instanceId = (deps.nonce ?? randomUUID)();
  let sessionDir: string;
  try {
    sessionDir = privateSessionDir(instanceId);
  } catch (error) {
    return {
      ok: false,
      text: error instanceof Error ? error.message : "Invalid teammate instance ID.",
    };
  }
  const taskSummary = summarizeTask(options.task);
  const extraEnv = buildSubagentEnv(options, agentName, safeName);

  if (deps.paneManager) {
    const paneResult = await spawnPaneTeammate({
      teamName,
      config,
      safeName,
      spawnCwd,
      taskSummary,
      effectiveModel,
      resolvedModel,
      agentDef,
      agentName,
      extraEnv,
      systemPromptPaths: systemPromptFiles.paths,
      deps,
      ctx,
      spawnKind: options.spawnKind,
      instanceId,
      sessionDir,
      extraArgs,
      initialMessage,
    });
    return paneResult;
  }

  return spawnRpcTeammate({
    teamName,
    safeName,
    spawnCwd,
    taskSummary,
    effectiveModel,
    resolvedModel,
    agentDef,
    agentName,
    extraEnv,
    systemPromptPaths: systemPromptFiles.paths,
    deps,
    spawnKind: options.spawnKind,
    instanceId,
    sessionDir,
    extraArgs,
    initialMessage,
  });
}

interface SpawnCommonArgs {
  teamName: string;
  safeName: string;
  spawnCwd: string;
  taskSummary: string;
  effectiveModel: string | undefined;
  resolvedModel: ResolvedTeammateModel | undefined;
  agentDef: AgentConfig | undefined;
  agentName: string | undefined;
  extraEnv: Record<string, string>;
  systemPromptPaths: string[];
  deps: SpawnTeammateDeps;
  spawnKind: "teammate" | "subagent";
  instanceId: string;
  sessionDir: string;
  extraArgs: string[];
  initialMessage: string;
}

interface SpawnPaneArgs extends SpawnCommonArgs {
  config: team.TeamConfig;
  ctx: any;
}

function finalizeReservedMember(args: SpawnCommonArgs, member: MemberConfig): "running" | "skipped-mismatch" {
  const config = team.readConfig(args.teamName);
  const reserved = config?.members.find(
    (candidate) => candidate.name === args.safeName && candidate.instanceId === args.instanceId,
  );
  if (!config || !reserved) return "skipped-mismatch";
  Object.assign(reserved, member, { state: "starting" as const });
  team.writeConfig(args.teamName, config);
  const result = team.finalizeMemberRegistration(args.teamName, args.safeName, args.instanceId);
  if (result === "running") member.state = "running";
  return result;
}

function removeReservedMember(args: SpawnCommonArgs): void {
  team.removeMemberRegistration(args.teamName, args.safeName, args.instanceId);
}

function deleteKickoff(teamName: string, memberName: string, kickoffId: string): void {
  const result = team.deleteMailboxMessages(teamName, memberName, [kickoffId]);
  if (result.failed.length > 0) throw new Error(`Failed to delete kickoff ${kickoffId}`);
}

function markReservedMemberStopping(args: SpawnCommonArgs, member: MemberConfig): void {
  const result = team.markMemberStopping(args.teamName, args.safeName, args.instanceId);
  if (result === "stopping") {
    const config = team.readConfig(args.teamName);
    const reserved = config?.members.find(
      (candidate) => candidate.name === args.safeName && candidate.instanceId === args.instanceId,
    );
    if (config && reserved) {
      Object.assign(reserved, member, { state: "stopping" as const });
      team.writeConfig(args.teamName, config);
    }
  }
  member.state = "stopping";
}

async function spawnPaneTeammate(args: SpawnPaneArgs): Promise<SpawnTeammateResult> {
  const paneManager = args.deps.paneManager;
  if (!paneManager) return { ok: false, text: "No pane manager available." };

  const agentDir = resolveAgentDir(process.env.PI_CODING_AGENT_DIR);
  const envParts = [
    `PI_TEAM_NAME=${shellEscape(args.teamName)}`,
    "PI_TEAM_ROLE=teammate",
    `PI_TEAM_AGENT_NAME=${shellEscape(args.safeName)}`,
    `${PI_TEAM_INSTANCE_ID_ENV}=${shellEscape(args.instanceId)}`,
    `${PI_TEAM_SESSION_DIR_ENV}=${shellEscape(args.sessionDir)}`,
  ];
  for (const [key, value] of Object.entries(args.extraEnv)) {
    envParts.push(`${key}=${shellEscape(value)}`);
  }
  envParts.push(`PI_CODING_AGENT_DIR=${shellEscape(agentDir)}`);
  if (process.env.PATH) {
    envParts.push(`PATH=${shellEscape(process.env.PATH)}`);
  }
  const childEnvParts = appendPaneChildPiEnv(envParts);

  let piExe = "pi";
  try {
    execSync("command -v devx", { stdio: "ignore" });
    piExe = "devx pi";
  } catch {
    /* devx not available */
  }

  const modelFlag = args.resolvedModel ? `--model ${shellEscape(args.resolvedModel.cliModel)}` : "";
  const teammateTools = buildTeammateToolAllowlist(args.agentDef?.tools);
  const toolsFlag = teammateTools ? `--tools ${shellEscape(teammateTools)}` : "";
  const systemPromptFlags = args.systemPromptPaths.map((promptPath) => `--append-system-prompt ${shellEscape(promptPath)}`);
  const extraArgFlags = args.extraArgs.map(shellEscape);

  let teammates = args.config.members.filter((m) => m.role === "teammate");

  if (!paneManager.isPaneAlive(args.config.leadPaneId)) {
    const freshPaneId = paneManager.getCurrentPaneId();
    args.config.leadPaneId = freshPaneId;
    const lead = args.config.members.find((m) => m.role === "lead");
    if (lead) lead.paneId = freshPaneId;
    team.writeConfig(args.teamName, args.config);
  }

  const teammateAnchor = selectTeammateAnchor(teammates, args.config.leadPaneId, paneManager);

  const wasFirstTeammate = teammates.length === 0;
  const staggerSecs = teammates.length * 3;
  const sleepPart = staggerSecs > 0 ? `sleep ${staggerSecs} &&` : "";
  const paneEnvHandoff = (args.deps.createPaneEnvHandoff ?? createPaneEnvHandoff)();
  let paneLaunchHandoff: ReturnType<typeof createPaneLaunchHandoff> | undefined;
  const startupArtifactCleanups = new Set<() => void>([paneEnvHandoff.cleanup]);
  const cleanupStartupArtifacts = (): void => {
    const errors: unknown[] = [];
    for (const cleanup of startupArtifactCleanups) {
      try {
        cleanup();
      } catch (error) {
        errors.push(error);
      }
    }
    if (errors.length > 0) throw errors[0];
  };
  try {
    args.deps.registerPaneEnvHandoff(args.instanceId, cleanupStartupArtifacts);
  } catch (error) {
    cleanupStartupArtifacts();
    throw error;
  }
  const piCmd = [
    `cd ${shellEscape(args.spawnCwd)}`,
    "&&",
    paneEnvHandoff.loadCommand,
    "&&",
    sleepPart,
    paneEnvHandoff.exportCommand,
    "&&",
    ...childEnvParts,
    "exec",
    piExe,
    "--session-dir",
    shellEscape(args.sessionDir),
    modelFlag,
    toolsFlag,
    ...systemPromptFlags,
    ...extraArgFlags,
  ]
    .filter(Boolean)
    .join(" ");

  try {
    paneLaunchHandoff = (args.deps.createPaneLaunchHandoff ?? createPaneLaunchHandoff)(piCmd);
    startupArtifactCleanups.add(paneLaunchHandoff.cleanup);
  } catch (error) {
    cleanupStartupArtifacts();
    throw error;
  }

  // Keep the command sent through pane-manager backends bounded. PATH and the
  // other child environment assignments remain in the launch script so child Pi
  // inherits the lead process PATH without exposing it to terminal command
  // length limits.
  const paneCommand = paneLaunchHandoff.command;

  const member = buildMember({ ...args, paneId: "pending", transport: paneManager.kind });
  let paneId: string | undefined;
  let paneCreationUncertain = false;
  const paneCreationObserver = {
    onPaneCreated: (createdPaneId: string) => {
      paneId = createdPaneId;
      member.paneId = createdPaneId;
    },
    onPaneCreationUncertain: () => {
      paneCreationUncertain = true;
    },
    onStartupArtifactCreated: (cleanup: () => void) => {
      startupArtifactCleanups.add(cleanup);
    },
  };
  let result: ReturnType<typeof runSpawnTransaction>;
  try {
    result = runSpawnTransaction({
      memberName: args.safeName,
      reserve: () => team.reserveMember(args.teamName, member),
      kickoff: () => team.sendMessage(args.teamName, "team_lead", args.safeName, args.initialMessage),
      start: () => {
        const returnedPaneId = teammateAnchor
          ? paneManager.splitForAdditionalTeammate(
              teammateAnchor.paneId,
              paneCommand,
              `@${args.safeName}`,
              paneCreationObserver,
            )
          : paneManager.splitForFirstTeammate(
              args.config.leadPaneId,
              paneCommand,
              `@${args.safeName}`,
              paneCreationObserver,
            );
        if (!paneId) paneCreationObserver.onPaneCreated(returnedPaneId);
        const activePaneId = paneId ?? returnedPaneId;
        paneManager.setPaneTitle(activePaneId, `@${args.safeName}`);
        paneManager.setTeamMetadata?.(activePaneId, {
          teamName: args.teamName,
          memberName: args.safeName,
          role: "teammate",
          status: args.taskSummary,
        });
        // Label the lead too, so the whole team reads as a group in the backend UI.
        if (wasFirstTeammate) {
          paneManager.setTeamMetadata?.(args.config.leadPaneId, {
            teamName: args.teamName,
            memberName: "team_lead",
            role: "lead",
          });
        }
      },
      finalize: () => finalizeReservedMember(args, member),
      retire: () => {
        if (!paneId) return paneCreationUncertain ? "unconfirmed" : "non-consumption-proven";
        paneManager.killPane(paneId);
        return paneManager.isPaneAlive(paneId) ? "unconfirmed" : "confirmed-dead";
      },
      cleanup: () => removeReservedMember(args),
      deleteKickoff: (kickoffId) => deleteKickoff(args.teamName, args.safeName, kickoffId),
      markStopping: () => markReservedMemberStopping(args, member),
    });
  } catch (error) {
    args.deps.cleanupPaneEnvHandoff(args.instanceId);
    throw error;
  }
  if (!result.ok) {
    args.deps.cleanupPaneEnvHandoff(args.instanceId);
    return { ok: false, text: result.error.message };
  }

  args.deps.emitTeamSpawn(
    args.teamName,
    args.safeName,
    args.effectiveModel,
    args.taskSummary,
    args.spawnCwd,
    member.spawnedAt,
    args.instanceId,
    args.sessionDir,
  );

  if (paneManager.kind === "iterm" && wasFirstTeammate) {
    const { loadITermLayout } = await import("./iterm.js");
    const layout = loadITermLayout();
    args.ctx.ui.notify(`Using iTerm2 native panes (${layout} mode). Change layout in ~/.pi/teams/config.json`, "info");
  }

  if (paneManager.kind === "ghostty" && wasFirstTeammate) {
    args.ctx.ui.notify("Using Ghostty native panes (AppleScript). Pane capture is unsupported — /team_diagnose is unavailable.", "info");
  }

  return {
    ok: true,
    teamName: args.teamName,
    member,
    name: args.safeName,
    effectiveModel: args.effectiveModel,
    taskSummary: args.taskSummary,
    rpcMode: false,
    identityNote: agentIdentityNote(args.agentDef, args.effectiveModel),
  };
}

function spawnRpcTeammate(args: SpawnCommonArgs): SpawnTeammateResult {
  const toolsStr = buildTeammateToolAllowlist(args.agentDef?.tools);
  const rpcRef: RpcRef<RpcTeammate> = { current: undefined };
  const exitCleanup = makeExitCleanup({
    name: args.safeName,
    teamName: args.teamName,
    instanceId: args.instanceId,
    rpcRef,
    rpcTeammates: args.deps.rpcTeammates,
    removeMemberRegistration: (teamName, name, instanceId) => team.removeMemberRegistration(teamName, name, instanceId),
    emitTeamRemove: args.deps.emitTeamRemove,
    updateTeammateStatus: args.deps.updateTeammateStatus,
  });
  const rpcTeammate = new RpcTeammate(
    {
      teamName: args.teamName,
      name: args.safeName,
      cwd: args.spawnCwd,
      instanceId: args.instanceId,
      sessionDir: args.sessionDir,
      model: args.resolvedModel?.cliModel,
      tools: toolsStr,
      systemPromptPaths: args.systemPromptPaths,
      extraEnv: args.extraEnv,
      extraArgs: args.extraArgs,
    },
    () => {
      args.deps.requestOverlayRender();
      args.deps.updateTeammateStatus();
    },
    exitCleanup,
  );
  rpcRef.current = rpcTeammate;

  const member = buildMember({ ...args, paneId: "none", transport: "rpc" });
  let workerLaunched = false;
  const result = runSpawnTransaction({
    memberName: args.safeName,
    reserve: () => team.reserveMember(args.teamName, member),
    kickoff: () => team.sendMessage(args.teamName, "team_lead", args.safeName, args.initialMessage),
    start: () => {
      args.deps.rpcTeammates.set(args.safeName, rpcTeammate);
      rpcTeammate.start();
      workerLaunched = true;
      if (rpcTeammate.pid) member.pid = rpcTeammate.pid;
    },
    finalize: () => finalizeReservedMember(args, member),
    retire: () => {
      if (!workerLaunched && !rpcTeammate.isAlive) return "non-consumption-proven";
      if (rpcTeammate.isAlive) {
        rpcTeammate.kill();
        return "unconfirmed";
      }
      return "confirmed-dead";
    },
    cleanup: () => {
      if (args.deps.rpcTeammates.get(args.safeName) === rpcTeammate) {
        args.deps.rpcTeammates.delete(args.safeName);
      }
      removeReservedMember(args);
    },
    deleteKickoff: (kickoffId) => deleteKickoff(args.teamName, args.safeName, kickoffId),
    markStopping: () => markReservedMemberStopping(args, member),
  });
  if (!result.ok) return { ok: false, text: result.error.message };

  args.deps.emitTeamSpawn(
    args.teamName,
    args.safeName,
    args.effectiveModel,
    args.taskSummary,
    args.spawnCwd,
    member.spawnedAt,
    args.instanceId,
    args.sessionDir,
  );

  return {
    ok: true,
    teamName: args.teamName,
    member,
    name: args.safeName,
    effectiveModel: args.effectiveModel,
    taskSummary: args.taskSummary,
    rpcMode: true,
    identityNote: agentIdentityNote(args.agentDef, args.effectiveModel),
  };
}

function buildMember(
  args: SpawnCommonArgs & { paneId: string; transport: MemberConfig["transport"]; pid?: number },
): MemberConfig {
  return {
    name: args.safeName,
    role: "teammate",
    paneId: args.paneId,
    transport: args.transport,
    ...(args.pid ? { pid: args.pid } : {}),
    task: args.taskSummary,
    spawnKind: args.spawnKind,
    ...(args.agentName ? { agent: args.agentName } : {}),
    cwd: args.spawnCwd,
    ...(args.effectiveModel ? { model: args.effectiveModel } : {}),
    spawnedAt: Date.now(),
    instanceId: args.instanceId,
    sessionDir: args.sessionDir,
  };
}
