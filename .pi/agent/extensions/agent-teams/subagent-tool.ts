import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import { randomBytes } from "node:crypto";
import * as path from "node:path";
import type { AgentConfig, AgentScope } from "./agents.js";
import { discoverAgents, loadAgentFromPath } from "./agents.js";
import { formatAvailableAgentReferences, formatAvailableSubagents, subagentSlug } from "./subagent-formatters.js";
import { buildSubagentTaskPrompt } from "./prompts.js";
import type { SpawnTeammateOptions, SpawnTeammateResult } from "./spawn-teammate.js";

const MAX_PARALLEL_SUBAGENTS = 8;

const SubagentTaskItem = Type.Object({
  agent: Type.String({ description: "Name of the agent to invoke" }),
  task: Type.String({ description: "Task to delegate to the agent" }),
  cwd: Type.Optional(Type.String({ description: "Working directory for the agent process" })),
});

const SubagentChainItem = Type.Object({
  agent: Type.String({ description: "Name of the agent to invoke" }),
  task: Type.String({ description: "Task with optional {previous} placeholder for prior output" }),
  cwd: Type.Optional(Type.String({ description: "Working directory for the agent process" })),
});

const SubagentParams = Type.Object({
  agent: Type.Optional(Type.String({ description: "Name of the agent to invoke (for single mode)" })),
  task: Type.Optional(Type.String({ description: "Task to delegate (for single mode)" })),
  tasks: Type.Optional(Type.Array(SubagentTaskItem, { description: "Array of {agent, task} for parallel execution" })),
  chain: Type.Optional(Type.Array(SubagentChainItem, { description: "Array of {agent, task} for sequential execution" })),
  agentScope: Type.Optional(
    Type.Union([Type.Literal("user"), Type.Literal("project"), Type.Literal("both")], {
      description: 'Which agent directories to use. Default: "user". Use "both" to include project-local agents.',
      default: "user",
    }),
  ),
  confirmProjectAgents: Type.Optional(
    Type.Boolean({ description: "Prompt before running project-local agents. Default: true.", default: true }),
  ),
  cwd: Type.Optional(Type.String({ description: "Working directory for the agent process (single mode)" })),
});

interface SubagentTaskSpec {
  agent: string;
  task: string;
  cwd?: string;
}

interface SubagentParamsInput {
  agent?: string;
  task?: string;
  tasks?: SubagentTaskSpec[];
  chain?: SubagentTaskSpec[];
  agentScope?: AgentScope;
  confirmProjectAgents?: boolean;
  cwd?: string;
}

export interface SpawnedSubagentInfo {
  name: string;
  agent: string;
  roleFile: string;
}

export type SpawnSubagentTeammate = (
  options: SpawnTeammateOptions,
  ctx: any,
) => Promise<SpawnTeammateResult>;

function shortId(): string {
  return randomBytes(2).toString("hex");
}

function resolveSubagentRole(agentName: string, agents: AgentConfig[], cwd: string): AgentConfig | null {
  const roleFromPath = loadAgentFromPath(agentName, cwd);
  if (roleFromPath) return roleFromPath;
  return agents.find((agent) => agent.name === agentName) ?? null;
}

function isProjectLocalRole(agent: AgentConfig): boolean {
  return agent.source === "project" || agent.source === "agents";
}

export function registerSubagentTool(pi: any, spawnTeammate: SpawnSubagentTeammate): void {
  pi.registerTool({
    name: "subagent",
    label: "Subagent",
    description:
      "Delegate tasks to focused subagent teammates with isolated context. " +
      "A subagent is a specialized teammate spawned with a role file and a short lifecycle: it works on one assigned task, reports back when done or blocked, then shuts down. " +
      "The tool returns after spawning. Once required subagents are running, the lead should not perform the delegated work or adjacent investigation; it should tell the user who is working and ask what else it can help with while waiting for completion reports. " +
      "Modes: single (agent + task), parallel (tasks array), chain (starts the first chain step; the lead continues one step at a time using {previous}).",
    parameters: SubagentParams,

    async execute(_id: string, params: SubagentParamsInput, _signal: unknown, _onUpdate: unknown, ctx: any) {
      const agentScope: AgentScope = params.agentScope ?? "user";
      const cwd = ctx.cwd ?? process.cwd();
      const discovery = discoverAgents(cwd, agentScope);
      const agents = discovery.agents;
      const confirmProjectAgents = params.confirmProjectAgents ?? true;

      const hasChain = (params.chain?.length ?? 0) > 0;
      const hasTasks = (params.tasks?.length ?? 0) > 0;
      const hasSingle = Boolean(params.agent && params.task);
      const modeCount = Number(hasChain) + Number(hasTasks) + Number(hasSingle);
      const hasAnyModeParam =
        params.agent !== undefined || params.task !== undefined || params.tasks !== undefined || params.chain !== undefined;

      const makeError = (text: string, mode: "single" | "parallel" | "chain" | "list" = "single") => ({
        content: [{ type: "text" as const, text }],
        details: { mode, agentScope, spawned: [] },
        isError: true,
      });

      if (modeCount === 0) {
        if (!hasAnyModeParam) {
          return {
            content: [{ type: "text" as const, text: formatAvailableSubagents(agents) }],
            details: { mode: "list", agentScope, agents },
          };
        }
        return makeError(`Invalid parameters. Provide exactly one mode.\nAvailable agents:${formatAvailableAgentReferences(agents)}`);
      }

      if (modeCount !== 1) {
        return makeError(`Invalid parameters. Provide exactly one mode.\nAvailable agents:${formatAvailableAgentReferences(agents)}`);
      }

      if (hasTasks && (params.tasks?.length ?? 0) > MAX_PARALLEL_SUBAGENTS) {
        return makeError(`Too many parallel tasks (${params.tasks?.length ?? 0}). Max is ${MAX_PARALLEL_SUBAGENTS}.`, "parallel");
      }

      const specs: SubagentTaskSpec[] = hasChain
        ? (params.chain ?? [])
        : hasTasks
          ? (params.tasks ?? [])
          : [{ agent: params.agent!, task: params.task!, cwd: params.cwd }];

      const resolved: { spec: SubagentTaskSpec; role: AgentConfig }[] = [];
      for (const spec of specs) {
        const role = resolveSubagentRole(spec.agent, agents, cwd);
        if (!role) {
          return makeError(
            `Unknown agent: "${spec.agent}".\nAvailable agents:${formatAvailableAgentReferences(agents)}`,
            hasChain ? "chain" : hasTasks ? "parallel" : "single",
          );
        }
        resolved.push({ spec, role });
      }

      const projectRoles = resolved
        .map(({ role }) => role)
        .filter(isProjectLocalRole)
        .filter((role, index, roles) => roles.findIndex((candidate) => candidate.filePath === role.filePath) === index);
      if (projectRoles.length > 0 && confirmProjectAgents && ctx.hasUI && ctx.ui?.confirm) {
        const roleNames = projectRoles.map((role) => role.name).join(", ");
        const sources = Array.from(new Set(projectRoles.map((role) => path.dirname(role.filePath))));
        const ok = await ctx.ui.confirm(
          "Run project-local subagent roles?",
          [
            `Roles: ${roleNames}`,
            "Sources:",
            ...sources.map((source) => `- ${source}`),
            "",
            "Project-local roles are repo-controlled prompts. Only continue for trusted repositories.",
          ].join("\n"),
        );
        if (!ok) {
          return {
            content: [{ type: "text" as const, text: "Canceled: project-local subagent roles not approved." }],
            details: { mode: hasChain ? "chain" : hasTasks ? "parallel" : "single", agentScope, spawned: [] },
          };
        }
      }

      const spawnOne = async (spec: SubagentTaskSpec, role: AgentConfig): Promise<SpawnedSubagentInfo | { error: string }> => {
        const wrappedTask = buildSubagentTaskPrompt({
          roleName: role.name,
          roleFile: role.filePath,
          originalTask: spec.task,
        });

        for (let attempt = 0; attempt < 5; attempt++) {
          const requestedName = `subagent-${subagentSlug(role.name)}-${shortId()}`;
          const result = await spawnTeammate(
            {
              requestedName,
              task: wrappedTask,
              cwd: spec.cwd,
              agentName: role.name,
              agentDef: role,
              spawnKind: "subagent",
              subagentRoleFile: role.filePath,
            },
            ctx,
          );
          if (result.ok) return { name: result.name, agent: role.name, roleFile: role.filePath };
          if (!result.text.includes("already exists")) return { error: result.text };
        }
        return { error: `Could not create a unique subagent name for role ${role.name}.` };
      };

      if (hasSingle) {
        const { spec, role } = resolved[0];
        const spawned = await spawnOne(spec, role);
        if ("error" in spawned) return makeError(spawned.error, "single");
        return {
          content: [
            {
              type: "text" as const,
              text: [
                `Spawned subagent @${spawned.name} using role ${role.name}.`,
                "",
                "Role file:",
                role.filePath,
                "",
                "Subagent semantics:",
                "- This result only confirms the subagent was spawned; it is not the subagent's findings.",
                `- Wait for @${spawned.name} to report completion before continuing dependent work.`,
                "- While waiting, do not perform this subagent's work or adjacent research yourself; ask the user if there is anything else you can help with.",
                "- The subagent is expected to report back when done, then shut down.",
              ].join("\n"),
            },
          ],
          details: { mode: "single", agentScope, spawned: [spawned] },
        };
      }

      if (hasTasks) {
        const spawnedSubagents: SpawnedSubagentInfo[] = [];
        for (const { spec, role } of resolved) {
          const spawned = await spawnOne(spec, role);
          if ("error" in spawned) return makeError(spawned.error, "parallel");
          spawnedSubagents.push(spawned);
        }

        const lines = [
          `Spawned ${spawnedSubagents.length} parallel subagents:`,
          "",
          ...spawnedSubagents.map((spawned) => `- @${spawned.name} using role ${spawned.agent}`),
          "",
          "Parallel subagent semantics:",
          "- This result only confirms the subagents were spawned; it is not their findings.",
          "- Wait for all listed subagents to report completion before synthesizing or continuing dependent work.",
          "- While they run, do not perform subagent work or adjacent research yourself; ask the user if there is anything else you can help with.",
          "- Each subagent is expected to report back when done, then shut down.",
        ];
        return {
          content: [{ type: "text" as const, text: lines.join("\n") }],
          details: { mode: "parallel", agentScope, spawned: spawnedSubagents },
        };
      }

      const [{ spec, role }] = resolved;
      const firstSpec = { ...spec, task: spec.task.replace(/\{previous\}/g, "") };
      const spawned = await spawnOne(firstSpec, role);
      if ("error" in spawned) return makeError(spawned.error, "chain");

      const remaining = resolved.slice(1).map(({ spec }) => spec);
      const remainingLines = remaining.length > 0
        ? remaining.map((step, index) => `${index + 2}. ${step.agent} — ${step.task}`)
        : ["(none; wait for step 1's report to complete the chain.)"];
      const chainInstructionLines = remaining.length > 0
        ? [
            "- Then spawn step 2 with {previous} replaced by step 1's report.",
            "- Continue one step at a time until the chain is complete.",
            "- Do not spawn later steps before their inputs are available.",
          ]
        : ["- There are no remaining steps; wait for step 1's report to complete the chain."];
      const chainLines = [
        "Started subagent chain.",
        "",
        `Spawned step 1/${resolved.length}:`,
        `- @${spawned.name} using role ${role.name}`,
        "",
        "Chain subagent semantics:",
        "- This result only confirms step 1 was spawned; it is not the chain output.",
        `- Wait for @${spawned.name} to report completion.`,
        "- While waiting for the current chain step, do not perform that step's work yourself; ask the user if there is anything else you can help with.",
        ...chainInstructionLines,
        "",
        "Remaining steps:",
        ...remainingLines,
      ];
      return {
        content: [{ type: "text" as const, text: chainLines.join("\n") }],
        details: { mode: "chain", agentScope, spawned: [spawned], remainingChain: remaining },
      };
    },

    renderCall(args: SubagentParamsInput, theme: any) {
      const scope: AgentScope = args.agentScope ?? "user";
      if (args.chain && args.chain.length > 0) {
        return new Text(
          theme.fg("toolTitle", theme.bold("subagent ")) +
            theme.fg("accent", `chain (${args.chain.length} steps)`) +
            theme.fg("muted", ` [${scope}]`),
          0,
          0,
        );
      }
      if (args.tasks && args.tasks.length > 0) {
        return new Text(
          theme.fg("toolTitle", theme.bold("subagent ")) +
            theme.fg("accent", `parallel (${args.tasks.length} tasks)`) +
            theme.fg("muted", ` [${scope}]`),
          0,
          0,
        );
      }
      const agentName = args.agent || "list";
      return new Text(
        theme.fg("toolTitle", theme.bold("subagent ")) +
          theme.fg("accent", String(agentName)) +
          theme.fg("muted", ` [${scope}]`),
        0,
        0,
      );
    },

    renderResult(result: any, _opts: unknown, theme: any) {
      const details = result.details as { mode?: string; spawned?: SpawnedSubagentInfo[] } | undefined;
      if (!details?.spawned || details.spawned.length === 0) {
        const t = result.content[0];
        return new Text(t?.type === "text" ? t.text : "", 0, 0);
      }
      const label = details.mode === "parallel"
        ? `spawned ${details.spawned.length} subagents`
        : details.mode === "chain"
          ? "started chain"
          : "spawned subagent";
      let text = theme.fg("success", "✓ ") + theme.fg("toolTitle", label);
      for (const spawned of details.spawned) {
        text += `\n  ${theme.fg("accent", `@${spawned.name}`)} ${theme.fg("muted", `(${spawned.agent})`)}`;
      }
      return new Text(text, 0, 0);
    },
  });
}
