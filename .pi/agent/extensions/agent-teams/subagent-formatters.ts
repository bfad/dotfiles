import type { AgentConfig } from "./agents.js";

export function subagentSlug(roleName: string): string {
  const slug = roleName.toLowerCase().replace(/[^a-z0-9_-]+/g, "-").replace(/^-+|-+$/g, "");
  return slug || "role";
}

export function formatAgentReference(agent: AgentConfig): string[] {
  return [`- ${agent.name} (${agent.source}): ${agent.description}`, `  file: ${agent.filePath}`];
}

export function formatAvailableSubagents(agents: AgentConfig[]): string {
  const lines = ["Available subagent roles:", ""];
  if (agents.length === 0) {
    lines.push("- none");
  } else {
    for (const agent of agents) {
      lines.push(...formatAgentReference(agent));
    }
  }
  lines.push(
    "",
    "Use subagent with:",
    "- { agent, task } for one subagent",
    "- { tasks: [...] } for parallel subagents",
    "- { chain: [...] } to start a sequential subagent chain",
  );
  return lines.join("\n");
}

export function formatAvailableAgentReferences(agents: AgentConfig[]): string {
  if (agents.length === 0) return " none";
  return "\n" + agents.flatMap(formatAgentReference).join("\n");
}
