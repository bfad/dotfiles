export const TEAMMATE_REQUIRED_TOOLS = ["team_status", "team_message", "team_broadcast", "team_shutdown"];

export function buildTeammateToolAllowlist(agentTools: string[] | undefined): string | undefined {
  if (!agentTools?.length) return undefined;
  return Array.from(new Set([...agentTools, ...TEAMMATE_REQUIRED_TOOLS])).join(",");
}
