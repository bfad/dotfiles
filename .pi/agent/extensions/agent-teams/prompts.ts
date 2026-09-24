import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

/**
 * Write an agent's system prompt body to a temp file so it can be passed
 * to the spawned pi process via --append-system-prompt.
 */
export function writeAgentPromptFile(agentName: string, prompt: string): string {
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "pi-agent-team-"));
  const safeName = agentName.replace(/[^\w.-]+/g, "_");
  const filePath = path.join(tmpDir, `prompt-${safeName}.md`);
  fs.writeFileSync(filePath, prompt, { encoding: "utf-8", mode: 0o600 });
  return filePath;
}

export function buildInitialAssignmentPrompt(task: string): string {
  return [
    "## Initial assignment from @team_lead",
    "",
    "Follow-up instructions, clarifications, and coordination messages may arrive later through `team_message` as mailbox/team messages. Treat those follow-up messages as updates to this assignment.",
    "",
    "Initial assignment:",
    "",
    "<TASK>",
    task,
    "</TASK>",
  ].join("\n");
}

export function initialAssignmentKickoffMessage(): string {
  return [
    "[INITIAL_ASSIGNMENT_READY]",
    "Your initial assignment from @team_lead has been loaded into your system prompt under \"Initial assignment from @team_lead\".",
    "Start working from that system prompt section now.",
    "Follow-up instructions or clarifications may arrive later through team_message mailbox messages.",
  ].join("\n");
}

export function buildSubagentTaskPrompt(args: { roleName: string; roleFile: string; originalTask: string }): string {
  return [
    "You are a subagent teammate.",
    "",
    `Subagent role: \`${args.roleName}\``,
    `Role file: \`${args.roleFile}\``,
    "",
    "First, read the role file above to understand your expected behavior if your tool set allows reading files. The role instructions may also have been loaded into your system prompt; use them as authoritative instructions for how to perform this task.",
    "",
    "Assigned task:",
    "",
    "<TASK>",
    args.originalTask,
    "</TASK>",
    "",
    "Lifecycle:",
    "",
    "- Work only on this assigned task.",
    "- Report to @team_lead only when you are done, blocked, or need required clarification.",
    "- When done, call `team_shutdown` with your complete final summary.",
    "- Your final summary should include all output requested by the task or role, including files changed, findings, decisions, and open questions where relevant.",
    "- Do not remain idle after completing the task.",
  ].join("\n");
}

export function leadSubagentPromptSection(): string {
  return [
    "\n\n## Subagents",
    "",
    "Agent-teams provides a `subagent` tool. A subagent is a specialized teammate spawned for a focused task using a role or skill Markdown file.",
    "",
    "If instructions mention \"subagents\", \"the subagent extension\", or the `subagent` tool, use the available `subagent` tool. Treat this as satisfying subagent requirements.",
    "",
    "Subagents differ from normal teammates:",
    "",
    "- `team_spawn` creates a normal teammate that may stay alive for ongoing collaboration.",
    "- `subagent` creates a focused subagent teammate with a short lifecycle.",
    "- Subagents are role-driven: they are given a role/skill file and task-specific instructions.",
    "- Subagents should report back only when done, blocked, or when required clarification is needed.",
    "- Subagents are expected to shut down after reporting completion.",
    "",
    "Important orchestration rules:",
    "",
    "- The `subagent` tool returns after spawning. Do not treat the spawn result as the subagent's work product.",
    "- For a single subagent, wait for that subagent's completion report before continuing dependent work.",
    "- For parallel subagents, wait for all spawned subagents to report before synthesizing or continuing dependent work.",
    "- For chain subagents, start one step at a time. After each step reports, substitute its output for `{previous}` in the next step and spawn the next subagent.",
    "- Use `team_spawn` directly only when you intentionally want a normal teammate rather than subagent lifecycle semantics.",
    "",
    "While subagents are working:",
    "",
    "- Your work on the delegated task is paused until the required subagents report.",
    "- As team lead, you are not expected to perform the delegated work yourself, redo their investigation, or do adjacent research while they are working.",
    "- Your job is to stay available for the user and directly help with anything else they may need.",
    "- After spawning the required subagents for a step, tell the user who is working and ask whether there is anything else you can help with while they run.",
    "- Do not monitor subagent panes, repeatedly check status, or intervene just because a subagent is still running.",
    "- Only request shutdown or force shutdown when the user asks, when a subagent reports it is blocked and shutdown is appropriate, or when team status shows the process is no longer alive.",
  ].join("\n");
}

export function teammateSubagentPromptSection(): string {
  const role = process.env.PI_TEAM_SUBAGENT_ROLE || "(unknown)";
  const roleFile = process.env.PI_TEAM_SUBAGENT_FILE || "(not provided)";
  return [
    "\n\n## Subagent Context",
    "",
    "You are a subagent teammate.",
    "",
    "You were spawned by @team_lead using the `subagent` tool. Subagents are specialized teammates with focused, role-driven instructions and a short lifecycle.",
    "",
    `Subagent role: ${role}`,
    `Subagent role file: ${roleFile}`,
    "",
    "Subagent expectations:",
    "",
    "1. Focus only on your assigned task.",
    "2. Follow your subagent role instructions and the lead's task instructions.",
    "3. Do not send progress updates unless you are blocked or need required clarification.",
    "4. Report back to @team_lead when the task is complete, blocked, or requires input.",
    "5. When complete, call `team_shutdown` with your complete final summary.",
    "6. Do not remain idle after completing your task.",
    "",
    "If a role file path was provided, read it before doing task work if your tool set allows it. If your role instructions were also loaded into your system prompt, treat the role file and loaded role instructions as authoritative over generic teammate behavior.",
  ].join("\n");
}
