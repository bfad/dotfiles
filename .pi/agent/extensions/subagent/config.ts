import { SettingsManager, getAgentDir } from "@mariozechner/pi-coding-agent";

const DISABLED_AGENT_CONTROL_CHAR_RE = /[\u0000-\u001F\u007F-\u009F\u2028\u2029]/;

export type SubagentConfigScope = "global" | "project";

export interface SubagentConfig {
  disabledAgents: string[];
  warnings: string[];
  errors: string[];
}

export interface RequestedAgentRef {
  name: string;
  location: string;
}

export interface RejectedAgentRef {
  name: string;
  location: string;
}

export function resolveSubagentConfigFromSettings(
  globalSettings: unknown,
  projectSettings: unknown,
  settingsLoadIssues: Array<{ scope: SubagentConfigScope; message: string }> = [],
): SubagentConfig {
  const errors: string[] = [];
  const warnings: string[] = [];
  const globalDisabled = parseDisabledAgentsFromSettings(globalSettings, "global", errors);
  const projectDisabled = parseDisabledAgentsFromSettings(projectSettings, "project", errors);

  for (const issue of settingsLoadIssues) {
    const message = `${issue.scope} settings failed to load: ${issue.message}`;
    if (issue.scope === "global") errors.push(message);
    else warnings.push(message);
  }

  return {
    disabledAgents: uniqueStable([...globalDisabled, ...projectDisabled]),
    warnings,
    errors,
  };
}

export function loadSubagentConfig(cwd: string): SubagentConfig {
  const manager = SettingsManager.create(cwd, getAgentDir());
  const issues = manager.drainErrors().map((issue) => ({
    scope: issue.scope as SubagentConfigScope,
    message: issue.error.message,
  }));
  return resolveSubagentConfigFromSettings(
    manager.getGlobalSettings(),
    manager.getProjectSettings(),
    issues,
  );
}

export function findRejectedDisabledAgents(
  requested: RequestedAgentRef[],
  disabledAgents: string[],
): RejectedAgentRef[] {
  const disabled = new Set(disabledAgents);
  return requested.filter((ref) => disabled.has(ref.name));
}

export function formatDisabledAgentsError(rejected: RejectedAgentRef[], disabledAgents: string[]): string {
  const rejectedText = rejected.map((ref) => `${ref.name} at ${ref.location}`).join(", ");
  const denylistText = disabledAgents.length > 0 ? disabledAgents.join(", ") : "(empty)";
  return `Blocked by subagent.disabledAgents. Rejected agent${rejected.length === 1 ? "" : "s"}: ${rejectedText}. Active denylist: ${denylistText}.`;
}

function parseDisabledAgentsFromSettings(settings: unknown, scope: SubagentConfigScope, errors: string[]): string[] {
  const root = asOptionalRecord(settings);
  if (!root || root.subagent === undefined) return [];

  if (!isRecord(root.subagent)) {
    errors.push(`${scope} subagent must be an object`);
    return [];
  }

  const disabledAgents = root.subagent.disabledAgents;
  if (disabledAgents === undefined) return [];
  if (!Array.isArray(disabledAgents)) {
    errors.push(`${scope} subagent.disabledAgents must be an array of non-empty strings`);
    return [];
  }

  const parsed: string[] = [];
  for (let index = 0; index < disabledAgents.length; index++) {
    const item = disabledAgents[index];
    if (typeof item !== "string") {
      errors.push(`${scope} subagent.disabledAgents[${index}] must be a string (got ${valueShape(item)})`);
      continue;
    }

    const trimmed = item.trim();
    if (!trimmed) {
      errors.push(`${scope} subagent.disabledAgents[${index}] must be a non-empty string`);
      continue;
    }
    if (DISABLED_AGENT_CONTROL_CHAR_RE.test(trimmed)) {
      errors.push(`${scope} subagent.disabledAgents[${index}] must not contain control characters`);
      continue;
    }

    parsed.push(trimmed);
  }
  return parsed;
}

function uniqueStable(values: string[]): string[] {
  const seen = new Set<string>();
  const unique: string[] = [];
  for (const value of values) {
    if (seen.has(value)) continue;
    seen.add(value);
    unique.push(value);
  }
  return unique;
}

function asOptionalRecord(value: unknown): Record<string, unknown> | undefined {
  return isRecord(value) ? value : undefined;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function valueShape(value: unknown): string {
  if (Array.isArray(value)) return "array";
  if (value === null) return "null";
  return typeof value;
}
