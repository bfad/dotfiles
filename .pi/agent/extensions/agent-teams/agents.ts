/**
 * Agent discovery and configuration.
 *
 * Discovery order (later wins on name conflicts):
 *   1. Package agents — agents/ dirs from installed packages in settings.json
 *   2. User agents — ~/.pi/agent/agents/
 *   3. Project agents — nearest .pi/agents/ (only with scope "project" or "both")
 *   4. Project .agents roles — nearest .agents/agents and skill-local agents dirs
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { getAgentDir, parseFrontmatter } from "@mariozechner/pi-coding-agent";
import { resolvePackageRoot } from "../../lib/packages.js";

export type AgentScope = "user" | "project" | "both";
export type AgentSource = "package" | "user" | "project" | "agents";

export interface AgentConfig {
  name: string;
  description: string;
  tools?: string[];
  model?: string;
  systemPrompt: string;
  source: AgentSource;
  filePath: string;
}

export interface AgentDiscoveryResult {
  agents: AgentConfig[];
  /** First project-local role directory, kept for existing callers. */
  projectAgentsDir: string | null;
  /** All project-local role directories discovered for confirmation prompts. */
  projectAgentDirs: string[];
}

interface LoadAgentOptions {
  allowMissingMetadata?: boolean;
}

function asString(value: unknown): string | undefined {
  return typeof value === "string" ? value : undefined;
}

function normalizeModel(value: unknown): string | undefined {
  const model = asString(value)?.trim();
  if (!model || model.toLowerCase() === "inherit") return undefined;
  return model;
}

function normalizeToolName(value: string): string | null {
  const trimmed = value.trim();
  if (!trimmed) return null;

  // Claude-style allowed-tools entries may include constraints, e.g. Bash(git:*)
  // or Read(/tmp/**). Preserve the base capability when it has a Pi equivalent.
  const base = trimmed.replace(/\(.+$/, "").trim();
  const normalized: Record<string, string> = {
    Bash: "bash",
    bash: "bash",
    Read: "read",
    read: "read",
    Write: "write",
    write: "write",
    Edit: "edit",
    edit: "edit",
    MultiEdit: "edit",
    Grep: "grep",
    grep: "grep",
    Glob: "find",
    glob: "find",
    Find: "find",
    find: "find",
    LS: "ls",
    Ls: "ls",
    ls: "ls",
  };

  return normalized[base] ?? trimmed;
}

function parseToolList(value: unknown): string[] | undefined {
  let rawTools: string[];
  if (Array.isArray(value)) {
    rawTools = value.map((tool) => String(tool));
  } else if (typeof value === "string") {
    rawTools = value.split(",");
  } else {
    return undefined;
  }

  const tools = rawTools
    .map(normalizeToolName)
    .filter((tool): tool is string => Boolean(tool));
  const uniqueTools = Array.from(new Set(tools));
  return uniqueTools.length > 0 ? uniqueTools : undefined;
}

function deriveAgentName(filePath: string): string {
  return path.basename(filePath, path.extname(filePath));
}

function loadAgentFromFile(
  filePath: string,
  source: AgentSource,
  options: LoadAgentOptions = {},
): AgentConfig | null {
  let content: string;
  try {
    content = fs.readFileSync(filePath, "utf-8");
  } catch {
    return null;
  }

  const { frontmatter, body } = parseFrontmatter<Record<string, unknown>>(content);
  const name = asString(frontmatter.name)?.trim() || (options.allowMissingMetadata ? deriveAgentName(filePath) : "");
  const description =
    asString(frontmatter.description)?.trim() ||
    (options.allowMissingMetadata ? `Subagent role file ${path.basename(filePath)}` : "");

  if (!name || !description) return null;

  const tools = parseToolList(frontmatter.tools) ?? parseToolList(frontmatter["allowed-tools"]);

  return {
    name,
    description,
    tools,
    model: normalizeModel(frontmatter.model),
    systemPrompt: body,
    source,
    filePath,
  };
}

function loadAgentsFromDir(dir: string, source: AgentSource): AgentConfig[] {
  const agents: AgentConfig[] = [];

  if (!fs.existsSync(dir)) return agents;

  let entries: fs.Dirent[];
  try {
    entries = fs.readdirSync(dir, { withFileTypes: true });
  } catch {
    return agents;
  }

  for (const entry of entries.sort((a, b) => a.name.localeCompare(b.name))) {
    if (!entry.name.endsWith(".md")) continue;
    if (!entry.isFile() && !entry.isSymbolicLink()) continue;

    const agent = loadAgentFromFile(path.join(dir, entry.name), source);
    if (agent) agents.push(agent);
  }

  return agents;
}

function isDirectory(p: string): boolean {
  try {
    return fs.statSync(p).isDirectory();
  } catch {
    return false;
  }
}

function findNearestProjectAgentsDir(cwd: string): string | null {
  let currentDir = path.resolve(cwd);
  while (true) {
    const candidate = path.join(currentDir, ".pi", "agents");
    if (isDirectory(candidate)) return candidate;

    const parentDir = path.dirname(currentDir);
    if (parentDir === currentDir) return null;
    currentDir = parentDir;
  }
}

function readSortedDirectories(dir: string): fs.Dirent[] {
  try {
    return fs
      .readdirSync(dir, { withFileTypes: true })
      .filter((entry) => entry.isDirectory() || entry.isSymbolicLink())
      .sort((a, b) => a.name.localeCompare(b.name));
  } catch {
    return [];
  }
}

function listDotAgentsDirs(root: string): string[] {
  const dotAgentsDir = path.join(root, ".agents");
  if (!isDirectory(dotAgentsDir)) return [];

  const dirs: string[] = [];
  const rootAgentsDir = path.join(dotAgentsDir, "agents");
  if (isDirectory(rootAgentsDir)) dirs.push(rootAgentsDir);

  const skillsDir = path.join(dotAgentsDir, "skills");
  if (!isDirectory(skillsDir)) return dirs;

  for (const skillEntry of readSortedDirectories(skillsDir)) {
    const skillDir = path.join(skillsDir, skillEntry.name);
    const skillAgentsDir = path.join(skillDir, "agents");
    if (isDirectory(skillAgentsDir)) dirs.push(skillAgentsDir);

    // Bounded support for skill-local files nested one extra level, e.g.
    // .agents/skills/vendor/skill-name/agents/*.md. Do not scan arbitrary depth.
    for (const nestedEntry of readSortedDirectories(skillDir)) {
      const nestedAgentsDir = path.join(skillDir, nestedEntry.name, "agents");
      if (isDirectory(nestedAgentsDir)) dirs.push(nestedAgentsDir);
    }
  }

  return dirs;
}

function findNearestDotAgentsDirs(cwd: string): string[] {
  let currentDir = path.resolve(cwd);
  while (true) {
    const dirs = listDotAgentsDirs(currentDir);
    if (dirs.length > 0) return dirs;

    const parentDir = path.dirname(currentDir);
    if (parentDir === currentDir) return [];
    currentDir = parentDir;
  }
}

/** Discover agents from all installed packages' agents/ directories. */
function discoverPackageAgents(agentDir: string): AgentConfig[] {
  const settingsPath = path.join(agentDir, "settings.json");
  if (!fs.existsSync(settingsPath)) return [];

  let settings: { packages?: (string | { source?: string })[] };
  try {
    settings = JSON.parse(fs.readFileSync(settingsPath, "utf-8"));
  } catch {
    return [];
  }

  const agents: AgentConfig[] = [];
  for (const entry of settings.packages ?? []) {
    const source = typeof entry === "string" ? entry : entry?.source;
    if (!source) continue;

    const pkgRoot = resolvePackageRoot(source, settingsPath, agentDir);
    if (!pkgRoot || !isDirectory(pkgRoot)) continue;

    agents.push(...loadAgentsFromDir(path.join(pkgRoot, "agents"), "package"));
  }

  return agents;
}

function expandHome(filePath: string): string {
  if (filePath === "~") return os.homedir();
  if (filePath.startsWith(`~${path.sep}`)) return path.join(os.homedir(), filePath.slice(2));
  return filePath;
}

export function isAgentPathReference(agent: string): boolean {
  return (
    path.isAbsolute(agent) ||
    agent.startsWith(".") ||
    agent.startsWith("~") ||
    agent.includes("/") ||
    agent.endsWith(".md")
  );
}

export function loadAgentFromPath(agentPath: string, cwd: string): AgentConfig | null {
  if (!isAgentPathReference(agentPath)) return null;

  const expanded = expandHome(agentPath);
  const filePath = path.isAbsolute(expanded) ? expanded : path.resolve(cwd, expanded);
  if (!filePath.endsWith(".md")) return null;

  return loadAgentFromFile(filePath, "project", { allowMissingMetadata: true });
}

export function discoverAgents(cwd: string, scope: AgentScope): AgentDiscoveryResult {
  const agentDir = getAgentDir();
  const userDir = path.join(agentDir, "agents");
  const projectPiAgentsDir = findNearestProjectAgentsDir(cwd);
  const dotAgentsDirs = findNearestDotAgentsDirs(cwd);
  const projectAgentDirs = [projectPiAgentsDir, ...dotAgentsDirs].filter((dir): dir is string => Boolean(dir));

  const packageAgents = scope === "project" ? [] : discoverPackageAgents(agentDir);
  const userAgents = scope === "project" ? [] : loadAgentsFromDir(userDir, "user");
  const projectPiAgents =
    scope === "user" || !projectPiAgentsDir ? [] : loadAgentsFromDir(projectPiAgentsDir, "project");
  const dotAgents =
    scope === "user" ? [] : dotAgentsDirs.flatMap((dir) => loadAgentsFromDir(dir, "agents"));

  const agentMap = new Map<string, AgentConfig>();

  if (scope === "both") {
    for (const agent of packageAgents) agentMap.set(agent.name, agent);
    for (const agent of userAgents) agentMap.set(agent.name, agent);
    for (const agent of projectPiAgents) agentMap.set(agent.name, agent);
    for (const agent of dotAgents) agentMap.set(agent.name, agent);
  } else if (scope === "user") {
    for (const agent of packageAgents) agentMap.set(agent.name, agent);
    for (const agent of userAgents) agentMap.set(agent.name, agent);
  } else {
    for (const agent of projectPiAgents) agentMap.set(agent.name, agent);
    for (const agent of dotAgents) agentMap.set(agent.name, agent);
  }

  return {
    agents: Array.from(agentMap.values()),
    projectAgentsDir: projectAgentDirs[0] ?? null,
    projectAgentDirs,
  };
}

export function formatAgentList(agents: AgentConfig[], maxItems: number): { text: string; remaining: number } {
  if (agents.length === 0) return { text: "none", remaining: 0 };
  const listed = agents.slice(0, maxItems);
  const remaining = agents.length - listed.length;
  return {
    text: listed.map((a) => `${a.name} (${a.source}): ${a.description}`).join("; "),
    remaining,
  };
}
