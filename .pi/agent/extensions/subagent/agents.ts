/**
 * Agent discovery and configuration
 *
 * Source precedence:
 *   1. Package tier — this extension's bundled role pack plus installed package agents
 *      from settings.json. Bundled role names are canonical inside this tier.
 *   2. User agents — ~/.pi/agent/agents/ override package-tier agents.
 *   3. Project agents — nearest .pi/agents/ override both when scope is "both".
 *
 * Scope "project" intentionally loads only project agents. Installed package agents with
 * bundled role names are ignored so stale role packs cannot shadow the bundled prompts/tools.
 */

import * as fs from "node:fs";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { getAgentDir, parseFrontmatter } from "@mariozechner/pi-coding-agent";
import { resolvePackageRoot } from "../../lib/packages.js";
import type { AgentScope, AgentSource } from "./types.js";

export type { AgentScope } from "./types.js";

export const AGENT_THINKING_LEVELS = ["off", "minimal", "low", "medium", "high", "xhigh", "max"] as const;
export type AgentThinkingLevel = (typeof AGENT_THINKING_LEVELS)[number];

export function isAgentThinkingLevel(value: unknown): value is AgentThinkingLevel {
  return typeof value === "string" && AGENT_THINKING_LEVELS.includes(value as AgentThinkingLevel);
}

export interface AgentConfig {
	name: string;
	description: string;
	tools?: string[];
  excludeTools?: string[];
	model?: string;
  thinking?: AgentThinkingLevel;
	systemPrompt: string;
	source: AgentSource;
	filePath: string;
}

export interface AgentDiscoveryResult {
	agents: AgentConfig[];
	projectAgentsDir: string | null;
}

const BUNDLED_AGENTS_DIR = path.join(path.dirname(fileURLToPath(import.meta.url)), "agents");

function loadAgentsFromDir(dir: string, source: AgentSource): AgentConfig[] {
	const agents: AgentConfig[] = [];

	if (!fs.existsSync(dir)) {
		return agents;
	}

	let entries: fs.Dirent[];
	try {
		entries = fs.readdirSync(dir, { withFileTypes: true });
	} catch {
		return agents;
	}

	for (const entry of entries.sort((a, b) => a.name.localeCompare(b.name))) {
		if (!entry.name.endsWith(".md")) continue;
		if (!entry.isFile() && !entry.isSymbolicLink()) continue;

		const filePath = path.join(dir, entry.name);
		let content: string;
		try {
			content = fs.readFileSync(filePath, "utf-8");
		} catch {
			continue;
		}

		const { frontmatter, body } = parseFrontmatter<Record<string, string>>(content);

		if (!frontmatter.name || !frontmatter.description) {
			continue;
		}

		const tools = frontmatter.tools
			?.split(",")
			.map((t: string) => t.trim())
			.filter(Boolean);
    const excludeTools = frontmatter.excludeTools
      ?.split(",")
      .map((tool) => tool.trim())
      .filter(Boolean);
    const rawThinking = (frontmatter as Record<string, unknown>).thinking;
    const thinking = isAgentThinkingLevel(rawThinking) ? rawThinking : undefined;

		agents.push({
			name: frontmatter.name,
			description: frontmatter.description,
			tools: tools && tools.length > 0 ? tools : undefined,
      excludeTools: excludeTools && excludeTools.length > 0 ? excludeTools : undefined,
			model: frontmatter.model,
      ...(thinking ? { thinking } : {}),
			systemPrompt: body,
			source,
			filePath,
		});
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
	let currentDir = cwd;
	while (true) {
		const candidate = path.join(currentDir, ".pi", "agents");
		if (isDirectory(candidate)) return candidate;

		const parentDir = path.dirname(currentDir);
		if (parentDir === currentDir) return null;
		currentDir = parentDir;
	}
}

/**
 * Discover the role pack bundled with this extension.
 */
export function discoverBundledAgents(): AgentConfig[] {
	return loadAgentsFromDir(BUNDLED_AGENTS_DIR, "package");
}

/**
 * Discover agents from all installed packages' agents/ directories.
 */
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

		const agentsDir = path.join(pkgRoot, "agents");
		agents.push(...loadAgentsFromDir(agentsDir, "package"));
	}

	return agents;
}

export function discoverAgents(cwd: string, scope: AgentScope): AgentDiscoveryResult {
	const agentDir = getAgentDir();
	const userDir = path.join(agentDir, "agents");
	const projectAgentsDir = findNearestProjectAgentsDir(cwd);

	const bundledAgents = scope === "project" ? [] : discoverBundledAgents();
	const bundledAgentNames = new Set(bundledAgents.map((agent) => agent.name));
	const installedPackageAgents = scope === "project"
		? []
		: discoverPackageAgents(agentDir).filter((agent) => !bundledAgentNames.has(agent.name));
	const packageAgents = [...bundledAgents, ...installedPackageAgents];
	const userAgents = scope === "project" ? [] : loadAgentsFromDir(userDir, "user");
	const projectAgents = scope === "user" || !projectAgentsDir ? [] : loadAgentsFromDir(projectAgentsDir, "project");

	// Insert order: package → user → project (later tiers win on name conflicts).
	// Within the package tier, bundled role names are canonical so stale installed
	// pi-subagents role packs cannot shadow this extension's bundled prompts/tools.
	const agentMap = new Map<string, AgentConfig>();

	if (scope === "both") {
		for (const agent of packageAgents) agentMap.set(agent.name, agent);
		for (const agent of userAgents) agentMap.set(agent.name, agent);
		for (const agent of projectAgents) agentMap.set(agent.name, agent);
	} else if (scope === "user") {
		for (const agent of packageAgents) agentMap.set(agent.name, agent);
		for (const agent of userAgents) agentMap.set(agent.name, agent);
	} else {
		for (const agent of projectAgents) agentMap.set(agent.name, agent);
	}

	return { agents: Array.from(agentMap.values()), projectAgentsDir };
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
