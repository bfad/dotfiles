/**
 * Shared package-resolution utilities.
 *
 * Any extension that needs to resolve package source URLs to local filesystem
 * paths should import from here instead of rolling its own logic.
 */

import * as path from "node:path";
import { expandHome, parseGitSource } from "./source-parsing.js";

/**
 * Convert a git package source to its installed filesystem path.
 *
 * Supported patterns:
 *   - https://github.com/org/repo[@ref] → {agentDir}/git/github.com/org/repo
 *   - git:github.com/org/repo[@ref]     → {agentDir}/git/github.com/org/repo
 *   - git:git@github.com:org/repo[@ref] → {agentDir}/git/github.com/org/repo
 *   - ~/local/path                      → resolved relative to settings.json dir
 *
 * Security: validates git results are within {agentDir}/git/ to prevent path escaping.
 * Returns null if the source is invalid.
 */
export function resolvePackageRoot(source: string, settingsPath: string, agentDir: string): string | null {
	const result = parseGitSource(source);
	if (result.kind === "invalid") return null;
	if (result.kind === "git") {
		const resolved = path.resolve(path.join(agentDir, "git", result.host, result.repoPath));
		const gitRoot = path.resolve(path.join(agentDir, "git"));
		// Defense in depth: parseGitSource() already rejects traversal, but keep
		// the filesystem boundary check here in case upstream parsing regresses.
		/* c8 ignore next 3 */
		if (!resolved.startsWith(gitRoot + path.sep) && resolved !== gitRoot) return null;
		return resolved;
	}

	const expanded = expandHome(source);
	const baseDir = path.dirname(settingsPath);
	return path.resolve(baseDir, expanded);
}
