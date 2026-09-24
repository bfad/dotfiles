/**
 * Shared git/local source parsing utilities.
 *
 * Used by both discovery.ts and migration.ts to resolve package sources
 * from settings.json into filesystem paths.
 */

import * as os from "node:os";
import * as path from "node:path";

export type GitParseResult =
  | { kind: "git"; host: string; repoPath: string }
  | { kind: "local" }
  | { kind: "invalid" };

export function stripGitSuffix(spec: string): string {
  return spec.endsWith(".git") ? spec.slice(0, -4) : spec;
}

export function isLikelyLocalPath(source: string): boolean {
  return (
    source.startsWith("/") ||
    source.startsWith("./") ||
    source.startsWith("../") ||
    source.startsWith("~/") ||
    source === "." ||
    source === ".." ||
    source === "~"
  );
}

export function stripPathRef(pathWithMaybeRef: string): string {
  const refSeparator = pathWithMaybeRef.indexOf("@");
  return refSeparator === -1 ? pathWithMaybeRef : pathWithMaybeRef.slice(0, refSeparator);
}

function hasUnsafeGitPathSegments(repoPath: string): boolean {
  return repoPath.split("/").some((segment) => segment === "" || segment === "." || segment === "..");
}

export function expandHome(p: string): string {
  if (p === "~") return os.homedir();
  if (p.startsWith("~/")) return path.join(os.homedir(), p.slice(2));
  return p;
}

/**
 * Parse a package source string into a structured result.
 *
 * - `{ kind: "git", host, repoPath }` — recognized git source
 * - `{ kind: "local" }` — looks like a local filesystem path
 * - `{ kind: "invalid" }` — looks like a git source but is malformed
 */
export function parseGitSource(source: string): GitParseResult {
  const trimmed = source.trim();
  if (isLikelyLocalPath(trimmed) || trimmed.startsWith("npm:")) return { kind: "local" };

  const stripped = trimmed.replace(/^git:/, "");

  if (/^(https?|ssh|git):\/\//i.test(stripped)) {
    const rawPath = stripped.replace(/^(?:https?|ssh|git):\/\/[^/]*\/?/i, "");
    if (rawPath) {
      const unsafeRawPath = stripGitSuffix(stripPathRef(rawPath.replace(/^\/+/, "")));
      if (hasUnsafeGitPathSegments(unsafeRawPath)) return { kind: "invalid" };
    }

    try {
      const url = new URL(stripped);
      const host = url.hostname;
      const repoPath = stripGitSuffix(stripPathRef(url.pathname.replace(/^\/+/, "")));
      if (!host || !repoPath || hasUnsafeGitPathSegments(repoPath)) return { kind: "invalid" };
      return { kind: "git", host, repoPath };
    } catch {
      return { kind: "invalid" };
    }
  }

  const sshMatch = stripped.match(/^git@([^:]+):(.+)$/i);
  if (sshMatch) {
    const host = sshMatch[1]!;
    const repoPath = stripGitSuffix(stripPathRef(sshMatch[2]!));
    if (!host || !repoPath || hasUnsafeGitPathSegments(repoPath)) return { kind: "invalid" };
    return { kind: "git", host, repoPath };
  }

  const shorthandMatch = stripped.match(/^([^/]+)\/(.+)$/);
  if (!shorthandMatch) return { kind: "local" };

  const host = shorthandMatch[1]!;
  const repoPath = stripGitSuffix(stripPathRef(shorthandMatch[2]!));
  if ((!host.includes(".") && host !== "localhost") || !repoPath || repoPath.split("/").length < 2) {
    return { kind: "local" };
  }
  if (hasUnsafeGitPathSegments(repoPath)) return { kind: "invalid" };
  return { kind: "git", host, repoPath };
}
