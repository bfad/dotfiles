/**
 * Git Commit Gate Extension
 *
 * Blocks any `git commit` (including `git commit --amend`) from bash tool calls
 * unless the user has explicitly invoked the /skill:commit skill in the current
 * turn. This prevents LLM agents from silently committing or amending without
 * the user's direct intent.
 *
 * Also blocks `git stash`, `git checkout -- <file>`, and `git reset` which can
 * silently discard uncommitted work.
 *
 * Also blocks operations that publish or rewrite history: `git push`,
 * `git rebase`, and branch creation/deletion (`git branch <name>`, `-d`/`-D`,
 * `-m`, `-c`, `git checkout -b`, `git switch -c`). Read-only `git branch`
 * queries (`--list`, `--show-current`, `--contains`, ...) are allowed.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	let commitSkillActive = false;

	// When the user explicitly invokes /skill:commit, allow git commit for that turn.
	pi.on("input", (_event) => {
		const text =
			typeof _event.text === "string"
				? _event.text
				: Array.isArray(_event.text)
					? _event.text.map((b: { text?: string }) => b.text ?? "").join("")
					: "";
		if (/\/skill:commit\b/i.test(text)) {
			commitSkillActive = true;
		}
		return undefined;
	});

	// Reset the flag at the end of each turn so the next agent turn
	// doesn't inherit the permission.
	pi.on("turn_end", () => {
		commitSkillActive = false;
		return undefined;
	});

	// `git branch` flags that consume a following value, so the value is not
	// mistaken for a new branch name.
	const BRANCH_VALUED_FLAGS = new Set([
		"--contains",
		"--no-contains",
		"--merged",
		"--no-merged",
		"--points-at",
		"--sort",
		"--format",
	]);

	const BRANCH_MUTATING_FLAGS =
		/(^|\s)(-d|-D|--delete|-m|-M|--move|-c|-C|--copy|-f|--force|-u|--set-upstream-to|--unset-upstream|--edit-description)(\s|=|$)/;

	// Read-only iff no mutating flag and no bare argument (a bare argument to
	// `git branch` is the name of a branch being created).
	function isReadOnlyBranch(command: string): boolean {
		const match = command.match(/\bgit\s+(?:(?:--?[\w-]+(?:=\S+)?|-[cC]\s+\S+)\s+)*branch\b(.*)$/);
		if (!match) return false;

		const rest = (match[1] ?? "").trim();
		if (BRANCH_MUTATING_FLAGS.test(` ${rest} `)) return false;

		const tokens = rest.split(/\s+/).filter(Boolean);
		for (let i = 0; i < tokens.length; i++) {
			const token = tokens[i] as string;
			if (token.startsWith("-")) {
				if (BRANCH_VALUED_FLAGS.has(token)) i++;
				continue;
			}
			return false;
		}
		return true;
	}

	// Git global options that may sit between `git` and the subcommand, so that
	// `git -C /path push` and `git -c k=v push` are not mistaken for reads.
	const GIT_GLOBAL_OPTS = String.raw`(?:(?:--?[\w-]+(?:=\S+)?|-[cC]\s+\S+)\s+)*`;

	function gitCommand(subcommand: string): RegExp {
		return new RegExp(String.raw`\bgit\s+${GIT_GLOBAL_OPTS}${subcommand}`);
	}

	const destructivePatterns: Array<{
		pattern: RegExp;
		label: string;
		allowIf?: (command: string) => boolean;
	}> = [
		{ pattern: /\bgit\s+commit\b/, label: "git commit" },
		{ pattern: /\bgit\s+stash\b(?!\s+list)/, label: "git stash (not list)" },
		{ pattern: /\bgit\s+checkout\s+--\s/, label: "git checkout -- (discard changes)" },
		{ pattern: /\bgit\s+restore\s/, label: "git restore" },
		{ pattern: /\bgit\s+reset\b/, label: "git reset" },
		{ pattern: gitCommand(String.raw`(?:push|send-pack)\b`), label: "git push" },
		{ pattern: gitCommand(String.raw`rebase\b`), label: "git rebase" },
		{
			pattern: gitCommand(String.raw`pull\b[^;&|]*--rebase\b`),
			label: "git pull --rebase",
		},
		{
			pattern: gitCommand(String.raw`worktree\s+add\b`),
			label: "git worktree add",
		},
		{
			pattern: gitCommand(String.raw`branch\b`),
			label: "git branch (create/delete/rename)",
			allowIf: isReadOnlyBranch,
		},
		{ pattern: /\bgit\s+checkout\s+(-b|-B)\b/, label: "git checkout -b (create branch)" },
		{ pattern: /\bgit\s+switch\s+(-c|-C|--create)\b/, label: "git switch -c (create branch)" },
		// `gs submit` pushes refs and creates/updates PRs; `gs continue` can finish
		// a submit that was interrupted, so it can push too.
		{ pattern: /\bgs\s+submit\b/, label: "gs submit (pushes + opens/updates PRs)" },
		{ pattern: /\bgs\s+continue\b/, label: "gs continue (may complete a push)" },
	];

	pi.on("tool_call", async (event, ctx) => {
		if (event.toolName !== "bash") return undefined;

		const command = event.input.command as string;

		for (const { pattern, label, allowIf } of destructivePatterns) {
			if (!pattern.test(command)) continue;
			if (allowIf?.(command)) continue;

			// git commit is only allowed when the /skill:commit was explicitly invoked
			if (label === "git commit" && commitSkillActive) continue;

			// For everything else (or git commit without the skill), prompt the user
			if (!ctx.hasUI) {
				return { block: true, reason: `${label} blocked — no UI for confirmation` };
			}

			const choice = await ctx.ui.select(
				`🛑 Agent wants to run: ${label}\n\n  ${command}\n\nAllow?`,
				["Yes, allow this once", "No, block it"],
			);

			if (choice !== "Yes, allow this once") {
				return { block: true, reason: `${label} blocked by user` };
			}
		}

		return undefined;
	});
}
