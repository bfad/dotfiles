/**
 * Session Namer — auto-names sessions with a cheap, fast model.
 *
 * Renames the session after each of the first few settled agent turns, so the
 * title sharpens as the conversation takes shape. Stops permanently once you
 * rename the session yourself (via /name or any other extension).
 *
 * Commands:
 *   /autoname          — (re)name the current session now
 *   /autoname backfill — name every unnamed session on disk
 */

import { type ExtensionAPI, type ExtensionContext, SessionManager } from "@earendil-works/pi-coding-agent";

/** Tried in order; first one that exists with configured auth wins. */
const CANDIDATE_MODELS: [provider: string, modelId: string][] = [
	["anthropic", "claude-haiku-4-5"],
	["openai", "gpt-5.4-mini"],
	["google", "gemini-flash-lite-latest"],
];

const MAX_WORDS = 8; // hard cap — shorter titles are welcome
const MAX_TURNS = 3; // auto-name attempts per session
const MAX_CONTEXT_CHARS = 6000;
const TIMEOUT_MS = 20_000;

const SYSTEM_PROMPT = `You title conversations between a user and a coding assistant.

Rules:
- Your ENTIRE response is the title: no quotes, markdown, preamble, or trailing punctuation
- At most ${MAX_WORDS} words. Use as few as convey the task — two or three words is great
- Name the task; never answer, continue, or comment on the conversation

Examples of valid outputs:
Fix flaky webhook test
JWT auth refactor
Session namer extension
Set up monorepo CI pipeline`;

export default function (pi: ExtensionAPI) {
	let turns = 0;
	let lastGenerated: string | undefined;

	pi.on("session_start", () => {
		const existing = pi.getSessionName();
		lastGenerated = existing;
		turns = existing ? MAX_TURNS : 0; // already titled? leave it alone
	});

	// A name we did not generate means the user renamed it — back off for good.
	pi.on("session_info_changed", (event) => {
		if (event.name !== lastGenerated) turns = MAX_TURNS;
	});

	pi.on("agent_settled", async (_event, ctx) => {
		if (turns++ >= MAX_TURNS) return;
		await nameCurrent(ctx);
	});

	pi.registerCommand("autoname", {
		description: "Name this session with an LLM (usage: /autoname [backfill])",
		handler: async (args, ctx) => {
			if (args.trim() === "backfill") return backfill(ctx);
			const name = await nameCurrent(ctx);
			ctx.ui.notify(name ? `Session named: ${name}` : "Could not generate a name", name ? "info" : "warning");
		},
	});

	async function nameCurrent(ctx: ExtensionContext): Promise<string | undefined> {
		const name = await generate(ctx, conversationText(ctx.sessionManager.getBranch()));
		if (name) {
			lastGenerated = name;
			pi.setSessionName(name);
		}
		return name;
	}

	async function backfill(ctx: ExtensionContext) {
		const sessions = await SessionManager.listAll().catch(() => []);
		const current = ctx.sessionManager.getSessionFile();
		const todo = sessions.filter((s) => !s.name && s.messageCount > 0 && s.path !== current);
		if (!todo.length) {
			ctx.ui.notify(`Nothing to backfill (${sessions.length} sessions scanned)`, "info");
			return;
		}
		const ok = await ctx.ui.confirm(
			`Name ${todo.length} unnamed sessions?`,
			`${sessions.length} sessions on disk. This makes ~${todo.length} LLM calls.`,
		);
		if (!ok) return;

		let named = 0;
		for (const session of todo) {
			const name = await generate(ctx, session.allMessagesText);
			if (!name) continue;
			try {
				SessionManager.open(session.path).appendSessionInfo(name);
				named++;
			} catch {
				// unreadable/locked session file — skip
			}
		}
		ctx.ui.notify(`Named ${named}/${todo.length} sessions`, named ? "info" : "warning");
	}

	async function generate(ctx: ExtensionContext, text: string): Promise<string | undefined> {
		const context = text.trim().slice(0, MAX_CONTEXT_CHARS);
		if (!context) return;

		const model = CANDIDATE_MODELS.map(([p, id]) => ctx.modelRegistry.find(p, id)).find(
			(m) => m && ctx.modelRegistry.hasConfiguredAuth(m),
		);
		if (!model) return;

		try {
			const response = await ctx.modelRegistry.complete(
				model,
				{
					systemPrompt: SYSTEM_PROMPT,
					messages: [
						{
							role: "user",
							content: [{ type: "text", text: `<conversation>\n${context}\n</conversation>` }],
							timestamp: Date.now(),
						},
					],
				},
				{ maxTokens: 64, cacheRetention: "none", signal: AbortSignal.timeout(TIMEOUT_MS) },
			);
			return sanitize(response.content.map((c) => (c.type === "text" ? c.text : "")).join(""));
		} catch {
			return; // offline, rate limited, aborted — naming is best effort
		}
	}
}

function sanitize(raw: string): string | undefined {
	const title = raw
		.trim()
		.split("\n")[0]
		.replace(/^#+\s*/, "")
		.replace(/^["'`]|["'`]$/g, "")
		.replace(/[.,;:!]+$/, "")
		.trim()
		.split(/\s+/)
		.slice(0, MAX_WORDS)
		.join(" ");
	return title && title.length <= 100 ? title : undefined;
}

/** Flatten a session branch into "User: ... / Assistant: ..." text. */
function conversationText(branch: { type: string; message?: { role?: string; content?: unknown } }[]): string {
	const parts: string[] = [];
	for (const entry of branch) {
		const role = entry.type === "message" ? entry.message?.role : undefined;
		if (role !== "user" && role !== "assistant") continue;
		const blocks: { type?: string; text?: string }[] = Array.isArray(entry.message?.content)
			? entry.message?.content
			: [];
		const text = blocks
			.filter((c) => c?.type === "text" && c.text)
			.map((c) => c.text)
			.join("\n")
			.trim();
		if (text) parts.push(`${role === "user" ? "User" : "Assistant"}: ${text}`);
	}
	return parts.join("\n\n");
}
