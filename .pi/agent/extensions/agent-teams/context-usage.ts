/**
 * Context-usage annotations on inbound team messages.
 *
 * The problem this solves: a team lead reading a teammate's message has no way
 * to tell how much of that teammate's context window is already spent. A
 * teammate running low on context produces worse work and is a candidate to be
 * retired and replaced. So each message can carry the sender's usage (token
 * count + percentage of the window), captured at send time via
 * `ctx.getContextUsage()`.
 *
 * The usage is delivered to the *reading* agent as a trailing one-line note on
 * the message body (so the lead can reason about it and act), but it is hidden
 * in the collapsed TUI view — it is steering data, not something the human
 * needs to read on every message. `stripContextUsageNote` removes it for that
 * collapsed display, mirroring how `stripTrailingNotes` handles other notes.
 *
 * The note names its sender (`[@alice context usage: …]`) so it is
 * unambiguously the *sender's* usage, never the reader's.
 */

import type { ContextUsage } from "./types.js";

/** Marker substring shared by every usage note, used to format and detect it. */
export const CONTEXT_USAGE_MARKER = "context usage:";

/** Matches a full trailing usage note line, e.g. `[@alice context usage: …]`. */
const CONTEXT_USAGE_NOTE_RE = /^\[@\S+ context usage: .+\]$/;

/**
 * Format a sender's context usage as a single-line note, e.g.
 * `[@alice context usage: 142,000 / 200,000 tokens (71%)]`.
 *
 * The note is prefixed with the sender's name so the reader can tell whose
 * usage it is. Returns `null` when there is nothing meaningful to report — no
 * usage object, an unknown token count, or a non-positive context window — so
 * callers can simply skip appending a note.
 */
export function formatContextUsageNote(from: string, usage: ContextUsage | null | undefined): string | null {
	if (!usage) return null;
	const { tokens, contextWindow } = usage;
	if (tokens === null || contextWindow <= 0) return null;
	const percent = usage.percent === null ? (tokens / contextWindow) * 100 : usage.percent;
	const used = formatTokens(tokens);
	const total = formatTokens(contextWindow);
	return `[@${from} ${CONTEXT_USAGE_MARKER} ${used} / ${total} tokens (${Math.round(percent)}%)]`;
}

/**
 * Read context usage without ever throwing.
 *
 * `getContextUsage` is optional on the host and may throw (e.g. mid-compaction
 * or in modes that don't implement it). Capturing usage must never block the
 * thing it annotates — a failed read degrades to *no note*, so `team_message`,
 * `team_broadcast`, and `team_shutdown` always proceed.
 */
export function safeContextUsage(
	ctx: { getContextUsage?: () => ContextUsage | undefined },
): ContextUsage | undefined {
	try {
		return ctx.getContextUsage?.();
	} catch {
		return undefined;
	}
}

/** Render a token count with thousands separators (locale-independent). */
function formatTokens(n: number): string {
	return Math.round(n).toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

/**
 * Return `content` with a trailing context-usage note removed (along with the
 * blank space before it). Used for the collapsed display only — the model
 * still receives the full body. If the last line is not a usage note, the text
 * is returned unchanged.
 */
export function stripContextUsageNote(content: string): string {
	const trimmed = content.replace(/\s+$/, "");
	const lastBreak = trimmed.lastIndexOf("\n");
	const lastLine = trimmed.slice(lastBreak + 1);
	if (CONTEXT_USAGE_NOTE_RE.test(lastLine)) {
		return lastBreak < 0 ? "" : trimmed.slice(0, lastBreak).replace(/\s+$/, "");
	}
	return content;
}
