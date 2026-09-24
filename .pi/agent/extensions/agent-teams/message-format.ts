/**
 * Formatting for inbound team messages shown in another member's pane.
 *
 * The problem this solves: a teammate's message used to render as plain
 * prose (`[Team message from @name]: ...`) that looked identical to the
 * reading member's own assistant text, so it was easy to miss who was
 * speaking. We render each message as a quote block:
 *
 *   ▏                       (top padding)
 *   ▏ from @alice:          (header, sender name in bold)
 *   ▏ the message body…
 *   ▏                       (bottom padding)
 *
 * The left rail is a solid colour cell (a space with a bright background,
 * no glyph). The header and body use the active theme's paired message
 * foreground and background, so light, dark, and custom themes stay readable.
 * Only the rail uses the per-sender colour (see sender-color.ts).
 *
 * Kept as a pure, width-aware function (returns lines) so it is trivial to
 * unit-test and to preview outside the TUI. Whether the body background
 * is drawn is passed in by the caller (env-controlled); the bright rail is
 * always drawn so the sender colour shows on any theme.
 */

import { truncateToWidth, visibleWidth, wrapTextWithAnsi } from "@mariozechner/pi-tui";
import { senderColor, senderRailBgCode } from "./sender-color.js";

/** Minimal slice of the TUI Theme we depend on (keeps this easy to stub). */
export interface MessageTheme {
  fg(color: string, text: string): string;
  bg(color: string, text: string): string;
  bold(text: string): string;
}

export interface RenderTeamMessageOptions {
	/** Sender name without the leading "@". */
	from: string;
	/** Full message body. */
	content: string;
	/** Viewport width the message must fit within. */
	width: number;
	theme: MessageTheme;
  /** Draw the theme's message background. Defaults to true. */
	background?: boolean;
}

const BG_RESET = "\x1b[49m";
// Solid rail cell (1) + a space of gutter (1) before the content.
const PREFIX = 2;
const TAB_SIZE = 4;
// C0 control chars (and DEL) that corrupt layout, excluding \t (\x09), \n
// (\x0a), \r (\x0d, normalised below) and ESC (\x1b, kept for ANSI colour).
const UNSAFE_CONTROLS = /[\x00-\x08\x0b\x0c\x0e-\x1a\x1c-\x1f\x7f]/g;

/**
 * Render an inbound team message to terminal lines (always fully expanded).
 *
 * The block is one blank padding line, a `from @sender:` header, the wrapped
 * body, and a closing blank padding line. Every line carries the solid rail;
 * with `background` on, every line uses the theme's message background,
 * padded to the full width.
 */
export function renderTeamMessageLines(opts: RenderTeamMessageOptions): string[] {
  const { from, content, width, theme, background = true } = opts;
  const railBg = senderRailBgCode(senderColor(from));
  const backgroundFn = background ? (text: string) => theme.bg("customMessageBg", text) : null;
  const innerWidth = Math.max(1, width - PREFIX);

  const header = fit(theme.fg("customMessageText", `from ${theme.bold(`@${from}`)}:`), innerWidth);
  const body = wrapContent(content, innerWidth).map((line) => theme.fg("customMessageText", line));

  // "" rows are the top/bottom internal padding.
  const rows = ["", header, ...body, ""];
  return rows.map((inner) => composeLine(inner, width, railBg, backgroundFn));
}

/**
 * Build one rendered line: a solid bright rail cell, then the content (and,
 * with a background, the rest of the line) on the theme's message background.
 */
function composeLine(inner: string, width: number, railBg: string, backgroundFn: ((text: string) => string) | null): string {
  const rail = `${railBg} ${BG_RESET}`;
  if (!backgroundFn) return `${rail} ${inner}`;
  const pad = " ".repeat(Math.max(0, width - PREFIX - visibleWidth(inner)));
  return `${rail}${backgroundFn(` ${inner}${pad}`)}`;
}

/** Truncate a styled line so it never exceeds the inner width. */
function fit(inner: string, innerWidth: number): string {
	return visibleWidth(inner) > innerWidth ? truncateToWidth(inner, innerWidth, "…") : inner;
}

/** Wrap content to the inner width, preserving intentional line breaks. */
function wrapContent(content: string, innerWidth: number): string[] {
	const out: string[] = [];
	for (const paragraph of sanitize(content).split("\n")) {
		if (paragraph.length === 0) {
			out.push("");
		} else if (visibleWidth(paragraph) <= innerWidth) {
			out.push(paragraph);
		} else {
			out.push(...wrapTextWithAnsi(paragraph, innerWidth));
		}
	}
	// Defensive clamp: wrapTextWithAnsi can still emit overwide lines when a
	// single grapheme is wider than innerWidth (very narrow panes / wide chars).
	const clamped = out.map((line) => (visibleWidth(line) > innerWidth ? truncateToWidth(line, innerWidth, "") : line));
	// Drop trailing blank lines so the body doesn't add empty rows before the pad.
	while (clamped.length > 1 && clamped[clamped.length - 1] === "") clamped.pop();
	return clamped;
}

/**
 * Make raw teammate content safe to draw line-by-line. Terminal control bytes
 * in a rendered line can move the cursor over the rail/gutter or desync our
 * width maths from what is actually drawn, so we normalise them here. This is
 * display-only — the model still receives the original, unmodified body.
 */
function sanitize(content: string): string {
	return content
		.replace(/\r\n?/g, "\n") // CRLF and lone CR → newline (no cursor-to-column-0)
		.replace(/\t/g, " ".repeat(TAB_SIZE)) // tabs → spaces (terminal tab stops desync width)
		.replace(UNSAFE_CONTROLS, ""); // drop other C0/DEL controls; keep \n and ANSI ESC
}
