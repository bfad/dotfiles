/**
 * Team Overlay — TUI component for viewing RPC teammate output.
 *
 * Toggled via Ctrl+Shift+M, this overlay shows:
 *   • Left: teammate list with status indicators
 *   • Right: selected teammate's live output (scrollable)
 *   • Bottom: keyboard hints
 */

import type { Theme } from "@mariozechner/pi-coding-agent";
import { matchesKey, truncateToWidth, visibleWidth, wrapTextWithAnsi } from "@mariozechner/pi-tui";
import type { RpcTeammate, TeammateStatus } from "./rpc-teammate.js";

// ============================================================================
// Types
// ============================================================================

export interface TeamOverlayOptions {
	teammates: RpcTeammate[];
	theme: Theme;
	done: (result: undefined) => void;
	requestRender: () => void;
}

// ============================================================================
// Component
// ============================================================================

export class TeamOverlay {
	private teammates: RpcTeammate[];
	private theme: Theme;
	private done: (result: undefined) => void;
	private requestRender: () => void;
	private selectedIndex = 0;
	private scrollOffset = 0;

	constructor(options: TeamOverlayOptions) {
		this.teammates = options.teammates;
		this.theme = options.theme;
		this.done = options.done;
		this.requestRender = options.requestRender;
	}

	/** Call to update the teammates list (e.g. after a new spawn). */
	updateTeammates(teammates: RpcTeammate[]): void {
		this.teammates = teammates;
		if (this.selectedIndex >= teammates.length) {
			this.selectedIndex = Math.max(0, teammates.length - 1);
		}
	}

	handleInput(data: string): void {
		if (matchesKey(data, "escape") || matchesKey(data, "q")) {
			this.done(undefined);
			return;
		}

		// Navigate teammate list
		if (matchesKey(data, "up") || matchesKey(data, "k")) {
			if (this.selectedIndex > 0) {
				this.selectedIndex--;
				this.scrollOffset = 0; // Reset scroll on teammate switch
			}
		} else if (matchesKey(data, "down") || matchesKey(data, "j")) {
			if (this.selectedIndex < this.teammates.length - 1) {
				this.selectedIndex++;
				this.scrollOffset = 0;
			}
		}
		// Scroll output
		else if (matchesKey(data, "shift+up") || matchesKey(data, "shift+k")) {
			this.scrollOffset = Math.max(0, this.scrollOffset - 1);
		} else if (matchesKey(data, "shift+down") || matchesKey(data, "shift+j")) {
			this.scrollOffset++;
		} else if (matchesKey(data, "pageUp")) {
			this.scrollOffset = Math.max(0, this.scrollOffset - 10);
		} else if (matchesKey(data, "pageDown")) {
			this.scrollOffset += 10;
		}
		// Home = scroll to bottom (latest output)
		else if (matchesKey(data, "home") || matchesKey(data, "g")) {
			this.scrollOffset = 0;
		}
	}

	render(width: number): string[] {
		const th = this.theme;
		const totalHeight = Math.min(30, process.stdout.rows ? process.stdout.rows - 4 : 26);
		const innerW = width - 2;

		if (this.teammates.length === 0) {
			return this.renderEmpty(width, innerW, totalHeight, th);
		}

		// Layout: sidebar (22 chars) + separator (1) + output area
		const sidebarW = Math.min(24, Math.floor(innerW * 0.3));
		const outputW = innerW - sidebarW - 1; // 1 for separator

		const lines: string[] = [];

		// Top border
		lines.push(th.fg("border", `╭${"─".repeat(innerW)}╮`));

		// Title
		const title = " Agent Teams ";
		const titlePad = Math.max(0, innerW - visibleWidth(title));
		const leftPad = Math.floor(titlePad / 2);
		lines.push(
			th.fg("border", "│") +
				" ".repeat(leftPad) +
				th.fg("accent", th.bold(title)) +
				" ".repeat(titlePad - leftPad) +
				th.fg("border", "│"),
		);

		// Separator below title
		lines.push(
			th.fg("border", "├") +
				th.fg("border", "─".repeat(sidebarW)) +
				th.fg("border", "┬") +
				th.fg("border", "─".repeat(outputW)) +
				th.fg("border", "┤"),
		);

		// Content rows
		const contentHeight = totalHeight - 5; // title + top/bottom borders + separator + help
		const sidebarLines = this.renderSidebar(sidebarW, contentHeight, th);
		const outputLines = this.renderOutput(outputW, contentHeight, th);

		for (let i = 0; i < contentHeight; i++) {
			const sideCell = sidebarLines[i] ?? padTo("", sidebarW);
			const outCell = outputLines[i] ?? padTo("", outputW);
			lines.push(
				th.fg("border", "│") + sideCell + th.fg("border", "│") + outCell + th.fg("border", "│"),
			);
		}

		// Separator above help
		lines.push(
			th.fg("border", "├") +
				th.fg("border", "─".repeat(sidebarW)) +
				th.fg("border", "┴") +
				th.fg("border", "─".repeat(outputW)) +
				th.fg("border", "┤"),
		);

		// Help bar
		const helpText = " ↑↓ select • Shift+↑↓ scroll • PgUp/PgDn • g top • Esc close";
		lines.push(
			th.fg("border", "│") +
				padTo(th.fg("dim", helpText), innerW) +
				th.fg("border", "│"),
		);

		// Bottom border
		lines.push(th.fg("border", `╰${"─".repeat(innerW)}╯`));

		return lines;
	}

	invalidate(): void {
		// No caching — always re-render
	}

	// -----------------------------------------------------------------------
	// Sidebar: teammate list
	// -----------------------------------------------------------------------

	private renderSidebar(width: number, height: number, th: Theme): string[] {
		const lines: string[] = [];

		for (let i = 0; i < this.teammates.length && lines.length < height; i++) {
			const t = this.teammates[i]!;
			const isSelected = i === this.selectedIndex;
			const icon = statusIcon(t.status, th);
			const prefix = isSelected ? th.fg("accent", "▶ ") : "  ";
			const name = isSelected ? th.fg("accent", `@${t.name}`) : `@${t.name}`;
			const line = `${prefix}${icon} ${name}`;
			lines.push(padTo(truncateToWidth(line, width), width));
		}

		// Fill remaining
		while (lines.length < height) {
			lines.push(" ".repeat(width));
		}

		return lines;
	}

	// -----------------------------------------------------------------------
	// Output: selected teammate's streamed output
	// -----------------------------------------------------------------------

	private renderOutput(width: number, height: number, th: Theme): string[] {
		const teammate = this.teammates[this.selectedIndex];
		if (!teammate) {
			const lines: string[] = [];
			while (lines.length < height) lines.push(" ".repeat(width));
			return lines;
		}

		// Get all output lines and wrap them to fit
		const rawLines = teammate.outputLines;
		const wrapped: string[] = [];
		for (const raw of rawLines) {
			if (visibleWidth(raw) <= width) {
				wrapped.push(raw);
			} else {
				// Wrap long lines
				const wrappedResult = wrapTextWithAnsi(raw, width);
				for (const wl of wrappedResult) {
					wrapped.push(wl);
				}
			}
		}

		// Scroll: offset 0 = show latest (bottom), positive = scroll up
		const totalLines = wrapped.length;
		const maxScroll = Math.max(0, totalLines - height);
		// Clamp scroll offset
		if (this.scrollOffset > maxScroll) {
			this.scrollOffset = maxScroll;
		}

		const startIdx = Math.max(0, totalLines - height - this.scrollOffset);
		const visibleSlice = wrapped.slice(startIdx, startIdx + height);

		const lines: string[] = [];

		// Show scroll position if not at bottom
		if (this.scrollOffset > 0) {
			const scrollInfo = th.fg("dim", ` ↑ ${this.scrollOffset} more lines above`);
			lines.push(padTo(truncateToWidth(scrollInfo, width), width));
			for (let i = 0; i < Math.min(visibleSlice.length, height - 1); i++) {
				lines.push(padTo(truncateToWidth(visibleSlice[i] ?? "", width), width));
			}
		} else {
			for (const vl of visibleSlice) {
				lines.push(padTo(truncateToWidth(vl, width), width));
			}
		}

		// Fill remaining
		while (lines.length < height) {
			lines.push(" ".repeat(width));
		}

		return lines;
	}

	// -----------------------------------------------------------------------
	// Empty state
	// -----------------------------------------------------------------------

	private renderEmpty(width: number, innerW: number, totalHeight: number, th: Theme): string[] {
		const lines: string[] = [];
		lines.push(th.fg("border", `╭${"─".repeat(innerW)}╮`));
		const title = " Agent Teams ";
		const titlePad = Math.max(0, innerW - visibleWidth(title));
		const leftPad = Math.floor(titlePad / 2);
		lines.push(
			th.fg("border", "│") +
				" ".repeat(leftPad) +
				th.fg("accent", th.bold(title)) +
				" ".repeat(titlePad - leftPad) +
				th.fg("border", "│"),
		);
		lines.push(th.fg("border", `├${"─".repeat(innerW)}┤`));

		const msg = "No teammates spawned yet.";
		const msgPad = Math.max(0, innerW - visibleWidth(msg));
		const msgLeft = Math.floor(msgPad / 2);
		const emptyRows = Math.max(1, totalHeight - 5);
		const midRow = Math.floor(emptyRows / 2);
		for (let i = 0; i < emptyRows; i++) {
			if (i === midRow) {
				lines.push(
					th.fg("border", "│") +
						" ".repeat(msgLeft) +
						th.fg("dim", msg) +
						" ".repeat(msgPad - msgLeft) +
						th.fg("border", "│"),
				);
			} else {
				lines.push(th.fg("border", "│") + " ".repeat(innerW) + th.fg("border", "│"));
			}
		}

		lines.push(th.fg("border", `├${"─".repeat(innerW)}┤`));
		const helpText = " Esc close";
		lines.push(
			th.fg("border", "│") +
				padTo(th.fg("dim", helpText), innerW) +
				th.fg("border", "│"),
		);
		lines.push(th.fg("border", `╰${"─".repeat(innerW)}╯`));
		return lines;
	}
}

// ============================================================================
// Helpers
// ============================================================================

function statusIcon(status: TeammateStatus, th: Theme): string {
	switch (status) {
		case "starting":
			return th.fg("warning", "◌");
		case "running":
			return th.fg("success", "●");
		case "idle":
			return th.fg("accent", "○");
		case "error":
			return th.fg("error", "✗");
		case "dead":
			return th.fg("dim", "✗");
	}
}

/** Pad a string with spaces to exactly `width` visible characters. */
function padTo(s: string, width: number): string {
	const vis = visibleWidth(s);
	if (vis >= width) return s;
	return s + " ".repeat(width - vis);
}
