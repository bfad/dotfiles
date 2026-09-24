/**
 * TUI customization: custom footer and terminal title.
 *
 * Footer layout:
 *   Line 1: session name (left)                    model(provider):thinking (right)
 *   Line 2: worldpath + branch (left)              context gauge (right)
 *   Line 3: extension statuses (if any)
 *
 * Terminal title: updates to "π - session name" when named.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@mariozechner/pi-tui";
import { execSync } from "child_process";

const PL_RIGHT = "\ue0b0"; // Powerline right-pointing solid arrow
const PL_LEFT = "\ue0b2"; //  Powerline left-pointing solid arrow
const GIT_ICON = "\ufb2b"; // שׂ (U+FB2B)

const esc = (code: string) => `\x1b[${code}m`;
const RESET = esc("0");

function twoColumn(left: string, right: string, width: number): string {
	const leftW = visibleWidth(left);
	const rightW = visibleWidth(right);
	const gap = width - leftW - rightW;
	if (gap >= 1) {
		return left + " ".repeat(gap) + right;
	}
	// Not enough room — truncate left to make space for right + 1 padding
	const available = width - rightW - 1;
	if (available > 0) {
		return truncateToWidth(left, available, "...") + " " + right;
	}
	return truncateToWidth(left + " " + right, width);
}

function getRepoBasename(): string | null {
	try {
		const toplevel = execSync("git rev-parse --show-toplevel", {
			encoding: "utf-8",
			stdio: ["pipe", "pipe", "pipe"],
			timeout: 500,
		}).trim();
		return toplevel.split("/").pop() || null;
	} catch {
		return null;
	}
}

function getWorldPath(): { path: string; branch: string | null } {
	try {
		let output = execSync("worldpath -f", {
			encoding: "utf-8",
			stdio: ["pipe", "pipe", "pipe"],
			timeout: 500,
		}).trimEnd();
		// Extract and strip the @branch suffix (rendered as \x1b[36m@...\x1b[0m)
		let branch: string | null = null;
		const branchMatch = output.match(/\x1b\[36m@([^\x1b]*)\x1b\[0m$/);
		if (branchMatch) {
			branch = branchMatch[1];
			output = output.replace(branchMatch[0], "");
		}
		return { path: output, branch };
	} catch {
		let pwd = process.cwd();
		const home = process.env.HOME || process.env.USERPROFILE;
		if (home && pwd.startsWith(home)) {
			pwd = `~${pwd.slice(home.length)}`;
		}
		return { path: pwd, branch: null };
	}
}

function powerlineBranch(branch: string): string {
	return [
		esc("32"),
		PL_LEFT,
		esc("42;30"),
    GIT_ICON,
    " ",
		branch,
		esc("0;32"),
		PL_RIGHT,
		RESET,
	].join("");
}

/**
 * Highlight the "important" directory in the worldpath output.
 *
 * Strategy:
 * 1. If worldpath contains [22m] (bold→unbold), it's a worktree/sparse-checkout boundary.
 *    Extract the leaf directory before the marker and highlight it.
 * 2. Otherwise, if we have a repo basename, find it in the path and highlight it.
 * 3. Otherwise, return the path unchanged.
 */
function highlightPath(worldpath: string, repoBasename: string | null): string {
	// Restyle worktree name: worldpath renders it as \x1b[32m+name\x1b[0m → yellow bg, black fg with  /
	worldpath = worldpath.replace(
		/\x1b\[32m([^\x1b]+)\x1b\[0m/,
		esc("33") + "\ue0ba" + esc("43;30") + "$1" + RESET + esc("33") + "\ue0bc" + RESET,
	);

	const UNBOLD = "\x1b[22m";
	const markerIdx = worldpath.indexOf(UNBOLD);

	const cyanCap = (name: string) =>
		esc("36") + "\ue0ba" + esc("46;30") + name + RESET + esc("36") + "\ue0bc" + RESET;

	if (markerIdx !== -1) {
		// Worktree/sparse-checkout: extract leaf before [22m]
		const before = worldpath.substring(0, markerIdx);
		const lastSlash = before.lastIndexOf("/");
		if (lastSlash !== -1) {
			const leaf = before.substring(lastSlash + 1);
			if (leaf) {
				// Consume leading / and trailing / (after [22m])
				const target = "/" + leaf + UNBOLD;
				const trailingSlash = worldpath[markerIdx + UNBOLD.length] === "/" ? "/" : "";
				const fullTarget = target + trailingSlash;
				const highlighted = cyanCap(leaf) + esc("34") + UNBOLD;
				return worldpath.replace(fullTarget, highlighted);
			}
		}
	} else if (repoBasename) {
		// Normal git repo: highlight the repo root directory name, consuming adjacent /
		const idx1 = worldpath.indexOf(`/${repoBasename}/`);
		if (idx1 !== -1) {
			const before = worldpath.substring(0, idx1);
			const after = worldpath.substring(idx1 + 1 + repoBasename.length + 1);
			return before + cyanCap(repoBasename) + esc("34") + after;
		}
		const idx2 = worldpath.indexOf(`/${repoBasename}\x1b`);
		if (idx2 !== -1) {
			const before = worldpath.substring(0, idx2);
			const after = worldpath.substring(idx2 + 1 + repoBasename.length);
			return before + cyanCap(repoBasename) + esc("34") + after;
		}
		if (worldpath.endsWith(repoBasename)) {
			const before = worldpath.substring(0, worldpath.length - repoBasename.length);
			return before + cyanCap(repoBasename);
		}
	}

	return worldpath;
}

function formatTokens(count: number): string {
	if (count < 1000) return count.toString();
	if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
	if (count < 1000000) return `${Math.round(count / 1000)}k`;
	return `${(count / 1000000).toFixed(1)}M`;
}

function sanitize(text: string): string {
	return text.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim();
}

export default function (pi: ExtensionAPI) {
	pi.on("session_start", (_event, ctx) => {
		ctx.ui.setFooter((tui, theme, footerData) => {
			let cachedRepoName: string | null = null;
			let cachedWorldPath: string = "";
			let cachedBranch: string | null = null;
			let dirty = true;

			const unsub = footerData.onBranchChange(() => {
				dirty = true;
				tui.requestRender();
			});

			return {
				dispose: unsub,
				invalidate() {
					dirty = true;
				},

				render(width: number): string[] {
					if (dirty) {
						const wp = getWorldPath();
						cachedWorldPath = wp.path;
						cachedBranch = wp.branch ?? footerData.getGitBranch();
						cachedRepoName = cachedBranch ? getRepoBasename() : null;
						dirty = false;
					}

					// ── Build right-side content ──

					const modelName = ctx.model?.id || "no-model";
					const provider = ctx.model?.provider || "";
					const providerPart = provider ? `(${provider})` : "";
					let modelStr = `${modelName}${providerPart}`;
					if (ctx.model?.reasoning) {
						const thinking = pi.getThinkingLevel();
						if (thinking && thinking !== "off") {
							modelStr = `${modelName}${providerPart}:${thinking}`;
						}
					}
					const modelRight = theme.fg("muted", modelStr);

					// Context usage gauge
					const ctxUsage = ctx.getContextUsage();
					const ctxPercent = ctxUsage?.percent ?? 0;
					const ctxWindow = ctxUsage?.contextWindow ?? 0;
					const ctxKnown = ctxUsage?.percent !== null;

					// Partial block characters indexed by eighths (0 = empty, 8 = full)
					// 0 = space, 1 = ▏, 2 = ▎, 3 = ▍, 4 = ▌, 5 = ▋, 6 = ▊, 7 = ▉, 8 = █
					const EIGHTH_BLOCKS = [" ", "\u258f", "\u258e", "\u258d", "\u258c", "\u258b", "\u258a", "\u2589", "\u2588"];

					const BAR_CELLS = 20;
					const BAR_STEPS = BAR_CELLS * 8; // 160 levels

					const steps = ctxKnown ? Math.round((ctxPercent / 100) * BAR_STEPS) : 0;
					const fullCells = Math.floor(steps / 8);
					const remainder = steps % 8;
					const emptyCells = BAR_CELLS - fullCells - (remainder > 0 ? 1 : 0);

					// Bright ANSI bg for filled, ANSI 8 (dark gray) bg for empty
					const barEmptyBg = esc("48;5;8");
					let barFg: "success" | "warning" | "error" = "success";
					let barBg = esc("102");
					if (ctxPercent > 75) { barFg = "error"; barBg = esc("101"); }
					else if (ctxPercent > 50) { barFg = "warning"; barBg = esc("103"); }

					// Filled: bright bg spaces, transition: bright fg on dim bg, empty: dim bg spaces
					const filledPart = fullCells > 0 ? barBg + " ".repeat(fullCells) + RESET : "";
					const transitionPart = remainder > 0 ? barEmptyBg + theme.fg(barFg, EIGHTH_BLOCKS[remainder]) + RESET : "";
					const emptyPart = emptyCells > 0 ? barEmptyBg + " ".repeat(emptyCells) + RESET : "";

					const pctStr = ctxKnown ? `${ctxPercent.toFixed(0)}%` : "?%";
					const windowStr = formatTokens(ctxWindow);

					const contextRight =
						filledPart +
						transitionPart +
						emptyPart +
						" " +
						theme.fg(barFg, pctStr) +
						theme.fg("muted", `/${windowStr}`);

					// ── Line 1: session name (left) | model (right) ──

					const sessionName = ctx.sessionManager.getSessionName();
					const sessionLeft = sessionName
						? esc("1;45;30") + " " + sessionName + RESET + esc("35") + "\ue0b4" + RESET
						: theme.fg("dim", "untitled session");
					// Update terminal title with session name
					if (sessionName) {
						ctx.ui.setTitle(`π - ${sessionName}`);
					}

					const lines = [twoColumn(sessionLeft, modelRight, width)];

					// ── Line 2: worldpath + branch (left) | context gauge (right) ──

					const styledPath = highlightPath(cachedWorldPath, cachedRepoName).replace(/\//g, "\ue0bb");
					const branchPart = cachedBranch ? powerlineBranch(cachedBranch) : "";
					const pathLeft = styledPath + branchPart;

					lines.push(twoColumn(pathLeft, contextRight, width));

					// ── Line 3: extension statuses ──

					const statuses = footerData.getExtensionStatuses();
					if (statuses.size > 0) {
						const statusLine = Array.from(statuses.entries())
							.sort(([a], [b]) => a.localeCompare(b))
							.map(([, t]) => sanitize(t))
							.join(" ");
						lines.push(truncateToWidth(statusLine, width, theme.fg("muted", "...")));
					}

					return lines;
				},
			};
		});
	});
}
