/**
 * Agent Teams – coordinate multiple pi sessions via herdr, tmux, zellij, WezTerm mux, or Shuttle.
 *
 * Replicates Claude Code's agent-teams feature:
 *   • The team lead (@team_lead) occupies the left ⅓ of the terminal.
 *   • Teammates share the right ⅔, stacking vertically.
 *   • Communication happens through a file-based mailbox.
 *
 * The extension auto-detects whether it's running inside herdr, tmux, zellij,
 * WezTerm mux, or Shuttle and uses the appropriate pane manager.
 *
 * Environment variables (set automatically when spawning teammates):
 *   PI_TEAM_NAME          – team identifier
 *   PI_TEAM_ROLE          – "lead" | "teammate"
 *   PI_TEAM_AGENT_NAME    – the teammate's unique name
 *   PI_TEAM_SPAWN_KIND    – "teammate" | "subagent" (set for subagent teammates)
 *   PI_TEAM_SUBAGENT_ROLE – subagent role name
 *   PI_TEAM_SUBAGENT_FILE – subagent role file path
 */

import * as fs from "node:fs";
import * as path from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { keyHint } from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import { type AgentConfig, discoverAgents } from "./agents.js";
import { renderTeamMessageLines } from "./message-format.js";
import { formatContextUsageNote, safeContextUsage, stripContextUsageNote } from "./context-usage.js";
import {
	TEAMMATES_WORKING_COMPLETION_NOTE,
	TEAMMATES_WORKING_REMINDER,
	stripTrailingNotes,
} from "./team-notes.js";
import { type PaneManager, detectPaneManager } from "./pane-manager.js";
import {
  createPaneEnvHandoffRegistry,
  type PaneEnvHandoffRegistry,
} from "./pane-env.js";
import { isPaneBackedByCurrentManager } from "./pane-anchor.js";
import { RpcTeammate } from "./rpc-teammate.js";
import * as team from "./team.js";
import { isTransportAlive } from "./shared-utils.js";
import { TeamOverlay } from "./team-overlay.js";
import { leadSubagentPromptSection, teammateSubagentPromptSection } from "./prompts.js";
import { spawnTeammate as spawnTeammateImpl, type SpawnTeammateOptions } from "./spawn-teammate.js";
import { TeamCostTracker, formatTeamStatus, memberFromEntryData } from "./team-cost.js";
import { registerSubagentTool as registerAgentTeamsSubagentTool } from "./subagent-tool.js";
import type { MemberConfig } from "./types.js";
import { createMailboxWatcher, type MailboxWatcher } from "./watcher.js";
import {
  PI_TEAM_INSTANCE_ID_ENV,
  PI_TEAM_SESSION_DIR_ENV,
  acquirePrivateSessionLease,
  releasePrivateSessionLease,
  schedulePrivateSessionPrune,
  type PrivateSessionLease,
} from "./private-sessions.js";

const NOOP_PANE_ENV_HANDOFF_REGISTRY: PaneEnvHandoffRegistry = {
  register() {},
  cleanup() {},
  cleanupAll() {},
  cleanupAllAndWait: () => Promise.resolve(),
};

export default function (pi: ExtensionAPI) {
	// -----------------------------------------------------------------------
	// Pane manager detection (herdr, tmux, zellij, WezTerm mux, etc.)
	// -----------------------------------------------------------------------
	const paneManager: PaneManager | null = detectPaneManager();

	// -----------------------------------------------------------------------
	// Role detection
	// -----------------------------------------------------------------------
	const env = team.getTeamEnv();
	const isVscode = !!process.env.PI_VSCODE;
	// VS Code sessions join as teammate "vscode" when a team already exists,
	// even without PI_TEAM_ROLE — the pi-chat extension only sets PI_TEAM_NAME.
	const isTeammate = isVscode
		? !!env.teamName && !!team.readConfig(env.teamName)
		: env.role === "teammate" && !!env.teamName && !!env.agentName;

	// Teammates don't need desktop notifications — the lead is the human's interface.
	if (isTeammate) pi.events.emit("notify:disable");

	let currentTeamName: string | null = env.teamName ?? null;
	const myName: string = isVscode ? "vscode" : isTeammate ? env.agentName! : "team_lead";
	let privateSessionLease: PrivateSessionLease | null = null;

	let watcher: MailboxWatcher | null = null;

	const BATCH_CUSTOM_TYPE = "agent-teams-mailbox-batch-v1";
	const BATCH_PROTOCOL = "agent-teams-mailbox-batch-v1";
	const RESUME_RECHECK_MS = 25;
	const NO_START_WATCHDOG_MS = 5_000;
	type TeammateContext = { isIdle: () => boolean; ui?: { notify?: (message: string, level: string) => void } };
	type InFlightBatch = { batchId: string; messageIds: string[]; messages: team.TeamMessage[] };
	const inFlightBatches = new Map<string, InFlightBatch>();
	const inFlightMessageIds = new Set<string>();
	const awaitingStartBatchIds = new Set<string>();
	const batchIdsExpectedByCurrentRun = new Set<string>();
	const noStartTimers = new Map<string, ReturnType<typeof setTimeout>>();
	let flushInFlight = false;
	let pendingDrain = false;
	let teammateCtx: TeammateContext | null = null;
	let deferredTimer: ReturnType<typeof setTimeout> | null = null;
	let isTeammateBusy = false;
	let deliveryUnconfirmed = false;
	let resumeWarningSent = false;
	let batchSequence = 0;

	// -----------------------------------------------------------------------
	// RPC teammates (used when no terminal multiplexer is available)
	// -----------------------------------------------------------------------
	const rpcTeammates: Map<string, RpcTeammate> = new Map();
  const paneEnvHandoffs = isTeammate
    ? NOOP_PANE_ENV_HANDOFF_REGISTRY
    : createPaneEnvHandoffRegistry();
	let overlayHandle: { close: () => void; requestRender: () => void } | null = null;
	let overlayInstance: TeamOverlay | null = null;

	// Stashed UI reference for updating the status bar from callbacks
	let stashedUI: { setStatus: (key: string, text: string | undefined) => void } | null = null;
	let subagentToolRegisteredByAgentTeams = false;

	const COST_MEMBER_CUSTOM_TYPE = "agent-teams-cost-member-v1";
	const costTracker = new TeamCostTracker();
	let costRefreshTimer: ReturnType<typeof setInterval> | null = null;

	function rehydrateCostMembers(ctx: any): void {
		const entries = ctx.sessionManager?.getEntries?.() ?? [];
		for (const entry of entries) {
			if (entry.type !== "custom" || entry.customType !== COST_MEMBER_CUSTOM_TYPE) continue;
			const member = memberFromEntryData(entry.data);
			if (member) {
				costTracker.addMember(
					member.name,
					member.cwd,
					member.spawnedAt,
					member.instanceId,
					member.sessionDir,
				);
			}
		}
		if (costTracker.hasMembers()) ensureCostRefreshTimer();
	}

	function leadRpcCounts(): { running: number; idle: number } | null {
		const alive = Array.from(rpcTeammates.values()).filter((t) => t.isAlive);
		if (alive.length === 0) return null;
		const running = alive.filter((t) => t.status === "running" || t.status === "starting").length;
		return { running, idle: alive.length - running };
	}

	function refreshLeadStatus(): void {
		if (!stashedUI) return;
		stashedUI.setStatus(
			"agent-teams",
			formatTeamStatus({
				teamName: currentTeamName,
				cost: costTracker.hasMembers() ? costTracker.totalCost() : null,
				rpcCounts: leadRpcCounts(),
			}),
		);
	}

	function ensureCostRefreshTimer(): void {
		if (costRefreshTimer) return;
		costRefreshTimer = setInterval(refreshLeadStatus, 5_000);
		costRefreshTimer.unref?.();
	}

	function updateTeammateStatus(): void {
		refreshLeadStatus();
	}

	// -----------------------------------------------------------------------
	// Member liveness check
	// -----------------------------------------------------------------------
	// Works in both lead and teammate processes:
	//  - Lead with RPC teammates: check RpcTeammate.isAlive
	//  - Lead with pane manager: call the local isMemberAlive helper
	//  - Teammate (no rpcTeammates, no paneManager): use pid-based check
	//    for RPC transport members, treat self as alive, lead as alive.

	function isMemberAlive(m: MemberConfig): boolean {
		// Lead-local RPC state (only populated in the lead process)
		const rpcT = rpcTeammates.get(m.name);
		if (rpcT) return rpcT.isAlive;

		// Pane-manager path (terminal mux / native-pane lead)
		if (paneManager) return isTransportAlive(m, paneManager.isPaneAlive);

		// No pane manager and no RPC state — we're inside an RPC teammate
		// or a lead without a multiplexer and no local RPC entry.
		if (m.name === myName) return true;          // we're alive if we're asking
		if (m.role === "lead") return true;           // assume the lead is alive
		if (m.transport === "rpc" && m.pid) {
			try {
				process.kill(m.pid, 0);                   // signal 0 = existence check
				return true;
			} catch {
				return false;
			}
		}
		return false;
	}

	// Liveness we can *prove*, as opposed to liveness we assume.
	// isMemberAlive() answers "treat as alive?" and falls back to false whenever
	// this process has no way to observe the member — the normal case for
	// teammate→teammate checks, where there is no pane manager and no RPC entry.
	// Anything that blocks or deletes work must ask this instead, so an
	// unobservable member is never mistaken for a dead one.
	//
	// Observability is per member, not per process: a pane manager can only
	// answer for panes of its own backend, so an RPC member (paneId "none"), a
	// member spawned under a different multiplexer, or a spawn still in flight
	// all stay unobservable even though `paneManager` exists.
	function isMemberKnownDead(m: MemberConfig): boolean {
		if (m.name === myName || m.role === "lead") return false;
		if (m.state === "starting") return false;
		const rpcT = rpcTeammates.get(m.name);
		if (rpcT) return !rpcT.isAlive;
		if (m.transport === "rpc" || m.transport === "vscode") {
			// Only an ESRCH pid probe counts; a missing pid tells us nothing.
			return isConfirmedDeadLegacyProcessMember(m);
		}
		if (!paneManager) return false;
		if (m.transport && m.transport !== paneManager.kind) return false;
		if (!m.paneId || m.paneId === "none" || m.paneId === "pending") return false;
		return !paneManager.isPaneAlive(m.paneId);
	}

	function isConfirmedDeadLegacyProcessMember(member: MemberConfig): boolean {
		if (member.transport !== "rpc" && member.transport !== "vscode") return false;
		if (!member.pid) return false;
		try {
			process.kill(member.pid, 0);
			return false;
		} catch (error) {
			return isRecord(error) && error.code === "ESRCH";
		}
	}

	// -----------------------------------------------------------------------
	// Agent identity discovery (lazy)
	// -----------------------------------------------------------------------
	// Discover defined agents (from ~/.pi/agent/agents/ and .pi/agents/)
	// so we can load their identity when team_spawn matches by name.
	// Lazy: runs on first team_spawn, not at extension load time.  This
	// ensures other extensions (e.g. context-guardian) have time to set up
	// agent symlinks before we scan the directory.
	let agentRoster: AgentConfig[] | null = null;
	function getAgentRoster(): AgentConfig[] {
		if (agentRoster !== null) return agentRoster;
		try {
			const discovery = discoverAgents(process.cwd(), "both");
			agentRoster = discovery.agents;
		} catch {
			agentRoster = [];
		}
		return agentRoster;
	}

	// -----------------------------------------------------------------------
	// Mailbox watching (fs.watch + fallback poll)
	// -----------------------------------------------------------------------
	// Instead of polling every 2 s, we watch the mailbox directory with
	// fs.watch() so messages arrive within milliseconds.  A slow fallback
	// poll (every 30 s) catches any events the OS watcher might drop.
	// Cap message bodies entering model context. pollMailbox() unlinks mailbox
	// JSON files as it reads them, so the full body is spilled to a durable
	// per-team oversized/ file and the truncation note points there.
	const MESSAGE_BODY_LIMIT = 4000;

	function boundMessageBody(body: string, msg: team.TeamMessage): string {
		if (body.length <= MESSAGE_BODY_LIMIT) return body;
		const omitted = body.length - MESSAGE_BODY_LIMIT;
		const note = `[…truncated ${omitted} chars`;
		try {
			const dir = team.oversizedDir(currentTeamName!);
			fs.mkdirSync(dir, { recursive: true });
			const file = path.join(dir, `${msg.id}.txt`);
			fs.writeFileSync(file, body);
			return `${body.slice(0, MESSAGE_BODY_LIMIT)}\n\n${note} — full message: ${file}]`;
		} catch {
			return `${body.slice(0, MESSAGE_BODY_LIMIT)}\n\n${note} — failed to spill full message to disk]`;
		}
	}

	function deliverTeamMessage(msg: team.TeamMessage, body: string, triggerTurn: boolean): void {
		// Bound only the sender's own content. Steering notes appended by
		// drainLeadMailbox ("just acknowledge" / "integrate now") and the
		// context-usage note below are required model-visible protocol text
		// (see team-notes.ts) and must survive truncation, so they are
		// appended after bounding.
		const note = body.startsWith(msg.content) ? body.slice(msg.content.length).trimStart() : "";
		const bounded = boundMessageBody(msg.content, msg);
		// Append the sender's reported context usage as a trailing note. It is
		// delivered to the reading agent (so the lead can decide to retire and
		// replace a teammate that is running low) but hidden in the collapsed view.
		const usageNote = formatContextUsageNote(msg.from, msg.contextUsage);
		const finalBody = [bounded, note, usageNote].filter((part): part is string => Boolean(part && part.length > 0)).join("\n\n");
		pi.sendMessage(
			{
				customType: "team-message",
				content: `[Team message from @${msg.from}]: ${finalBody}`,
				display: true,
				details: { from: msg.from, to: msg.to, content: finalBody, timestamp: msg.timestamp },
			},
			{ triggerTurn, deliverAs: "steer" },
		);
	}

	function drainLeadMailbox(): void {
		if (!currentTeamName) return;
		const messages = team.pollMailbox(currentTeamName, myName);
		const config = !isTeammate ? team.readConfig(currentTeamName) : null;
		for (const msg of messages) {
			const isCompletion = msg.content.startsWith("[COMPLETED]");
			const hasWorkingTeammate = config?.members.some(
				(m) => m.role === "teammate" && m.name !== msg.from && isMemberAlive(m),
			);
			let body: string;
			if (isCompletion && hasWorkingTeammate) {
				body = `${msg.content}\n\n${TEAMMATES_WORKING_COMPLETION_NOTE}`;
			} else if (!isCompletion && hasWorkingTeammate) {
				body = `${msg.content}\n\n${TEAMMATES_WORKING_REMINDER}`;
			} else {
				body = msg.content;
			}
			deliverTeamMessage(msg, body, true);
		}
	}

	function formatBatch(messages: team.TeamMessage[]): string {
		if (messages.length === 0) throw new Error("Cannot format an empty Agent Teams batch");
		const canonical = messages.map((message) => ({
			id: message.id,
			from: message.from,
			to: message.to,
			content: boundMessageBody(message.content, message),
			timestamp: message.timestamp,
			order: message.order,
			...(message.contextUsage === undefined ? {} : { contextUsage: message.contextUsage }),
		}));
		return `Agent Teams batch v1 — ${messages.length} message(s)\n${JSON.stringify(canonical, null, 2)}`;
	}

	function correlatedBatchIndexes(messages: unknown[], batch: InFlightBatch): number[] {
		const indexes: number[] = [];
		for (let index = 0; index < messages.length; index++) {
			const message = messagePayload(messages[index]);
			if (!message || message.role !== "custom" || message.customType !== BATCH_CUSTOM_TYPE) continue;
			const details = isRecord(message.details) ? message.details : null;
			if (!details || details.protocol !== BATCH_PROTOCOL || details.batchId !== batch.batchId) continue;
			if (!Array.isArray(details.messageIds) || details.messageIds.length !== batch.messageIds.length) continue;
			if (!details.messageIds.every((id, idIndex) => id === batch.messageIds[idIndex])) continue;
			indexes.push(index);
		}
		return indexes;
	}

	function recordMemberState(status: team.MemberStatus, reason?: string): string | null {
		if (!currentTeamName || !isTeammate) return null;
		if (deliveryUnconfirmed && status === "working") return null;
		const state: team.MemberState = { status, updatedAt: Date.now() };
		if (reason !== undefined) state.reason = reason;
		if (status === "working") state.lastProgressAt = state.updatedAt;
		try {
			team.writeMemberState(currentTeamName, myName, state);
			return null;
		} catch (error) {
			return `Warning: message delivered, but member state could not be updated (${error instanceof Error ? error.message : String(error)}).`;
		}
	}

	function recordOutboundState(content: string): string | null {
		if (content.startsWith("[COMPLETED]")) return recordMemberState("completed-awaiting-shutdown");
		if (content.startsWith("[NEEDS-ASSIST]")) return recordMemberState("waiting-for-lead", content.slice("[NEEDS-ASSIST]".length).trim() || "assistance requested");
		return recordMemberState("working");
	}

	function warnResumeOnce(reason: string): void {
		if (resumeWarningSent || !currentTeamName) return;
		resumeWarningSent = true;
		try {
			team.sendMessage(currentTeamName, myName, "team_lead", `[RESUME-WARNING] ${reason}`);
		} catch {
			// The durable mailbox batch remains untouched even if the warning cannot be published.
		}
	}

	function clearNoStartWatchdog(batchId: string): void {
		const timer = noStartTimers.get(batchId);
		if (timer !== undefined) clearTimeout(timer);
		noStartTimers.delete(batchId);
		awaitingStartBatchIds.delete(batchId);
	}

	function clearNoStartWatchdogs(): void {
		for (const timer of noStartTimers.values()) clearTimeout(timer);
		noStartTimers.clear();
		awaitingStartBatchIds.clear();
	}

	function removeInFlightBatch(batch: InFlightBatch): void {
		inFlightBatches.delete(batch.batchId);
		batchIdsExpectedByCurrentRun.delete(batch.batchId);
		for (const messageId of batch.messageIds) inFlightMessageIds.delete(messageId);
		clearNoStartWatchdog(batch.batchId);
	}

	function markDeliveryUnconfirmed(): void {
		batchIdsExpectedByCurrentRun.clear();
		if (deliveryUnconfirmed) return;
		deliveryUnconfirmed = true;
		clearNoStartWatchdogs();
		recordMemberState("waiting-for-lead", "resume delivery unconfirmed; restart teammate");
		warnResumeOnce("Resume delivery was not correlated; mailbox bytes were retained. Restart the teammate before retrying.");
	}

	function armNoStartWatchdog(batchId: string): void {
		const existingTimer = noStartTimers.get(batchId);
		if (existingTimer !== undefined) clearTimeout(existingTimer);
		noStartTimers.set(batchId, setTimeout(() => {
			noStartTimers.delete(batchId);
			if (!awaitingStartBatchIds.has(batchId) || !inFlightBatches.has(batchId)) return;
			let idle = false;
			try {
				idle = teammateCtx?.isIdle() === true;
			} catch {
				idle = false;
			}
			if (!idle) {
				armNoStartWatchdog(batchId);
				return;
			}
			markDeliveryUnconfirmed();
		}, NO_START_WATCHDOG_MS));
	}

	function peekDeliverableMailboxMessages(): team.TeamMessage[] {
		const snapshot = team.peekMailboxBatch(currentTeamName!, myName);
		return snapshot.messages.filter((message) => !inFlightMessageIds.has(message.id));
	}

	function runResumeFlush(): void {
		flushInFlight = false;
		deferredTimer = null;
		if (!pendingDrain || deliveryUnconfirmed) return;
		if (!teammateCtx) return;
		let idle = false;
		try {
			idle = teammateCtx.isIdle() === true;
		} catch {
			idle = false;
		}
		if (inFlightBatches.size === 0 && (isTeammateBusy || !idle)) {
			scheduleResumeFlush(RESUME_RECHECK_MS);
			return;
		}
		pendingDrain = false;
		let messages: team.TeamMessage[];
		try {
			messages = peekDeliverableMailboxMessages();
		} catch (error) {
			pendingDrain = true;
			deliveryUnconfirmed = true;
			recordMemberState("waiting-for-lead", "mailbox batch validation failed; restart teammate");
			warnResumeOnce(
				`Mailbox batch validation failed: ${error instanceof Error ? error.message : String(error)}. Mailbox bytes were retained. Restart the teammate before retrying.`,
			);
			return;
		}
		if (messages.length === 0) return;
		const batchId = `batch-${Date.now()}-${++batchSequence}`;
		const batch = { batchId, messageIds: messages.map((message) => message.id), messages };
		inFlightBatches.set(batchId, batch);
		for (const messageId of batch.messageIds) inFlightMessageIds.add(messageId);
		pi.sendMessage(
			{
				customType: BATCH_CUSTOM_TYPE,
				content: formatBatch(batch.messages),
				display: true,
				details: { protocol: BATCH_PROTOCOL, batchId, messageIds: [...batch.messageIds] },
			},
			{ triggerTurn: true, deliverAs: "steer" },
		);
		if (!isTeammateBusy && idle) {
			awaitingStartBatchIds.add(batchId);
			armNoStartWatchdog(batchId);
		}
	}

	function scheduleResumeFlush(delay = 0): void {
		pendingDrain = true;
		if (flushInFlight || deliveryUnconfirmed) return;
		flushInFlight = true;
		deferredTimer = setTimeout(runResumeFlush, delay);
	}

	function startWatching(): void {
		if (!currentTeamName || watcher) return;
		const dir = team.mailboxDir(currentTeamName, myName);
		watcher = createMailboxWatcher({ dir, onMessages: isTeammate ? () => scheduleResumeFlush() : drainLeadMailbox });
		watcher.start();
	}
	function stopWatching(): void {
		if (watcher) {
			watcher.stop();
			watcher = null;
		}
	}

	function isRecord(value: unknown): value is Record<string, unknown> {
		return typeof value === "object" && value !== null;
	}

	function normalizeTeamMemberName(value: unknown): string {
		return typeof value === "string" ? value.replace(/^@/, "") : "";
	}

	function messagePayload(value: unknown): Record<string, unknown> | null {
		if (!isRecord(value)) return null;
		return isRecord(value.message) ? value.message : value;
	}

	type InboundTeamMessage = {
		from: string;
	};

	function inboundTeamMessage(value: unknown): InboundTeamMessage | null {
		const message = messagePayload(value);
		if (!message || message.customType !== "team-message") return null;
		const details = isRecord(message.details) ? message.details : {};
		const from = normalizeTeamMemberName(details.from);
		const to = normalizeTeamMemberName(details.to);
		if (!from || from === myName) return null;
		if (to && to !== myName) return null;
		return { from };
	}

	const TEAM_COMMUNICATION_TOOLS = new Set(["team_message", "team_broadcast", "team_shutdown"]);

	function hasSuccessfulTeamCommunication(messages: unknown[]): boolean {
		for (const rawMessage of messages) {
			const message = messagePayload(rawMessage);
			if (!message || message.role !== "toolResult" || message.isError === true) continue;
			if (typeof message.toolName === "string" && TEAM_COMMUNICATION_TOOLS.has(message.toolName)) return true;
		}
		return false;
	}

	function latestInboundTeamMessage(messages: unknown[]): { message: InboundTeamMessage; index: number } | null {
		for (let i = messages.length - 1; i >= 0; i--) {
			const message = inboundTeamMessage(messages[i]);
			if (message) return { message, index: i };
		}
		return null;
	}

	function formatSenderList(senders: string[]): string {
		const names = senders.map((sender) => `@${sender}`);
		if (names.length <= 1) return names[0] ?? "another teammate";
		if (names.length === 2) return `${names[0]} and ${names[1]}`;
		return `${names.slice(0, -1).join(", ")}, and ${names[names.length - 1]}`;
	}

	function sendMissingTeamReportReminder(senders: string[]): void {
		const senderText = formatSenderList(senders);
		const promptPhrase = senders.length === 1
			? `a team message from ${senderText}`
			: `team messages from ${senderText}`;
		pi.sendMessage(
			{
				customType: "agent-teams-report-reminder",
				content: [
					`Agent Teams reminder: this turn appears to have been prompted by ${promptPhrase}, and no team message was observed.`,
					"",
					"Consider whether another teammate needs a result, blocker, question, or final report. If yes, send it with `team_message` to the appropriate teammate, or use `team_shutdown` with a complete final report if you are done.",
					"",
					"If you were told not to report yet, have nothing useful to share, or are intentionally waiting, you may ignore this reminder.",
				].join("\n"),
				display: true,
				details: { senders, reason: "missing-team-message" },
			},
			{ triggerTurn: true, deliverAs: "followUp" },
		);
	}

	function queueMissingTeamReportReminder(senders: string[], ctx: unknown): void {
		const trySend = (attempt: number) => {
			const isIdle = isRecord(ctx) && typeof ctx.isIdle === "function" ? ctx.isIdle : null;
			try {
				if (!isIdle || isIdle.call(ctx) || attempt >= 10) {
					sendMissingTeamReportReminder(senders);
					return;
				}
			} catch {
				sendMissingTeamReportReminder(senders);
				return;
			}
			setTimeout(() => trySend(attempt + 1), 10);
		};
		setTimeout(() => trySend(0), 0);
	}

	// -----------------------------------------------------------------------
	// Lazy team creation (lead only)
	// -----------------------------------------------------------------------

	let teamInitialized = false;

	function ensureTeam(name?: string): string {
		if (!currentTeamName) {
			currentTeamName = name || env.teamName || `team-${Date.now()}`;
		}
		// Always verify the config file still exists on disk.
		// It may have been cleaned up by team_cleanup, a previous session,
		// or a race condition — in which case we need to recreate it.
		const configExists = team.readConfig(currentTeamName) !== null;
		if (!teamInitialized || !configExists) {
			teamInitialized = true;
			const paneId = paneManager ? paneManager.getCurrentPaneId() : "none";
			const existing = team.readConfig(currentTeamName);
			if (existing) {
				// Rejoin existing team as new lead
				existing.leadPaneId = paneId;
				const lead = existing.members.find((m) => m.role === "lead");
				if (lead) lead.paneId = paneId;
				existing.cwd = process.cwd();
				team.writeConfig(currentTeamName, existing);
			} else {
				team.createTeam(currentTeamName, paneId, process.cwd());
			}
			startWatching();
		}
		return currentTeamName;
	}

	// -----------------------------------------------------------------------
	// System prompt injection
	// -----------------------------------------------------------------------

	pi.on("before_agent_start", async (event) => {
		let systemPrompt = event.systemPrompt;
		let changed = false;

		if (!isTeammate && subagentToolRegisteredByAgentTeams) {
			systemPrompt += leadSubagentPromptSection();
			changed = true;
		}

		if (isTeammate && currentTeamName) {
			const config = team.readConfig(currentTeamName);
			if (config) {
				const extra = [
					"\n\n## Agent Team Context",
					`You are teammate **@${myName}** in team "${currentTeamName}" led by @team_lead.`,
					"",
					"Available team tools:",
					"- **team_status** – see all team members and whether their panes are alive.",
					"- **team_message** – send a message to any member (by name, e.g. `team_lead`). Use this for reports, questions, and blockers.",
					"- **team_broadcast** – send a message to every other member.",
					"- **team_shutdown** – send an optional final report and gracefully exit. Use this when done and exiting.",
					"",
					"Workflow:",
					"1. Work on your assigned task.",
					"2. Use team_message to coordinate with teammates when needed.",
					"3. When done or blocked, use team_message to report back to @team_lead with a complete, self-contained report. Include what you did, key findings/results, relevant evidence, files changed or inspected, caveats, tradeoffs, unresolved questions, and any recommended next steps.",
					"4. If you are done and ready to exit, use team_shutdown after sending or including your final report.",
					"5. Never go idle without reporting back — the lead can only act when you send a message.",
					"6. If you receive a `[SHUTDOWN_REQUEST]`, wrap up promptly and call team_shutdown.",
					"7. These instructions can be superseded by the lead or the user.",
				].join("\n");

				systemPrompt += extra;
				changed = true;
			}
		}

		if (isTeammate && process.env.PI_TEAM_SPAWN_KIND === "subagent") {
			systemPrompt += teammateSubagentPromptSection();
			changed = true;
		}

		if (!changed) return;
		return { systemPrompt };
	});

	if (isTeammate) {
		pi.on("agent_start", async () => {
			isTeammateBusy = true;
			batchIdsExpectedByCurrentRun.clear();
			for (const batchId of inFlightBatches.keys()) batchIdsExpectedByCurrentRun.add(batchId);
			for (const batchId of [...awaitingStartBatchIds]) clearNoStartWatchdog(batchId);
			recordMemberState("working");
		});

		pi.on("agent_end", async (event: { messages?: unknown[] }, ctx: unknown) => {
			isTeammateBusy = false;
			const messages = Array.isArray(event.messages) ? event.messages : [];
			const batches = [...inFlightBatches.values()];
			if (batches.length > 0) {
				const correlations = batches.map((batch) => ({ batch, wrapperIndexes: correlatedBatchIndexes(messages, batch) }));
				for (const { batch, wrapperIndexes } of correlations) {
					if (wrapperIndexes.length !== 1) continue;
					const latestSender = [...batch.messages].reverse().find((message) => message.from !== myName)?.from;
					if (latestSender && !hasSuccessfulTeamCommunication(messages.slice(wrapperIndexes[0] + 1))) {
						queueMissingTeamReportReminder([latestSender], ctx);
					}
					team.deleteMailboxMessages(currentTeamName!, myName, batch.messageIds);
					removeInFlightBatch(batch);
				}
				if (correlations.some(({ batch, wrapperIndexes }) => batchIdsExpectedByCurrentRun.has(batch.batchId) && wrapperIndexes.length !== 1)) {
					markDeliveryUnconfirmed();
				} else {
					for (const batch of inFlightBatches.values()) {
						if (batchIdsExpectedByCurrentRun.has(batch.batchId)) continue;
						awaitingStartBatchIds.add(batch.batchId);
						armNoStartWatchdog(batch.batchId);
					}
					deliveryUnconfirmed = false;
					resumeWarningSent = false;
					scheduleResumeFlush();
				}
			} else {
				const teamMessage = latestInboundTeamMessage(messages);
				if (teamMessage && !hasSuccessfulTeamCommunication(messages.slice(teamMessage.index + 1))) {
					queueMissingTeamReportReminder([teamMessage.message.from], ctx);
				}
				scheduleResumeFlush();
			}
		});
	}

	// -----------------------------------------------------------------------
	// Teammate startup – deliver queued mailbox messages
	// -----------------------------------------------------------------------

	if (isTeammate) {
		pi.on("session_start", async (_event, ctx) => {
			const instanceId = process.env[PI_TEAM_INSTANCE_ID_ENV];
			const sessionDir = process.env[PI_TEAM_SESSION_DIR_ENV];
			if ((instanceId === undefined) !== (sessionDir === undefined)) {
				throw new Error("Agent Teams teammate session identity requires both instance ID and session directory");
			}
			if (instanceId !== undefined && sessionDir !== undefined && privateSessionLease === null) {
				try {
					privateSessionLease = acquirePrivateSessionLease(sessionDir, instanceId);
				} catch (error) {
					const message = error instanceof Error ? error.message : String(error);
					ctx.ui.notify(`Agent Teams could not create the private-session lease: ${message}`, "warning");
				}
			}

			// VS Code teammates self-register into the existing team
			if (isVscode && currentTeamName) {
				const config = team.readConfig(currentTeamName);
				if (config && !config.members.some((m) => m.name === "vscode")) {
					team.addMember(currentTeamName, {
						name: "vscode",
						role: "teammate",
						paneId: "none",
						transport: "vscode",
						pid: process.pid,
						spawnedAt: Date.now(),
					});
				} else if (config) {
					// Update PID on reconnect
					const member = config.members.find((m) => m.name === "vscode");
					if (member) {
						member.pid = process.pid;
						member.transport = "vscode";
						team.writeConfig(currentTeamName, config);
					}
				}
			}
			startWatching();

			// Name the session so the pane/tab is identifiable
			pi.setSessionName(`@${myName}`);

			// Show role + team in footer
			ctx.ui.setStatus("agent-teams", currentTeamName ? `@${myName} [${currentTeamName}]` : `@${myName}`);
			// Notify listeners that this teammate session has started
			if (currentTeamName) {
				pi.events.emit("team:session", {
					teamName: currentTeamName, name: myName, cwd: process.cwd(),
				});
			}
			// Startup catch-up uses the same nondestructive, safe-idle scheduler as watcher events.
			if (currentTeamName && typeof (ctx as TeammateContext).isIdle === "function") {
				teammateCtx = ctx as TeammateContext;
				scheduleResumeFlush();
			}
		});
	} else {
		pi.on("session_start", async (_event, ctx) => {
			schedulePrivateSessionPrune();
			// Stash UI ref for RPC status bar updates from callbacks
			stashedUI = ctx.ui;
			if (!subagentToolRegisteredByAgentTeams) {
				const tools = typeof (pi as any).getAllTools === "function" ? (pi as any).getAllTools() : [];
				const hasSubagentTool = tools.some((tool: { name?: string }) => tool.name === "subagent");
				if (!hasSubagentTool) {
					registerSubagentTool();
					subagentToolRegisteredByAgentTeams = true;
				}
			}
			if (currentTeamName) {
				// Eagerly create/rejoin the team when PI_TEAM_NAME is set,
				// so team_status works before the first team_spawn.
				ensureTeam();
				startWatching();
			}
			rehydrateCostMembers(ctx);
			refreshLeadStatus();
		});
	}

	// -----------------------------------------------------------------------
	// Cleanup on shutdown
	// -----------------------------------------------------------------------

	pi.on("session_shutdown", async () => {
		if (privateSessionLease) {
			releasePrivateSessionLease(privateSessionLease);
			privateSessionLease = null;
		}
    if (!isTeammate) await paneEnvHandoffs.cleanupAllAndWait();
		stopWatching();
		if (costRefreshTimer) {
			clearInterval(costRefreshTimer);
			costRefreshTimer = null;
		}
		if (deferredTimer) {
			clearTimeout(deferredTimer);
			deferredTimer = null;
		}
		clearNoStartWatchdogs();
		batchIdsExpectedByCurrentRun.clear();
		flushInFlight = false;
		teammateCtx = null;
		if (isTeammate && currentTeamName) {
			const teamName = currentTeamName;
			team.sendMessage(teamName, myName, "team_lead", `@${myName} has shut down.`);
			const member = team.readConfig(teamName)?.members.find((candidate) => candidate.name === myName);
			if (member?.instanceId) team.markMemberStopping(teamName, myName, member.instanceId);
			// RPC removal is deferred to the exact instance's observed exit callback.
			if (!paneManager) {
				setTimeout(() => process.exit(0), 500);
			}
		}
		// Lead shutdown marks each exact RPC instance stopping and retains map/config
		// until its observed exit callback performs the single conditional removal.
		if (!isTeammate && currentTeamName) {
			const config = team.readConfig(currentTeamName);
			for (const [name, rpc] of rpcTeammates) {
				const member = config?.members.find((candidate) => candidate.name === name);
				if (member?.instanceId) team.markMemberStopping(currentTeamName, name, member.instanceId);
				rpc.kill();
			}
		}
	});

	// -----------------------------------------------------------------------
	// Overlay shortcut (Ctrl+Shift+M) — view RPC teammate output
	// -----------------------------------------------------------------------

	if (!isTeammate) {
		pi.registerShortcut("ctrl+shift+m", {
			description: "Toggle agent teams overlay",
			handler: async (ctx) => {
				if (rpcTeammates.size === 0) {
					ctx.ui.notify("No RPC teammates active. Use team_spawn to create teammates.", "info");
					return;
				}

				await ctx.ui.custom<undefined>(
					(tui, theme, _kb, done) => {
						const teammates = Array.from(rpcTeammates.values());
						const overlay = new TeamOverlay({
							teammates,
							theme,
							done,
							requestRender: () => tui.requestRender(),
						});
						overlayInstance = overlay;

						// Store handle for external re-renders
						overlayHandle = {
							close: () => done(undefined),
							requestRender: () => tui.requestRender(),
						};

						return {
							render: (w: number) => overlay.render(w),
							invalidate: () => overlay.invalidate(),
							handleInput: (data: string) => {
								overlay.handleInput(data);
								tui.requestRender();
							},
						};
					},
					{ overlay: true },
				);

				// Clean up handle when overlay closes
				overlayHandle = null;
				overlayInstance = null;
			},
		});
	}

	// -----------------------------------------------------------------------
	// Custom message renderers
	// -----------------------------------------------------------------------

	function expandHint(): string {
		try {
			return keyHint("app.tools.expand", "expand");
		} catch {
			return "Ctrl+O to expand";
		}
	}

	function fullMessageText(value: unknown): string {
		return typeof value === "string" ? value : "";
	}

  // The message background follows Pi's theme. Keep the rail alone by
  // preference with PI_TEAM_MESSAGE_BG=0/off/false/none.
	function teamMessageBackgroundEnabled(): boolean {
		const v = (process.env.PI_TEAM_MESSAGE_BG ?? "").trim().toLowerCase();
		return !(v === "0" || v === "off" || v === "false" || v === "no" || v === "none");
	}

	pi.registerMessageRenderer("team-message", (message, { expanded }, theme) => {
		const d = message.details as { from?: string; content?: string } | undefined;
		const from = d?.from ?? "unknown";
		const full = fullMessageText(d?.content ?? message.content);
		// Presentation only: appended notes (teammate-working reminders and the
		// sender's context-usage line) stay in the delivered body, but we hide them
		// in the collapsed view and reveal them when expanded. The usage note is the
		// outermost trailing note, so strip it first.
		const content = expanded ? full : stripTrailingNotes(stripContextUsageNote(full));
		const background = teamMessageBackgroundEnabled();
		// Rendered lazily so the width-aware blockquote fits the live pane width.
		return {
			render: (width: number) => renderTeamMessageLines({ from, content, width, theme, background }),
			invalidate: () => {},
		};
	});

	pi.registerMessageRenderer("agent-teams-report-reminder", (message, { expanded }, theme) => {
		const header = theme.fg("warning", "[Agent reminded to report]");
		if (!expanded) return new Text(`${header} ${theme.fg("dim", `(${expandHint()})`)}`, 0, 0);
		return new Text(`${header}\n${fullMessageText(message.content)}`, 0, 0);
	});

	// ===================================================================
	// SHARED TOOLS (both lead and teammate)
	// ===================================================================

	function memberStatusLabel(member: MemberConfig): "dead" | "active" | team.MemberStatus {
		if (!isMemberAlive(member)) return "dead";
		return team.readMemberState(currentTeamName!, member.name)?.status ?? "active";
	}

	const TASK_PREVIEW_LENGTH = 80;

	function taskPreview(task: string | undefined): string {
		if (!task) return "";
		const oneLine = task.replace(/\s+/g, " ").trim();
		if (oneLine.length <= TASK_PREVIEW_LENGTH) return ` — ${oneLine}`;
		return ` — ${oneLine.slice(0, TASK_PREVIEW_LENGTH)}…`;
	}

	// One computed view, shared by execute() and renderResult(), so the panel and
	// the model can never drift apart again.
	type StatusEntry = { name: string; role: string; model?: string; status: string; task: string };
	type StatusView = { teamName: string; live: StatusEntry[]; reaped: string[]; stillDead: string[] };

	function statusSummaryLines(view: StatusView): string[] {
		const lines: string[] = [];
		if (view.reaped.length > 0) {
			lines.push(`removed ${view.reaped.length} finished: ${view.reaped.map((n) => `@${n}`).join(", ")}`);
		}
		if (view.stillDead.length > 0) {
			lines.push(`${view.stillDead.length} dead: ${view.stillDead.map((n) => `@${n}`).join(", ")}`);
		}
		return lines;
	}

	function statusLines(view: StatusView): string[] {
		const lines = [`Team: ${view.teamName}`, ""];
		for (const m of view.live) {
			const model = m.model ? ` [${m.model}]` : "";
			lines.push(`  @${m.name} (${m.role})${model} ✓ ${m.status}${m.task}`);
		}
		for (const line of statusSummaryLines(view)) lines.push(`  ✗ ${line}`);
		return lines;
	}

	pi.registerTool({
		name: "team_status",
		label: "Team Status",
		description:
			"For the lead to check which agents currently exist before making changes to the team. " +
			"Not for monitoring, status polling, or waiting for progress.",
		parameters: Type.Object({}),

		async execute() {
			if (!currentTeamName) {
				return {
					content: [{ type: "text", text: "No active team." }],
					details: { config: undefined } as { config: team.TeamConfig | undefined },
				};
			}
			const config = team.readConfig(currentTeamName);
			if (!config) {
				return {
					content: [{ type: "text", text: "Team config not found." }],
					details: { config: undefined } as { config: team.TeamConfig | undefined },
				};
			}

			const live: StatusEntry[] = [];
			const deadMembers: MemberConfig[] = [];
			for (const m of config.members) {
				const status = memberStatusLabel(m);
				if (status === "dead") {
					deadMembers.push(m);
					continue;
				}
				live.push({ name: m.name, role: m.role, model: m.model, status, task: taskPreview(m.task) });
			}

			// A teammate that finishes on its own is never unregistered, so a
			// long-lived team name accumulates every worker it ever ran. Drop the
			// ones we can prove are gone, but only where this process can observe
			// them and only when no shutdown handshake is still pending — an RPC
			// member's removal belongs to its own observed-exit callback.
			//
			// Removal is instance-scoped, never by name: the config is shared on
			// disk and names are reusable, so between this snapshot and the write
			// another process can have respawned the same name. A member with no
			// instanceId predates that identity and cannot be proven, so it is left
			// for an explicit team_force_shutdown.
			const reaped: string[] = [];
			for (const m of deadMembers) {
				if (m.role !== "teammate" || !m.instanceId) continue;
				if (!isMemberKnownDead(m) || rpcTeammates.has(m.name)) continue;
				if (team.removeMemberRegistration(currentTeamName, m.name, m.instanceId) !== "removed") continue;
				reaped.push(m.name);
				pi.events.emit("team:remove", { teamName: currentTeamName, name: m.name });
			}
			if (reaped.length > 0) updateTeammateStatus();
			const stillDead = deadMembers.filter((m) => !reaped.includes(m.name)).map((m) => m.name);

			const view: StatusView = { teamName: config.name, live, reaped, stillDead };
			return {
				content: [{ type: "text", text: statusLines(view).join("\n") }],
				details: { view } as { view: StatusView | undefined },
			};
		},

		renderResult(result, _opts, theme) {
			const view = (result.details as any)?.view as StatusView | undefined;
			if (!view) {
				const t = result.content[0];
				return new Text(t?.type === "text" ? t.text : "(no data)", 0, 0);
			}
			// Renders the same collapsed view the model gets: full task text for
			// every member ever spawned is what turned this panel into a wall.
			let text = theme.fg("toolTitle", theme.bold(`Team ${view.teamName}`));
			for (const m of view.live) {
				const icon = theme.fg("success", "✓");
				const name = theme.fg("accent", `@${m.name}`);
				const role = theme.fg("muted", ` (${m.role})`);
				const model = m.model ? theme.fg("muted", ` [${m.model}]`) : "";
				const statusText = theme.fg("muted", ` ${m.status}`);
				const task = m.task ? theme.fg("dim", m.task) : "";
				text += `\n  ${icon} ${name}${role}${model}${statusText}${task}`;
			}
			for (const line of statusSummaryLines(view)) {
				text += `\n  ${theme.fg("error", "✗")} ${theme.fg("muted", line)}`;
			}
			return new Text(text, 0, 0);
		},
	});

	pi.registerTool({
		name: "team_message",
		label: "Team Message",
		description:
			"Send a message to a specific team member. Use their name without the @ prefix (e.g. 'researcher', 'team_lead').",
		parameters: Type.Object({
			to: Type.String({ description: "Recipient name (e.g. 'researcher', 'team_lead')" }),
			content: Type.String({ description: "Message content" }),
		}),

		async execute(_id, params, _signal, _onUpdate, ctx) {
			if (!currentTeamName) {
				return { content: [{ type: "text", text: "No active team." }], details: {}, isError: true };
			}
			const to = params.to.replace(/^@/, "");
			// A mailbox write always "succeeds" — the file lands even when nobody is
			// left to read it. Refuse the send when the recipient is unknown or
			// provably gone, so a finished worker's queue is not mistaken for work.
			const senderConfig = team.readConfig(currentTeamName);
			const recipient = senderConfig?.members.find((m) => m.name === to);
			if (senderConfig && !recipient) {
				return {
					content: [{ type: "text", text: `No team member named @${to} — message not sent. Use team_status to see who exists.` }],
					details: { to, delivered: false },
					isError: true,
				};
			}
			if (recipient && isMemberKnownDead(recipient)) {
				return {
					content: [{ type: "text", text: `@${to} has exited — message NOT delivered and no work will happen. Respawn with team_spawn (repeat the full brief; the new instance has no memory of the old one) or pick a live member.` }],
					details: { to, delivered: false },
					isError: true,
				};
			}
			team.sendMessage(currentTeamName, myName, to, params.content, safeContextUsage(ctx));
			const stateWarning = recordOutboundState(params.content);
			const text = `Message sent to @${to}.${stateWarning ? ` ${stateWarning}` : ""}`;
			return { content: [{ type: "text", text }], details: { to } };
		},

		renderCall(args, theme, context) {
			const to = (args.to as string)?.replace(/^@/, "") ?? "?";
			const content = typeof args.content === "string" ? args.content : "";
			let text = theme.fg("toolTitle", theme.bold("team_message ")) + theme.fg("accent", `@${to}`);
			if (content) {
				text += context?.expanded === true
					? `\n${content}`
					: ` ${theme.fg("dim", `(${expandHint()})`)}`;
			}
			return new Text(text, 0, 0);
		},
	});

	pi.registerTool({
		name: "team_broadcast",
		label: "Broadcast",
		description: "Send a message to all other team members at once. Use sparingly — costs scale with team size.",
		parameters: Type.Object({
			content: Type.String({ description: "Message content to broadcast" }),
		}),

		async execute(_id, params, _signal, _onUpdate, ctx) {
			if (!currentTeamName) {
				return { content: [{ type: "text", text: "No active team." }], details: {}, isError: true };
			}
			const config = team.readConfig(currentTeamName);
			if (!config) {
				return { content: [{ type: "text", text: "Team config not found." }], details: {}, isError: true };
			}
			const usage = safeContextUsage(ctx);
			const others = config.members.filter((m) => m.name !== myName);
			// Same rule as team_message: never report delivery to a member we can
			// prove has exited. A long-lived team is mostly finished workers.
			const skipped = others.filter((m) => isMemberKnownDead(m)).map((m) => m.name);
			const recipients = others.filter((m) => !skipped.includes(m.name));
			for (const m of recipients) {
				team.sendMessage(currentTeamName, myName, m.name, params.content, usage);
			}
			const stateWarning = recordOutboundState(params.content);
			const skippedNote = skipped.length > 0 ? ` Skipped ${skipped.length} exited: ${skipped.map((n) => `@${n}`).join(", ")}.` : "";
			return {
				content: [{
					type: "text",
					text: `Broadcast sent to ${recipients.length} member(s).${skippedNote}${stateWarning ? ` ${stateWarning}` : ""}`,
				}],
				details: {},
			};
		},
	});

	// ===================================================================
	// LEAD-ONLY TOOLS
	// ===================================================================

	const emitTeamSpawn = (
		teamName: string,
		name: string,
		model: string | null | undefined,
		task: string,
		cwd: string,
		spawnedAt: number,
		instanceId: string,
		sessionDir: string,
	) => {
		costTracker.addMember(name, cwd, spawnedAt, instanceId, sessionDir);
		pi.appendEntry(COST_MEMBER_CUSTOM_TYPE, { name, cwd, spawnedAt, instanceId, sessionDir });
		ensureCostRefreshTimer();
		refreshLeadStatus();
		pi.events.emit("team:spawn", {
			teamName,
			name,
			model: model ?? null,
			task,
			cwd,
			spawnedAt,
			instanceId,
			sessionDir,
		});
	};

	const spawnTeammate = (options: SpawnTeammateOptions, ctx: any) => spawnTeammateImpl(
		options,
		{
			paneManager,
			rpcTeammates,
      registerPaneEnvHandoff: paneEnvHandoffs.register,
      cleanupPaneEnvHandoff: paneEnvHandoffs.cleanup,
			ensureTeam,
			getAgentRoster,
			getCurrentTeamName: () => currentTeamName,
			updateTeammateStatus,
			requestOverlayRender: () => {
				if (overlayHandle) overlayHandle.requestRender();
			},
			emitTeamSpawn,
			emitTeamRemove: (teamName: string, name: string) => {
				pi.events.emit("team:remove", { teamName, name });
			},
		},
		ctx,
	);

	function registerSubagentTool() {
		registerAgentTeamsSubagentTool(pi, spawnTeammate);
	}

	if (!isTeammate) {
		pi.registerTool({
			name: "team_spawn",
			label: "Spawn Teammate",
			description:
				"Spawn a teammate Pi instance to work on a task in parallel." +
				" By default, do not set the model parameter; omit it so the teammate inherits the lead's current model unless you have a concrete reason to choose a different model." +
				" Use teammates when: executing a plan (delegate each step to a separate teammate to preserve your context);" +
				" parallel research (spawn multiple teammates to investigate different angles simultaneously);" +
				" background work (offload a task so you stay available for the user);" +
				" code review (have a teammate review changes while you continue other work)." +
				" Do NOT spawn a teammate for simple, quick, single-step tasks — just do those yourself." +
				" Each teammate gets its own context window and runs independently." +
				" The initial task is loaded into the teammate's system prompt so it survives context compaction; follow-up team_message messages remain mailbox messages." +
				" Give each teammate a clear, self-contained task with all necessary context (file paths, requirements, constraints)." +
				" Avoid assigning the same files to multiple teammates." +
				" After spawning, teammates report back via messages, which gives you a new turn to process their results." +
				" After spawning all needed teammates, let the user know who's working on what and ask if there's anything else you can help with in the meantime.",
			parameters: Type.Object({
				name: Type.String({ description: "Unique name for the teammate (e.g. 'researcher', 'reviewer')" }),
				task: Type.String({ description: "The task / initial prompt for the teammate" }),
				model: Type.Optional(
					Type.String({
						description:
							"Model for the teammate. Omit this by default so the teammate inherits the lead's current model. " +
							"Only set it when you have a concrete reason to choose a different model for this task. " +
							"Supports 'provider/id' format or just the model id. " +
							"If you do set it, use a current-generation model.",
					}),
				),
				cwd: Type.Optional(
					Type.String({
						description:
							"Working directory for the teammate. " +
							"Relative paths resolve from the lead's current working directory. " +
							"If omitted, the teammate uses the team's default cwd.",
					}),
				),
				team: Type.Optional(
					Type.String({
						description:
							"Team name. If provided, the team uses this name instead of an auto-generated one. " +
							"If a team with this name already exists, it is reused. " +
							"Useful for persistent teams across sessions.",
					}),
				),
			}),

			async execute(_id, params, _signal, _onUpdate, ctx) {
				const result = await spawnTeammate(
					{
						requestedName: params.name,
						task: params.task,
						teamName: params.team,
						model: params.model,
						cwd: params.cwd,
						spawnKind: "teammate",
					},
					ctx,
				);

				if (!result.ok) {
					return { content: [{ type: "text", text: result.text }], details: {}, isError: true };
				}

				const rpcNote = result.rpcMode
					? " (RPC mode). Use Ctrl+Shift+M to view output. For visible teammate panes, try running inside herdr, iTerm2, tmux, or WezTerm."
					: ".";
				return {
					content: [{ type: "text", text: `Spawned teammate @${result.name}${rpcNote}${result.identityNote}` }],
					details: { member: result.member },
				};
			},
			renderCall(args, theme) {
				const name = (args.name as string) ?? "?";
				const task = typeof args.task === "string" ? args.task : "…";
				const preview = task.length > 80 ? task.slice(0, 80) + "…" : task;
				const model = args.model as string | undefined;
				const modelPart = model ? " " + theme.fg("muted", `[${model}]`) : "";
				return new Text(
					theme.fg("toolTitle", theme.bold("team_spawn ")) +
						theme.fg("accent", `@${name}`) +
						modelPart +
						"\n  " +
						theme.fg("dim", preview),
					0,
					0,
				);
			},

			renderResult(result, _opts, theme) {
				const member = (result.details as any)?.member as MemberConfig | undefined;
				if (!member) {
					const t = result.content[0];
					return new Text(t?.type === "text" ? t.text : "", 0, 0);
				}
				const modelInfo = member.model ? theme.fg("muted", ` [${member.model}]`) : "";
				return new Text(
					theme.fg("success", "✓ ") +
						theme.fg("accent", `@${member.name}`) +
						modelInfo +
						theme.fg("muted", ` spawned in pane ${member.paneId}`),
					0,
					0,
				);
			},
		});

		pi.registerTool({
			name: "team_diagnose",
			label: "Diagnose Teammate",
			description:
				"Capture the last N lines of a teammate's terminal output to troubleshoot only when there is a concrete reason to suspect something is broken. " +
				"Not for monitoring or status polling; a teammate simply not responding yet is not enough reason to suspect it is broken.",
			parameters: Type.Object({
				name: Type.String({ description: "Teammate name" }),
				lines: Type.Optional(
					Type.Number({ description: "Number of lines to capture (default: 20)", default: 20 }),
				),
			}),

			async execute(_id, params, _signal, _onUpdate, _ctx) {
				if (!currentTeamName) {
					return { content: [{ type: "text", text: "No active team." }], details: {}, isError: true };
				}
				const name = params.name.replace(/^@/, "");

				// Check RPC teammates first
				const rpc = rpcTeammates.get(name);
				if (rpc) {
					const lines = params.lines ?? 20;
					const output = rpc.outputLines;
					const content = output.slice(-lines).join("\n");
					return {
						content: [{ type: "text", text: content || "(no output yet)" }],
						details: { name, lines },
					};
				}

				if (!paneManager) {
					return {
						content: [{ type: "text", text: "Not running inside a terminal multiplexer and no RPC teammate found." }],
						details: {},
						isError: true,
					};
				}
				const config = team.readConfig(currentTeamName);
				const member = config?.members.find((m) => m.name === name);
				if (!member) {
					return {
						content: [{ type: "text", text: `Teammate @${name} not found.` }],
						details: {},
						isError: true,
					};
				}
				if (member.transport === "vscode") {
					return {
						content: [{ type: "text", text: `@${name} is a VS Code teammate — no terminal pane to capture. Use team_message instead.` }],
						details: {},
						isError: true,
					};
				}
				if (!isMemberAlive(member)) {
					return {
						content: [{ type: "text", text: `@${name} is not alive (pane dead).` }],
						details: {},
						isError: true,
					};
				}
				const lines = params.lines ?? 20;
				const content = paneManager.capturePaneContent(member.paneId, lines);
				if (content === null) {
					return {
						content: [{ type: "text", text: `Could not capture pane content for @${name}.` }],
						details: {},
						isError: true,
					};
				}
				return {
					content: [{ type: "text", text: content }],
					details: { name, lines },
				};
			},
		});

		pi.registerTool({
			name: "team_request_shutdown",
			label: "Request Shutdown",
			description:
				"Ask a teammate to finish its current work and shut down gracefully. " +
				"Use when the user explicitly asks, when the teammate reports it is blocked and shutdown is appropriate, or when canceling remaining work after the user changes direction. " +
				"Do not use this merely because a teammate is still running.",
			parameters: Type.Object({
				name: Type.String({ description: "Teammate name" }),
			}),

			async execute(_id, params) {
				if (!currentTeamName) {
					return { content: [{ type: "text", text: "No active team." }], details: {}, isError: true };
				}
				const to = params.name.replace(/^@/, "");
				team.sendMessage(
					currentTeamName,
					"team_lead",
					to,
					"[SHUTDOWN_REQUEST] Please finish your current work and shut down.",
				);
				return { content: [{ type: "text", text: `Shutdown request sent to @${to}.` }], details: {} };
			},
		});

		pi.registerTool({
			name: "team_force_shutdown",
			label: "Force Shutdown",
			description:
				"Request immediate teammate termination and retain its stopping registration until exit is observed. " +
				"Use only when the user explicitly asks, or after team_status shows the teammate is no longer alive. " +
				"Do not use this because a teammate is taking longer than expected.",
			parameters: Type.Object({
				name: Type.String({ description: "Teammate name" }),
			}),

			async execute(_id, params, _signal, _onUpdate, ctx) {
				if (!currentTeamName) {
					return { content: [{ type: "text", text: "No active team." }], details: {}, isError: true };
				}
				const name = params.name.replace(/^@/, "");
				const config = team.readConfig(currentTeamName);
				const member = config?.members.find((m) => m.name === name);
				if (!member) {
					return {
						content: [{ type: "text", text: `Teammate @${name} not found.` }],
						details: {},
						isError: true,
					};
				}
				if (!member.instanceId) {
					if (member.role !== "teammate" || !isConfirmedDeadLegacyProcessMember(member)) {
						return {
							content: [{ type: "text", text: `Teammate @${name} has no instance identity; refusing unsafe removal.` }],
							details: {},
							isError: true,
						};
					}
					team.removeMember(currentTeamName, name);
					team.removeMemberState(currentTeamName, name);
					pi.events.emit("team:remove", { teamName: currentTeamName, name });
					updateTeammateStatus();
					return {
						content: [{ type: "text", text: `Removed confirmed-dead legacy registration for @${name}.` }],
						details: {},
					};
				}
				const rpc = rpcTeammates.get(name);
				if (!rpc && !paneManager) {
					return {
						content: [{ type: "text", text: `Cannot force shutdown @${name}: no local RPC or pane termination handle; registration retained unchanged.` }],
						details: {},
						isError: true,
					};
				}
        paneEnvHandoffs.cleanup(member.instanceId);
				team.markMemberStopping(currentTeamName, name, member.instanceId);
				let observedExit = false;
				if (rpc) {
					rpc.kill();
					// Map/config/state removal and the sole team:remove event are deferred
					// to this exact RPC instance's observed exit callback.
				} else if (paneManager) {
					const paneWasAlive = paneManager.isPaneAlive(member.paneId);
					if (paneWasAlive) paneManager.killPane(member.paneId);
					if (!paneWasAlive || !paneManager.isPaneAlive(member.paneId)) {
						observedExit = true;
						team.removeMember(currentTeamName, name);
						team.removeMemberState(currentTeamName, name);
						pi.events.emit("team:remove", { teamName: currentTeamName, name });
					}
				}
				updateTeammateStatus();
				const text = observedExit
					? `Force-killed @${name}.`
					: `Shutdown initiated for @${name}; registration retained until observed exit.`;
				return { content: [{ type: "text", text }], details: {} };
			},
		});

		pi.registerTool({
			name: "team_cleanup",
			label: "Cleanup Team",
			description:
				"Tear down the entire team: kill all teammate panes and remove team files. The lead session continues.",
			parameters: Type.Object({}),

			async execute(_id, _params, _signal, _onUpdate, ctx) {
				if (!currentTeamName) {
					return { content: [{ type: "text", text: "No active team to clean up." }], details: {} };
				}
        paneEnvHandoffs.cleanupAll();
				const config = team.readConfig(currentTeamName);
				if (config) {
					for (const m of config.members.filter((m) => m.role === "teammate")) {
						const rpc = rpcTeammates.get(m.name);
						if (rpc) {
							rpc.kill();
							// team:remove emitted by the onExit callback — no duplicate emit here
						} else if (
							paneManager &&
							m.paneId !== config.leadPaneId &&
							paneManager.isPaneAlive(m.paneId)
						) {
							// Never pass a stale or lead pane handle to a multiplexer. Some
							// backends fall back to the active pane when a target is invalid.
							paneManager.killPane(m.paneId);
							// No onExit callback for pane-manager — emit directly
							pi.events.emit("team:remove", { teamName: currentTeamName, name: m.name });
						}
					}
				}
				rpcTeammates.clear();
				team.cleanupTeam(currentTeamName);
				stopWatching();
				currentTeamName = null;
				teamInitialized = false;
				if (!stashedUI) stashedUI = ctx.ui;
				refreshLeadStatus();
				return { content: [{ type: "text", text: "Team cleaned up." }], details: {} };
			},
		});
	}

	// ===================================================================
	// TEAMMATE-ONLY TOOLS
	// ===================================================================

	if (isTeammate) {
		pi.registerTool({
			name: "team_shutdown",
			label: "Shutdown",
			description:
				"Gracefully shut down this teammate session. Optionally send a complete final report to the team lead first.",
			parameters: Type.Object({
				summary: Type.Optional(
					Type.String({ description: "Complete final report to send to @team_lead before shutting down" }),
				),
			}),

			async execute(_id, params, _signal, _onUpdate, ctx) {
				let stateWarning: string | null = null;
				if (params.summary && currentTeamName) {
					team.sendMessage(currentTeamName, myName, "team_lead", `[COMPLETED] ${params.summary}`, safeContextUsage(ctx));
					stateWarning = recordMemberState("completed-awaiting-shutdown");
				} else if (currentTeamName) {
					stateWarning = recordMemberState("completed-awaiting-shutdown");
				}
				if (paneManager?.kind === "zellij" && currentTeamName) {
					try {
						const member = team.readConfig(currentTeamName)?.members.find((candidate) => candidate.name === myName);
						if (member && isPaneBackedByCurrentManager(member, paneManager)) {
							const currentPaneId = paneManager.getCurrentPaneId();
							if (member.paneId === currentPaneId) {
								// Zellij holds exited panes. The detached helper waits for this Pi
								// process to exit — after every session_shutdown handler finishes —
								// before closing the configured teammate pane.
								paneManager.closePaneAfterProcessExit?.(currentPaneId);
							}
						}
					} catch {
						// Config/pane discovery/helper failure must not block graceful shutdown.
					}
				}
				ctx.shutdown();
				// In RPC mode, ctx.shutdown() ends the session but doesn't terminate
				// the process — it stays alive waiting for the next JSONL command.
				// Exit explicitly after a short delay to allow cleanup to flush.
				// In terminal mode this is harmless — ctx.shutdown() exits first.
				setTimeout(() => process.exit(0), 1000);
				return {
					content: [{ type: "text", text: `Shutting down…${stateWarning ? ` ${stateWarning}` : ""}` }],
					details: {},
				};
			},
		});
	}

	// ===================================================================
	// /team command – quick interactive status check (both roles)
	// ===================================================================

	pi.registerCommand("team", {
		description: "Show agent team status",
		handler: async (_args, ctx) => {
			if (!currentTeamName) {
				ctx.ui.notify("No active team.", "info");
				return;
			}
			const config = team.readConfig(currentTeamName);
			if (!config) {
				ctx.ui.notify("Team config not found.", "error");
				return;
			}
			const lines = [`Team: ${config.name}`, `Role: ${isTeammate ? `@${myName} (teammate)` : "@team_lead"}`];
			for (const m of config.members) {
				const alive = isMemberAlive(m);
				const model = m.model ? ` [${m.model}]` : "";
				lines.push(`  ${alive ? "✓" : "✗"} @${m.name} (${m.role})${model}${m.task ? ` — ${m.task}` : ""}`);
			}
			ctx.ui.notify(lines.join("\n"), "info");
		},
	});
}
