/**
 * Shared types for the agent-teams extension.
 */

export interface VscodeWorkspace {
	path: string; // absolute workspace path
	branch?: string; // git branch name (if set, must match for connection)
}

export interface TeamConfig {
	name: string;
	leadPaneId: string;
	cwd: string;
	members: MemberConfig[];
	vscodeWorkspaces?: VscodeWorkspace[];
	createdAt: number;
}

export interface MemberConfig {
	name: string;
	role: "lead" | "teammate";
	paneId: string;
	transport?: "tmux" | "zellij" | "wezterm" | "shuttle" | "iterm" | "ghostty" | "cmux" | "herdr" | "vscode" | "rpc";
	pid?: number;
	task?: string;
	model?: string;
	spawnKind?: "teammate" | "subagent";
	agent?: string;
	cwd?: string;
	spawnedAt: number;
	instanceId?: string;
	sessionDir?: string;
	state?: "starting" | "running" | "stopping";
}

/**
 * A snapshot of the sender's context-window usage at the moment a message was
 * sent. Structurally matches the coding agent's `ContextUsage` (returned by
 * `ctx.getContextUsage()`) so values can be passed through without conversion.
 */
export interface ContextUsage {
	/** Estimated context tokens in use, or null if unknown. */
	tokens: number | null;
	/** Total context window size for the sender's model. */
	contextWindow: number;
	/** Usage as a percentage of the context window, or null if unknown. */
	percent: number | null;
}

export interface MessageOrder {
	v: 1;
	logicalMs: number;
	writerId: string;
	seq: number;
	nonce: string;
}

export interface TeamMessage {
	id: string;
	/** Optional for source compatibility; v1 mailbox batch reads require it. */
	order?: MessageOrder;
	from: string;
	to: string;
	content: string;
	timestamp: number;
	/** The sender's context usage when the message was sent, if reported. */
	contextUsage?: ContextUsage;
}
