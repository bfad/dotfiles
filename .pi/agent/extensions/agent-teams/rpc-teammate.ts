/**
 * RPC Teammate — manages a teammate pi instance as a headless subprocess.
 *
 * Used when the lead is NOT running inside tmux/zellij.  Each teammate
 * is launched with a private `pi --mode rpc --session-dir` and communicates via JSONL
 * over stdin/stdout.  The file-based mailbox still handles team messaging
 * (the teammate's agent-teams extension polls it as usual); this module
 * captures the teammate's streaming output so the lead can display it
 * in an overlay.
 */

import { type ChildProcess, spawn } from "node:child_process";
import { execSync } from "node:child_process";
import { buildRpcChildPiEnv } from "./child-pi-env.js";
import { buildRpcCliArgs } from "./rpc-args.js";

// ============================================================================
// Types
// ============================================================================

export type TeammateStatus = "starting" | "running" | "idle" | "error" | "dead";

export interface RpcTeammateOptions {
	/** Team name (set as PI_TEAM_NAME). */
	teamName: string;
	/** Teammate's unique name (set as PI_TEAM_AGENT_NAME). */
	name: string;
	/** Working directory for the subprocess. */
	cwd: string;
	/** Unique per-spawn identity used for this child's private session lease. */
	instanceId: string;
	/** Exact private directory where Pi persists this child's session. */
	sessionDir: string;
	/** Optional canonical model value (provider/id, passed via --model). */
	model?: string;
	/** Optional comma-separated tool allowlist (passed via --tools). */
	tools?: string;
	/** Optional paths to files whose contents are appended to the system prompt, in order. */
	systemPromptPaths?: string[];
	/** Additional environment variables to pass to the teammate process. */
	extraEnv?: Record<string, string>;
	/** Pre-parsed, isolation-safe extra Pi CLI arguments. */
	extraArgs?: string[];
	/** Max lines to keep in the output buffer. */
	maxOutputLines?: number;
}

interface RpcEvent {
	type: string;
	[key: string]: unknown;
}

// ============================================================================
// RpcTeammate
// ============================================================================

export class RpcTeammate {
	readonly name: string;
	private proc: ChildProcess | null = null;
	private _status: TeammateStatus = "starting";
	private _outputLines: string[] = [];
	private _currentLine = "";
	private readonly maxLines: number;
	private readonly options: RpcTeammateOptions;
	private requestId = 0;
	private lineBuffer = "";
	private onStatusChange?: () => void;
	private onExit?: () => void;

	constructor(options: RpcTeammateOptions, onStatusChange?: () => void, onExit?: () => void) {
		this.name = options.name;
		this.options = options;
		this.maxLines = options.maxOutputLines ?? 500;
		this.onStatusChange = onStatusChange;
		this.onExit = onExit;
	}

	get status(): TeammateStatus {
		return this._status;
	}

	get outputLines(): readonly string[] {
		// Include any partial current line
		if (this._currentLine) {
			return [...this._outputLines, this._currentLine];
		}
		return this._outputLines;
	}

	get pid(): number | undefined {
		return this.proc?.pid;
	}

	get isAlive(): boolean {
		return this.proc !== null && this.proc.exitCode === null;
	}

	// -----------------------------------------------------------------------
	// Lifecycle
	// -----------------------------------------------------------------------

	/** Spawn the RPC subprocess. */
	start(): void {
		if (this.proc) throw new Error(`Teammate @${this.name} already started`);

		// Determine the pi executable
		let piExe = "pi";
		try {
			execSync("command -v devx", { stdio: "ignore" });
			piExe = "devx";
		} catch {
			/* devx not available */
		}

		const args = buildRpcCliArgs(piExe, {
			sessionDir: this.options.sessionDir,
			model: this.options.model,
			tools: this.options.tools,
			systemPromptPaths: this.options.systemPromptPaths,
			extraArgs: this.options.extraArgs,
		});

		const env = buildRpcChildPiEnv({
			baseEnv: process.env,
			extraEnv: this.options.extraEnv,
			teamName: this.options.teamName,
			agentName: this.options.name,
			instanceId: this.options.instanceId,
			sessionDir: this.options.sessionDir,
		});

		this.proc = spawn(piExe, args, {
			cwd: this.options.cwd,
			env,
			stdio: ["pipe", "pipe", "pipe"],
		});

		this.appendOutput(`[Starting @${this.name}...]`);

		// Read stdout (JSONL events)
		this.proc.stdout?.on("data", (chunk: Buffer) => {
			this.handleStdoutChunk(chunk.toString("utf-8"));
		});

		// Read stderr for debugging
		this.proc.stderr?.on("data", (chunk: Buffer) => {
			const text = chunk.toString("utf-8").trim();
			if (text) {
				for (const line of text.split("\n")) {
					this.appendOutput(`[stderr] ${line}`);
				}
			}
		});

		this.proc.on("exit", (code) => {
			this.appendOutput(`[Process exited with code ${code ?? "unknown"}]`);
			this.setStatus("dead");
			this.proc = null;
			this.onExit?.();
		});

		this.proc.on("error", (err) => {
			this.appendOutput(`[Process error: ${err.message}]`);
			this.setStatus("error");
		});

		this.setStatus("starting");
	}

	/** Send a prompt to the teammate via RPC. */
	sendPrompt(message: string): void {
		this.sendCommand({ type: "prompt", message });
	}

	/** Abort the teammate's current operation. */
	abort(): void {
		this.sendCommand({ type: "abort" });
	}

	/** Kill the subprocess. */
	kill(): void {
		if (this.proc) {
			try {
				this.proc.kill("SIGTERM");
			} catch {
				/* already dead */
			}
			// Force-kill after 2s
			const p = this.proc;
			setTimeout(() => {
				try {
					p?.kill("SIGKILL");
				} catch {
					/* already dead */
				}
			}, 2000);
			this.proc = null;
			this.setStatus("dead");
		}
	}

	// -----------------------------------------------------------------------
	// Internal
	// -----------------------------------------------------------------------

	private sendCommand(cmd: Record<string, unknown>): void {
		if (!this.proc?.stdin?.writable) return;
		const id = `req_${++this.requestId}`;
		const line = JSON.stringify({ ...cmd, id }) + "\n";
		try {
			this.proc.stdin.write(line);
		} catch {
			/* stdin might be closed */
		}
	}

	private handleStdoutChunk(chunk: string): void {
		this.lineBuffer += chunk;
		let nlIdx: number;
		while ((nlIdx = this.lineBuffer.indexOf("\n")) !== -1) {
			let line = this.lineBuffer.slice(0, nlIdx);
			this.lineBuffer = this.lineBuffer.slice(nlIdx + 1);
			if (line.endsWith("\r")) line = line.slice(0, -1);
			this.handleJsonLine(line);
		}
	}

	private handleJsonLine(line: string): void {
		let event: RpcEvent;
		try {
			event = JSON.parse(line);
		} catch {
			return; // ignore non-JSON
		}

		// Route based on event type
		switch (event.type) {
			case "agent_start":
				this.setStatus("running");
				break;

			case "agent_end":
				this.setStatus("idle");
				break;

			case "message_update": {
				const delta = event.assistantMessageEvent as Record<string, unknown> | undefined;
				if (delta?.type === "text_delta" && typeof delta.delta === "string") {
					this.appendTextDelta(delta.delta);
				}
				break;
			}

			case "tool_execution_start": {
				const toolName = event.toolName as string;
				const args = event.args as Record<string, unknown> | undefined;
				let summary = `[tool: ${toolName}`;
				if (toolName === "bash" && args?.command) {
					const cmd = String(args.command);
					summary += ` → ${cmd.length > 80 ? cmd.slice(0, 80) + "…" : cmd}`;
				} else if (toolName === "read" && args?.path) {
					summary += ` → ${args.path}`;
				} else if (toolName === "edit" && args?.path) {
					summary += ` → ${args.path}`;
				} else if (toolName === "write" && args?.path) {
					summary += ` → ${args.path}`;
				}
				summary += "]";
				this.appendOutput(summary);
				break;
			}

			case "tool_execution_end": {
				const isError = event.isError as boolean;
				if (isError) {
					this.appendOutput(`[tool error]`);
				}
				break;
			}

			case "response": {
				// RPC command responses — mostly ignore, but log errors
				const success = event.success as boolean;
				if (!success && event.error) {
					this.appendOutput(`[RPC error: ${event.error}]`);
				}
				break;
			}
		}
	}

	private appendTextDelta(delta: string): void {
		// Process character by character to handle newlines in the delta
		for (const ch of delta) {
			if (ch === "\n") {
				this._outputLines.push(this._currentLine);
				this._currentLine = "";
				this.trimOutput();
			} else {
				this._currentLine += ch;
			}
		}
		this.onStatusChange?.();
	}

	private appendOutput(line: string): void {
		// Flush any partial current line first
		if (this._currentLine) {
			this._outputLines.push(this._currentLine);
			this._currentLine = "";
		}
		this._outputLines.push(line);
		this.trimOutput();
		this.onStatusChange?.();
	}

	private trimOutput(): void {
		if (this._outputLines.length > this.maxLines) {
			this._outputLines = this._outputLines.slice(-this.maxLines);
		}
	}

	private setStatus(status: TeammateStatus): void {
		if (this._status !== status) {
			this._status = status;
			this.onStatusChange?.();
		}
	}
}
