/**
 * Mailbox directory watcher — extracted from index.ts for testability.
 *
 * Uses fs.watch() for near-instant message delivery with a debounce
 * to coalesce duplicate macOS events, plus a slow fallback poll as a
 * safety net for any events the OS watcher might drop.
 */

import * as fs from "node:fs";

export interface MailboxWatcherOptions {
	/** Directory to watch for new .json message files. */
	dir: string;
	/** Called when new messages may be available. */
	onMessages: () => void;
	/** Debounce delay in ms (default: 50). */
	debounceMs?: number;
	/** Fallback poll interval in ms (default: 30_000). */
	fallbackMs?: number;
	/** @internal Test-only: override fs.watch for deterministic error testing. */
	_watchFn?: typeof fs.watch;
}

export interface MailboxWatcher {
	/** Start watching the mailbox directory. */
	start(): void;
	/** Stop watching and clean up all timers. Idempotent. */
	stop(): void;
}

export function createMailboxWatcher(opts: MailboxWatcherOptions): MailboxWatcher {
	const debounceMs = opts.debounceMs ?? 50;
	const fallbackMs = opts.fallbackMs ?? 30_000;

	let fsWatcher: fs.FSWatcher | null = null;
	let fallbackInterval: ReturnType<typeof setInterval> | null = null;
	let debounceTimer: ReturnType<typeof setTimeout> | null = null;
	let stopped = false;

	function scheduleDrain(): void {
		if (debounceTimer) clearTimeout(debounceTimer);
		debounceTimer = setTimeout(() => {
			debounceTimer = null;
			try {
				opts.onMessages();
			} catch {
				/* drain errors should not propagate into the timer context */
			}
		}, debounceMs);
	}

	/** Try to attach fs.watch(). Returns true on success, false on failure. */
	function attachFsWatcher(): boolean {
		try {
			fsWatcher = (opts._watchFn ?? fs.watch)(opts.dir, (_eventType, filename) => {
				if (filename && filename.endsWith(".json")) {
					scheduleDrain();
				}
			});
			fsWatcher.on("error", () => {
				try {
					fsWatcher?.close();
				} catch {
					/* ignore */
				}
				fsWatcher = null;
			});
			return true;
		} catch {
			fsWatcher = null;
			return false;
		}
	}

	function start(): void {
		if (fsWatcher) return; // already watching
		stopped = false;

		// Ensure the directory exists before attaching the watcher.
		fs.mkdirSync(opts.dir, { recursive: true });

		attachFsWatcher();

		// Safety-net: slow fallback poll catches anything the OS watcher misses.
		// Also attempts to re-attach fs.watch if it errored out.
		if (!fallbackInterval) {
			fallbackInterval = setInterval(() => {
				if (!fsWatcher && !stopped) {
					attachFsWatcher();
				}
				try {
					opts.onMessages();
				} catch {
					/* drain errors should not kill the interval */
				}
			}, fallbackMs);
		}
	}

	function stop(): void {
		stopped = true;
		if (debounceTimer) {
			clearTimeout(debounceTimer);
			debounceTimer = null;
		}
		if (fsWatcher) {
			try {
				fsWatcher.close();
			} catch {
				/* ignore */
			}
			fsWatcher = null;
		}
		if (fallbackInterval) {
			clearInterval(fallbackInterval);
			fallbackInterval = null;
		}
	}

	return { start, stop };
}
