/**
 * Extra CLI args to append when spawning teammates.
 *
 * Set `PI_TEAM_EXTRA_ARGS` to pass additional flags to every teammate the lead
 * spawns. When unset, spawning is unchanged. When set, the value is appended to
 * the end of the spawn command (after the model/tools/system-prompt flags).
 *
 * The motivating case is development: a lead launched with
 * `pi -ne -e extensions/agent-teams/index.ts` runs the working-copy extension,
 * but its teammates spawn a plain `pi` and load the *installed* extension. Set
 * `PI_TEAM_EXTRA_ARGS="-ne -e extensions/agent-teams/index.ts"` to make
 * teammates load the same working copy. (Relative paths resolve from the
 * teammate's working directory.)
 *
 * The value is a shell-style argument string. `parseExtraArgs` tokenizes it on
 * unquoted whitespace, honouring single and double quotes so args with spaces
 * survive (e.g. `-e "my dir/index.ts"`). RPC receives that argv array directly;
 * pane transports shell-escape each parsed argument before building a command.
 * Session-control flags are rejected so extra args cannot disable isolation.
 */

/** The environment variable holding extra teammate spawn args. */
export const EXTRA_ARGS_ENV = "PI_TEAM_EXTRA_ARGS";

const SESSION_CONTROL_FLAGS = [
  "--no-session",
  "--session-dir",
  "--session",
  "--session-id",
  "--resume",
  "--continue",
  "--fork",
] as const;

/**
 * Tokenize a shell-style argument string into an argv array.
 *
 * Splits on unquoted whitespace; single and double quotes group a run of
 * characters (including spaces) into one token. Returns `[]` for an unset,
 * empty, or whitespace-only value. Backslash escapes are not interpreted —
 * use quotes for args containing spaces.
 */
export function parseExtraArgs(value: string | undefined): string[] {
	if (!value) return [];
	const tokens: string[] = [];
	let current = "";
	let started = false;
	let quote: '"' | "'" | null = null;
	for (const ch of value) {
		if (quote) {
			if (ch === quote) quote = null;
			else current += ch;
		} else if (ch === '"' || ch === "'") {
			quote = ch;
			started = true;
		} else if (/\s/.test(ch)) {
			if (started) {
				tokens.push(current);
				current = "";
				started = false;
			}
		} else {
			current += ch;
			started = true;
		}
	}
	if (started) tokens.push(current);
	for (const token of tokens) {
		const isLongSessionControl = SESSION_CONTROL_FLAGS.some(
			(flag) => token === flag || token.startsWith(`${flag}=`),
		);
		if (isLongSessionControl || token === "-r" || token === "-c") {
			throw new Error(`${EXTRA_ARGS_ENV} cannot override Agent Teams session isolation with ${token}`);
		}
	}
	return tokens;
}
