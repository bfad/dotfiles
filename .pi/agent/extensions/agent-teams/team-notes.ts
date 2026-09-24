/**
 * System notes appended to inbound team messages.
 *
 * These lines are steering instructions for the *reading* agent (e.g. "other
 * teammates are still working…"). They are appended to the message body that
 * is delivered to the model and MUST stay there — the agent needs them.
 *
 * For presentation only, the renderer hides a trailing note when the message
 * is collapsed and shows it when expanded. `stripTrailingNotes` does that in a
 * generic way: it removes a known trailing note (and surrounding blank space)
 * from a copy of the text used purely for display.
 */

export const TEAMMATES_WORKING_COMPLETION_NOTE =
	"Note: other teammates are still working. Integrate this result now — you will hear from them when they finish.";

export const TEAMMATES_WORKING_REMINDER =
	"Reminder: other teammates are still working. Unless the message warrants action, just acknowledge it to the user.";

/** Every note that may be appended to a team message body. */
export const APPENDED_TEAM_NOTES: readonly string[] = [
	TEAMMATES_WORKING_COMPLETION_NOTE,
	TEAMMATES_WORKING_REMINDER,
];

/**
 * Return `content` with a trailing appended note removed (along with the blank
 * space before it). Used for the collapsed display only — the model still
 * receives the full body. If no known note is at the end, the text is returned
 * unchanged.
 */
export function stripTrailingNotes(content: string, notes: readonly string[] = APPENDED_TEAM_NOTES): string {
	let result = content;
	for (const note of notes) {
		if (!note) continue;
		const trimmed = result.replace(/\s+$/, "");
		if (trimmed.endsWith(note)) {
			result = trimmed.slice(0, trimmed.length - note.length).replace(/\s+$/, "");
		}
	}
	return result;
}
