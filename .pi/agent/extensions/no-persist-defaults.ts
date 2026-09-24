/**
 * no-persist-defaults — make model/thinking changes session-only.
 *
 * Pi hard-codes persisting every in-session model change (/model, Ctrl+P)
 * and thinking-level change back to ~/.pi/agent/settings.json as the new
 * global default (see https://github.com/earendil-works/pi/issues/5263).
 *
 * This no-ops the SettingsManager methods responsible, so defaults change
 * only when you edit settings.json yourself. Works because extensions share
 * the live module graph with pi core, so patching the prototype affects the
 * real instances.
 *
 * Delete this file to restore stock behavior. Revisit when pi#5263 lands.
 */
import { SettingsManager, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

const PATCH = [
	"setDefaultModel",
	"setDefaultProvider",
	"setDefaultModelAndProvider",
	"setDefaultThinkingLevel",
	// "setHideThinkingBlock", // uncomment to also stop the thinking-visibility toggle from persisting
] as const;

const proto = SettingsManager.prototype as unknown as Record<string, unknown>;
const missing: string[] = [];
for (const name of PATCH) {
	if (typeof proto[name] === "function") {
		proto[name] = () => {};
	} else {
		missing.push(name);
	}
}

export default function (pi: ExtensionAPI) {
	// Guard against silent breakage on pi upgrades: if internals were renamed,
	// the patch no longer applies and defaults would start persisting again.
	if (missing.length > 0) {
		pi.on("session_start", async (_event, ctx) => {
			if (ctx.hasUI) {
				ctx.ui.notify(
					`no-persist-defaults: pi internals changed (missing: ${missing.join(", ")}) — model/thinking changes may persist to settings.json again`,
					"warning",
				);
			}
		});
	}
}
