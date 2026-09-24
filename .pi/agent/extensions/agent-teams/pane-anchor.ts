import type { PaneManager } from "./pane-manager.js";
import type { MemberConfig } from "./types.js";

export function isPaneBackedByCurrentManager(member: MemberConfig, paneManager: PaneManager): boolean {
	if (member.paneId === "none" || member.transport === "vscode" || member.transport === "rpc") return false;
	return !member.transport || member.transport === paneManager.kind;
}

/**
 * Pick the newest live teammate pane that is safe to split from.
 *
 * Liveness alone is not enough: a stale team config can point at a pane that is
 * alive in another tmux window/session. Pane location is only an anchor-safety
 * signal; it must not be used to decide whether a teammate still belongs to the
 * persisted team.
 */
export function selectTeammateAnchor(
	teammates: MemberConfig[],
	leadPaneId: string,
	paneManager: PaneManager,
): MemberConfig | null {
	const leadLocation = paneManager.getPaneLocation?.(leadPaneId) ?? null;
	const canValidateLocation = !paneManager.getPaneLocation || leadLocation !== null;

	for (const member of [...teammates].reverse()) {
		if (!isPaneBackedByCurrentManager(member, paneManager)) continue;
		if (!paneManager.isPaneAlive(member.paneId)) continue;
		if (!canValidateLocation) continue;

		if (leadLocation && paneManager.getPaneLocation) {
			const memberLocation = paneManager.getPaneLocation(member.paneId);
			if (
				!memberLocation ||
				memberLocation.sessionId !== leadLocation.sessionId ||
				memberLocation.windowId !== leadLocation.windowId
			) {
				continue;
			}
		}

		return member;
	}

	return null;
}
