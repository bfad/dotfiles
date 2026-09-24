/**
 * Per-sender colour assignment for team messages.
 *
 * Each teammate name is hashed to a fixed palette entry so the same name
 * always gets the same colour, and different names usually differ. The
 * palette is the Okabe–Ito qualitative set, which stays distinguishable
 * under red/green colour-vision deficiency.
 */

export interface SenderColor {
	/** Stable label, handy for tests and debugging. */
	readonly name: string;
	/** Hex (for SVG / web previews). */
	readonly hex: string;
	/** 24-bit RGB (for terminal SGR codes). */
	readonly rgb: readonly [number, number, number];
}

// Okabe–Ito qualitative palette (colour-blind safe). Black is omitted so a
// sender never collapses into ordinary text.
export const SENDER_PALETTE: readonly SenderColor[] = [
	{ name: "orange", hex: "#E69F00", rgb: [230, 159, 0] },
	{ name: "sky", hex: "#56B4E9", rgb: [86, 180, 233] },
	{ name: "green", hex: "#009E73", rgb: [0, 158, 115] },
	{ name: "vermillion", hex: "#D55E00", rgb: [213, 94, 0] },
	{ name: "purple", hex: "#CC79A7", rgb: [204, 121, 167] },
	{ name: "blue", hex: "#3B9EE5", rgb: [59, 158, 229] },
];

/** Deterministic FNV-1a hash → palette index. */
export function senderColorIndex(name: string): number {
	let h = 0x811c9dc5;
	for (let i = 0; i < name.length; i++) {
		h ^= name.charCodeAt(i);
		h = Math.imul(h, 0x01000193);
	}
	return (h >>> 0) % SENDER_PALETTE.length;
}

export function senderColor(name: string): SenderColor {
	return SENDER_PALETTE[senderColorIndex(name)]!;
}

/** Foreground SGR prefix for a sender colour, e.g. "\x1b[38;2;230;159;0m". */
export function senderFgCode(color: SenderColor): string {
	const [r, g, b] = color.rgb;
	return `\x1b[38;2;${r};${g};${b}m`;
}

/** Wrap text in a sender's foreground colour, resetting only the colour after. */
export function colorizeFg(text: string, color: SenderColor): string {
	return `${senderFgCode(color)}${text}\x1b[39m`;
}

// Dark base we blend toward for the message band. Tuned for dark themes;
// the band is opt-out via env (see index.ts) for light themes.
const BAND_BASE: readonly [number, number, number] = [30, 30, 46];
const BAND_MIX = 0.22;

/** A muted, dark tint of the sender colour for use as a background band. */
export function senderBandRgb(color: SenderColor): [number, number, number] {
	const [r, g, b] = color.rgb;
	const mix = (channel: number, base: number) => Math.round(base + (channel - base) * BAND_MIX);
	return [mix(r, BAND_BASE[0]), mix(g, BAND_BASE[1]), mix(b, BAND_BASE[2])];
}

/** Background SGR prefix for a sender's band, e.g. "\x1b[48;2;36;58;86m". */
export function senderBandCode(color: SenderColor): string {
	const [r, g, b] = senderBandRgb(color);
	return `\x1b[48;2;${r};${g};${b}m`;
}

/** Bright background SGR prefix for the solid rail, e.g. "\x1b[48;2;230;159;0m". */
export function senderRailBgCode(color: SenderColor): string {
	const [r, g, b] = color.rgb;
	return `\x1b[48;2;${r};${g};${b}m`;
}
