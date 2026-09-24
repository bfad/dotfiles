import type { Message } from "@mariozechner/pi-ai";
import type { DisplayItem, SingleResult, UsageStats } from "./types.js";

type UsageForDisplay = Omit<UsageStats, "contextTokens" | "turns"> & {
  contextTokens?: number;
  turns?: number;
};

export function formatTokens(count: number): string {
  if (count < 1000) return count.toString();
  if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
  if (count < 1000000) return `${Math.round(count / 1000)}k`;
  return `${(count / 1000000).toFixed(1)}M`;
}

export function formatUsageStats(usage: UsageForDisplay): string {
  const parts: string[] = [];
  if (usage.turns) parts.push(`${usage.turns} turn${usage.turns > 1 ? "s" : ""}`);
  if (usage.input) parts.push(`↑${formatTokens(usage.input)}`);
  if (usage.output) parts.push(`↓${formatTokens(usage.output)}`);
  if (usage.cacheRead) parts.push(`R${formatTokens(usage.cacheRead)}`);
  if (usage.cacheWrite) parts.push(`W${formatTokens(usage.cacheWrite)}`);
  if (usage.cost) parts.push(`$${usage.cost.toFixed(4)}`);
  if (usage.contextTokens && usage.contextTokens > 0) {
    parts.push(`ctx:${formatTokens(usage.contextTokens)}`);
  }
  return parts.join(" ");
}

export function getFinalOutput(messages: Message[]): string {
  for (let i = messages.length - 1; i >= 0; i--) {
    const msg = messages[i];
    if (msg.role === "assistant") {
      const content = Array.isArray(msg.content) ? msg.content : [];
      const textParts = content
        .filter((part) => part?.type === "text")
        .map((part) => part.text);
      if (textParts.length > 0) return textParts.join("\n");
    }
  }
  return "";
}

export function getDisplayItems(messages: Message[]): DisplayItem[] {
  const items: DisplayItem[] = [];
  for (const msg of messages) {
    if (msg.role === "assistant") {
      const content = Array.isArray(msg.content) ? msg.content : [];
      for (const part of content) {
        if (part?.type === "text") items.push({ type: "text", text: part.text });
        else if (part?.type === "toolCall") items.push({ type: "toolCall", name: part.name, args: part.arguments });
      }
    }
  }
  return items;
}

export function aggregateUsage(results: SingleResult[]): Omit<UsageStats, "contextTokens"> {
  const total = { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: 0, turns: 0 };
  for (const r of results) {
    total.input += r.usage.input;
    total.output += r.usage.output;
    total.cacheRead += r.usage.cacheRead;
    total.cacheWrite += r.usage.cacheWrite;
    total.cost += r.usage.cost;
    total.turns += r.usage.turns;
  }
  return total;
}
