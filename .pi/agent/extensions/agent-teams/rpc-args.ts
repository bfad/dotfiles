export interface RpcCliArgsOptions {
  sessionDir: string;
  model?: string;
  tools?: string;
  systemPromptPaths?: string[];
  /** Parsed extra CLI args (from PI_TEAM_EXTRA_ARGS) appended at the end. */
  extraArgs?: string[];
}

export function buildRpcCliArgs(piExe: string, options: RpcCliArgsOptions): string[] {
  const args = piExe === "devx"
    ? ["pi", "--mode", "rpc", "--session-dir", options.sessionDir]
    : ["--mode", "rpc", "--session-dir", options.sessionDir];

  if (options.model) {
    args.push("--model", options.model);
  }
  if (options.tools) {
    args.push("--tools", options.tools);
  }
  if (options.systemPromptPaths) {
    for (const systemPromptPath of options.systemPromptPaths) {
      args.push("--append-system-prompt", systemPromptPath);
    }
  }
  if (options.extraArgs) {
    for (const extraArg of options.extraArgs) {
      args.push(extraArg);
    }
  }

  return args;
}
