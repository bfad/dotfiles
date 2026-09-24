import { PI_NO_NOTIFY, PI_NO_NOTIFY_VALUE, childPiEnv } from "../../lib/child-pi-env.js";
import { shellEscape } from "./shared-utils.js";
import {
  PI_TEAM_INSTANCE_ID_ENV,
  PI_TEAM_SESSION_DIR_ENV,
  resolveAgentDir,
} from "./private-sessions.js";

export interface RpcChildPiEnvOptions {
  baseEnv: NodeJS.ProcessEnv;
  extraEnv?: Record<string, string>;
  teamName: string;
  agentName: string;
  instanceId: string;
  sessionDir: string;
}

export function buildRpcChildPiEnv(options: RpcChildPiEnvOptions): NodeJS.ProcessEnv {
  const env: NodeJS.ProcessEnv = {
    ...options.baseEnv,
    ...options.extraEnv,
    PI_TEAM_NAME: options.teamName,
    PI_TEAM_ROLE: "teammate",
    PI_TEAM_AGENT_NAME: options.agentName,
    [PI_TEAM_INSTANCE_ID_ENV]: options.instanceId,
    [PI_TEAM_SESSION_DIR_ENV]: options.sessionDir,
  };
  env.PI_CODING_AGENT_DIR = resolveAgentDir(
    options.baseEnv.PI_CODING_AGENT_DIR ?? options.extraEnv?.PI_CODING_AGENT_DIR,
  );
  return childPiEnv(env);
}

export function appendPaneChildPiEnv(envParts: string[]): string[] {
  return [
    ...envParts,
    `${PI_NO_NOTIFY}=${shellEscape(PI_NO_NOTIFY_VALUE)}`,
  ];
}
