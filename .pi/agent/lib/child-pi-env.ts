export const PI_NO_NOTIFY = "PI_NO_NOTIFY";
export const PI_NO_NOTIFY_VALUE = "1";

/**
 * Marks a spawned child with the kind of agent it is (an agent definition name
 * such as `scout`, or a role such as `teammate`). Set by whoever spawns the
 * child process; read by extensions that vary behaviour between the driving
 * session and the agents it spawns.
 *
 * Only meaningful for out-of-process children. In-process subagents (omp `task`)
 * share one `process.env`, so a consumer MUST prefer an in-session signal and
 * fall back to this marker, never the reverse.
 */
export const PI_AGENT_CLASS = "PI_AGENT_CLASS";

export function childPiEnv(env: NodeJS.ProcessEnv = process.env): NodeJS.ProcessEnv {
  return {
    ...env,
    [PI_NO_NOTIFY]: PI_NO_NOTIFY_VALUE,
  };
}

/**
 * Stamp a child env with its OWN agent class. An inherited value is always
 * replaced (or removed when `agentClass` is blank) so a parent's identity can
 * never leak into a grandchild.
 */
export function withAgentClass(
  env: NodeJS.ProcessEnv,
  agentClass: string,
): NodeJS.ProcessEnv {
  const next = { ...env };
  const trimmed = agentClass.trim();
  if (trimmed) next[PI_AGENT_CLASS] = trimmed;
  else delete next[PI_AGENT_CLASS];
  return next;
}

export function notificationsDisabled(env: NodeJS.ProcessEnv = process.env): boolean {
  return env[PI_NO_NOTIFY] === PI_NO_NOTIFY_VALUE;
}
