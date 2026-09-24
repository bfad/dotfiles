---
summary: Delegate work to visible Pi teammates that run alongside your main instance.
commands: [/teams]
tools: [team_spawn, subagent, team_status, team_message, team_broadcast, team_request_shutdown, team_force_shutdown, team_cleanup]
category: workflow
keywords: [teams, teammates, parallel, delegation, multi-agent, coordination]
status: stable
---

# Agent Teams

Delegate work to visible Pi teammates that run alongside your main instance.
You stay free to keep working while they do.

## Why use teammates?

- **Team lead remains available** — unlike subagents, the main instance stays idle and responsive while teammates work
- **Visibility & interactivity** — watch teammates work in real-time and interact directly in their pane
- **Preserved context** — offload work to teammates instead of filling your own context window
- **Durable assignments** — the initial `team_spawn` task is loaded into the teammate's system prompt so it survives context compaction
- **Parallel execution** — multiple tasks running at the same time

## Examples

Spawn teammates to work on related but independent tasks simultaneously:
> "Refactor the auth module to use the new token format, and update all the integration tests separately"

Divide and conquer a large investigation across existing code, Vault, Grokt, and other sources:
> "Figure out how our rate limiting works end-to-end — I need to understand the full picture"

Execute a multi-step plan, delegating each step to a fresh teammate to preserve context:
> "Here's my plan with 4 tasks. Execute them one at a time, spawning a new teammate for each"

Get a teammate to research something in the background while you keep working:
> "Spawn a teammate to research what changed in the GraphQL API between v2024-01 and v2025-01"

Have a teammate review your changes (pairs well with code-reviewing skills):
> "Spawn a teammate to review the changes on this branch"

Spawn a teammate in a different worktree by passing a working directory:
> "Spawn a teammate in .worktrees/feat-agent-teams-cwd to implement cwd support in the extension"

`team_spawn` accepts an optional `cwd` parameter. Relative paths resolve from the lead's current working directory.

`team_spawn` also accepts an optional `model` parameter, but you should omit it by default so the teammate inherits the lead's current model. Set a model only when you have a concrete reason to choose a different one.

For normal teammates, the initial task is loaded into the teammate's system prompt as an initial assignment from `@team_lead`; a minimal mailbox kickoff triggers the first turn. Follow-up `team_message` messages remain mailbox/team messages.

## Child session history

Every Agent Teams teammate and subagent writes its operational transcript to a
unique private directory under `~/.pi/agent/team-sessions/` (or
`$PI_CODING_AGENT_DIR/team-sessions/`). Pi's normal `/resume` picker does not scan
these directories. The files remain available while a team is live and after the
lead resumes, so file-based team cost tracking keeps working. Lead sessions prune
private child directories with no writes for 7 days on startup. Existing child
transcripts in Pi's default session directories are supported only as a legacy
cost-tracking fallback.

## Reading teammate messages

When a teammate messages you, it renders as a blockquote in your pane so it's
clearly distinct from your own output: a coloured left rail and a bold sender
name, with the body indented beside it. The message body stays visible in full;
press the expand key (e.g. `Ctrl+O`) to reveal appended operational notes.

Each sender gets a stable rail colour derived from their name (same name → same
colour), drawn from a colour-blind-safe palette. The header and body use Pi's
`customMessageText` and `customMessageBg` theme colours. Light, dark, and custom
themes supply their own foreground/background pair, including after a theme change.

Set `PI_TEAM_MESSAGE_BG=off` (or `0`/`false`/`none`) to turn the message background
off and keep just the coloured rail.

## Team cost in the status bar

The lead's footer status line reports the running cost of every teammate
spawned this session, including teammates that have since shut down or been
killed. Costs are read from each teammate's persisted session file (matched by
spawn directory and spawn time) and refreshed on team events and every 5
seconds:

- Active team: `@team_lead [my-team] · team cost $2.41`
- RPC mode: `Teammates: 2 running, 1 idle · team cost $2.41 · Ctrl+Shift+M to view`
- After `team_cleanup`: `team cost $2.41 (session total)` — the total stays
  visible for the rest of the session

Each spawn is persisted to the lead's session as a custom entry, so the
tracker rehydrates on `/resume` and the session total keeps covering teams
from earlier in the session. Teammate session files are matched by spawn
directory, spawn time, and the `@name` session name teammates set at startup,
so an unrelated session in the same directory is never counted.

## Context usage on messages

Every `team_message`, `team_broadcast`, and `team_shutdown` report carries a
snapshot of the sender's context-window usage, captured at send time. The
reading agent receives it as a trailing one-line note on the message body,
named with the sender so it is unambiguously theirs —
`[@alice context usage: 142,000 / 200,000 tokens (71%)]` — so the lead can tell
when a teammate is running low on context.

This powers a simple workflow: when a teammate reports back with high usage, the
lead can retire it with `team_request_shutdown` and spawn a fresh replacement
with `team_spawn`, keeping the work on a teammate with healthy context.

The note is steering data, not something the human needs to read on every
message, so it is hidden in the collapsed message view and revealed only when
you expand the message. Messages from senders whose token count is unknown
(e.g. right after compaction) simply omit the note.

## Wait states and mailbox resume

Agent Teams persists each teammate's reported runtime state separately from the
shared team config. `team_status` can show `working`, `waiting-for-lead`, or
`completed-awaiting-shutdown`; a dead process is always reported as `dead`,
regardless of stale persisted state. If a state write fails after a message was
enqueued, the send remains successful and the result includes a warning, which
avoids encouraging a duplicate send.

## Dead recipients and stale registrations

A mailbox write always lands, even when the recipient has already exited, so
`team_message` used to report success for work that would never happen.
`team_message` now fails when the recipient is unknown, or when this process can
*prove* it has exited (its own RPC handle or a pane manager). Where liveness is
unobservable — a teammate messaging a peer — the send still goes through, because
an unobservable member must not be mistaken for a dead one. `team_broadcast`
applies the same rule per recipient and reports who it skipped.

Observability is judged per member, not per process. A pane manager can only
answer for panes of its own backend, so an RPC member (`paneId: "none"`), a
member whose `transport` is a different multiplexer, and a spawn still in
flight (`state: "starting"`) all stay unobservable even in a lead that has a
pane manager. RPC and VS Code members are judged by an ESRCH pid probe; a
missing pid means unobservable.

Because unregistering a member frees its name for reuse, `reserveMember` now
empties the name-scoped mailbox before a new instance claims it — otherwise a
respawned `@worker` could inherit the previous occupant's unread messages and
run obsolete work (or act on a stale shutdown request).

Teammates that finish on their own are never unregistered, so a reused team name
grows without bound. `team_status` unregisters members it can prove are gone
(reporting them once as `removed N finished`) and skips any member whose
shutdown handshake is still pending. Removal goes through
`removeMemberRegistration` and is scoped to the observed `instanceId`, because
the config is shared on disk and the same name may have been respawned since
the snapshot. Members predating `instanceId` are reported but never reaped;
clear those with `team_force_shutdown`.

Follow-up messages use the filesystem mailbox. A teammate never destructively
polls that mailbox while it is busy, streaming, or already handling a batch.
When it is safely idle, it peeks at one validated, ordered snapshot without
deleting it and starts one turn with a single
`agent-teams-mailbox-batch-v1` custom message. The wrapper contains the ordered
messages as canonical JSON and carries its `batchId` and ordered `messageIds`
in `details`.

`agent_start` confirms only that the turn began. Mailbox files are deleted by
exact ID only after the matching `agent_end` contains exactly one wrapper with
the same protocol, `batchId`, and ordered `messageIds`. Missing, changed, or
ambiguous correlation retains the files and warns the lead instead of guessing.
Messages that arrive after the snapshot remain for the next safely idle turn.
The lead path is unchanged: the lead still receives individual `team-message`
displays through its read-and-delete mailbox poll.

Delivery is at-least-once, not exactly-once. A crash before the correlated
delete, or a partial delete failure, can replay messages after a restart or
same-name respawn. Mailbox publication provides collision-exclusive atomic
visibility of the final file on the same filesystem, but it doesn't claim
`fsync` or power-loss durability.

The initial assignment keeps its existing single-message kickoff semantics:
each successful spawn enqueues one ordinary kickoff. Mailbox and teammate-state
paths remain name-scoped, and a later same-name spawn reuses any preserved
mailbox messages. If a failed spawn can't prove that the kickoff was never
consumed, a retry can leave another kickoff for at-least-once replay.
`team_cleanup` is the only operation that recursively purges mailbox data.

## Team lifecycle: binding, cleanup, and rebinding

A lead session binds to one team — the first one `team_spawn` creates. While that binding is active, the `team` parameter on later spawns is ignored and teammates join the bound team.

A teammate name remains held while its registration is `starting`, `running`, or `stopping`. When a teammate begins shutdown, or the lead force-shuts it down, Agent Teams marks the exact instance as `stopping` and requests termination. Requesting termination isn't observed exit: for a remote procedure call (RPC) teammate, its config registration and in-memory entry remain until that exact process exits, so an immediate same-name respawn is rejected instead of creating two consumers for the same name-scoped mailbox.

For pane backends, force shutdown deregisters the teammate only when the backend immediately confirms that the pane is no longer alive. Automatic deregistration after an otherwise unobserved pane crash is unsupported. In that case the stopping or running registration can remain and continue to hold the name; Agent Teams doesn't claim that an unobserved crash cleans itself up.

`team_cleanup` is the runtime owner of the binding lifecycle. On success it kills the teammates, removes the team files, stops watching, switches the UI status to the session cost total, and then resets the in-memory binding (`currentTeamName = null`, `teamInitialized = false`). Because the reset runs after teardown, the next `team_spawn` in the same lead session may establish a new team using its requested name — no new Pi session is required after a successful cleanup.

If cleanup fails before those resets, the binding reset is not reached and the binding is retained. Teardown may already be partial (for example, teammates killed, the in-memory RPC map cleared, or the old config removed), so a later spawn recreates the retained team binding rather than rebinding to the requested team. The safe fallback after a cleanup failure is a fresh lead session. The extension reports the exact error and preserves whatever on-disk state remains; Fleet never deletes Agent Teams internals manually.

## Pane environment handoff

Pane-backed teammates copy `TOOL_GATEWAY_TOKEN` and
`TOOL_GATEWAY_MCP_URL` from the lead process through a per-spawn temporary
file. Agent Teams writes the file with mode `0600`, puts only its path in the
pane command, loads it with shell tracing disabled, and removes it before any
staggered sleep or Pi startup. The values stay local to the shell during the
sleep and are exported only when the shell executes Pi.

The handoff explicitly unsets values that are absent from the lead, so a stale
terminal application environment cannot leak into a teammate. Spawn rollback,
force shutdown, full team cleanup, and a stale-start timeout all remove pending
handoffs. RPC teammates keep their existing full `process.env` inheritance.

## Passing extra CLI args to teammates

Set `PI_TEAM_EXTRA_ARGS` to append extra flags to every teammate the lead
spawns. Unset, spawning is unchanged. When set, the value is appended to the
end of the teammate's `pi` command (after the model, tools, and
system-prompt flags).

The motivating case is local development. If you launch the lead with a
working-copy extension:

```bash
pi -ne -e extensions/agent-teams/index.ts
```

its teammates still spawn a plain `pi` and load the *installed* extension — so
features only present in your working copy won't run on the teammate side. Tell
teammates to load the same working copy:

```bash
PI_TEAM_EXTRA_ARGS="-ne -e extensions/agent-teams/index.ts" pi -ne -e extensions/agent-teams/index.ts
```

The value is tokenised into argv while honouring single and double quotes
(e.g. `-e "my dir/index.ts"`). RPC children receive those arguments directly;
pane children receive each parsed argument as an individually shell-escaped
literal, so shell expansion is not evaluated. Relative paths resolve from the
teammate's working directory. Session-control arguments (`--no-session`,
`--session-dir`, `--session`, `--session-id`, `--resume`, `--continue`,
`--fork`, `-r`, and `-c`) are rejected because child session isolation cannot
be overridden.

## iTerm2 teammate layout

On iTerm2 the lead pane is split for the first teammate, and additional
teammates stack top/bottom. Configure via `~/.pi/teams/config.json`:

```json
{
  "itermLayout": "split",
  "itermSplitDirection": "auto"
}
```

`itermLayout` values: `"split"` (default — split the lead pane), `"tab"`
(teammates in a new tab), `"window"` (teammates in a new window).

`itermSplitDirection` controls the first split in `"split"` layout:

- `"auto"` (default) — reads the lead window's aspect ratio: landscape
  windows split side-by-side, portrait/square windows split top/bottom so
  panes keep a usable line width. Falls back to side-by-side when geometry
  can't be read.
- `"right"` — always side-by-side (the pre-`auto` behavior).
- `"down"` — always top/bottom.

## cmux teammate layout

cmux uses split panes by default: the lead stays on the left while teammates
stack vertically on the right. To open every teammate as a tab beside the lead
instead, create `~/.pi/teams/config.json` with:

```json
{
  "cmuxLayout": "tab"
}
```

Valid values are `"split"` (default) and `"tab"`. Restart Pi or run `/reload`
after changing the setting. It applies to newly spawned teammates and does not
move existing teammate surfaces.

## Ghostty teammate layout

On Ghostty (macOS, 1.3.0+) the lead pane is split right for the first
teammate, and additional teammates stack top/bottom. Panes are managed
through Ghostty's native AppleScript API (enabled by default; requires the
macOS Automation permission prompt to be accepted).

Limitations: Ghostty's AppleScript dictionary cannot read terminal contents,
so pane capture (`/team_diagnose`) is unsupported under Ghostty.

## Supported backends

Detected automatically — no configuration needed:

1. **herdr** — split panes in the herdr workspace (via herdr CLI, found through `$HERDR_BIN_PATH` or PATH). Detected first, before the host terminal, since herdr commonly runs on top of WezTerm.
2. **tmux** / **zellij** / **WezTerm mux** — split panes in your multiplexer
3. **Shuttle** — Ghostty workspace panes (via Shuttle CLI)
4. **cmux** — Ghostty workspace splits or sibling tabs (via cmux CLI)
5. **Ghostty** — native macOS split panes via AppleScript (requires Ghostty 1.3.0+)
6. **iTerm2** — native macOS panes (split, tab, or window)
7. **Fallback** — background processes, visible via `Ctrl+Shift+M` overlay

## Subagents

When the standalone `subagent` extension is not enabled, Agent Teams provides a `subagent` tool.

A subagent is a focused teammate spawned with a role file and a short lifecycle:

1. It works on one assigned task.
2. It reports back when done, blocked, or needing clarification.
3. It shuts down after reporting completion.

The `subagent` tool supports the same invocation shape as Pi subagents:

- `{}` — list available subagent roles, including the source file path for each role
- `{ agent, task }` — spawn one subagent
- `{ tasks: [...] }` — spawn parallel subagents
- `{ chain: [...] }` — start a sequential chain by spawning the first step

Agent Teams discovers role files from installed Pi packages, `~/.pi/agent/agents`, project `.pi/agents`, project `.agents/agents`, and skill-local `.agents/skills/**/agents` directories. Package role files do not need to be symlinked separately for Agent Teams subagents.

Unlike blocking subprocess-based subagents, Agent Teams subagents return after spawning so the lead remains available. The lead should wait for completion reports before continuing dependent work.
