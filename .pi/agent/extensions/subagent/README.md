---
summary: Delegate bounded work to specialized subagents with isolated context windows.
tools: [subagent]
category: productivity
keywords: [agents, delegation, roles, parallel, coordinator, workflow]
---

# Subagent

Delegate bounded tasks to specialized subagents with isolated context windows.

:note: This extension was originally copied from the [pi-mono subagent example](https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent/examples/extensions/subagent). It has since deviated from the original source.

## Features

- **Isolated context**: Each subagent runs in a separate `pi` process.
- **Bundled role pack**: Built-in `scout`, `planner`, `reviewer`, `worker`, and `oracle` roles are discovered automatically as package agents, and their names are canonical over stale installed package copies.
- **User/project overrides**: User agents override package-tier agents; trusted project agents can override both with `agentScope: "both"`.
- **Parallel and chain modes**: Fan out independent tasks or pass `{previous}` between sequential steps.
- **Lifecycle timeouts**: Set `timeoutMs` and `idleTimeoutMs` per invocation for long-running or quiet workers.
- **Artifact-first output**: Saves each child final output, transcript, stderr, and summary JSON under `~/.pi/subagent-runs/<runId>/` by default or a caller-provided output directory before formatting the parent result.
- **Durable run state**: Writes `~/.pi/subagent-runs/<runId>/status.json` and `events.ndjson`; inspect with `action: "status"`.
- **Foreground live visibility**: Foreground runs also write per-task live output logs like `~/.pi/subagent-runs/<runId>/output-01.log` and surface current child activity through `action: "status"`.
- **Prune/recovery path**: `action: "prune"` previews or removes stale run-state directories for the current cwd scope and attempts cleanup of recorded managed worktrees/branches.
- **Doctor diagnostics**: `action: "doctor"` checks agent discovery, bundled roles, disabled-agent config, package path, child Pi startup, likely package conflicts, and recoverable worktrees from global recent runs.
- **Bounded child output previews**: `outputMode: "file-only"` returns concise artifact-card summaries with required run/artifact metadata, capped previews, and no full child messages in parent details.
- **Parallel write guard**: Mutating parallel workers sharing a checkout are rejected unless you opt in.
- **Managed worktree isolation**: `worktree: true` creates temporary git worktrees for mutating parallel workers, can symlink `node_modules` or run a setup hook, captures patch/diffstat artifacts, and applies an explicit `worktreeCleanup` recovery policy.
- **Worktree patch check/apply**: `action: "apply"` checks captured worktree patches from a prior run and optionally applies them into a clean target checkout.
- **Usage tracking**: Shows turns, tokens, cost, context usage, and model per child run.
- **Abort support**: Ctrl+C propagates to terminate child `pi` processes.

## Structure

```text
subagent/
├── README.md
├── index.ts             # Registers the subagent tool
├── agents.ts            # Agent discovery logic
├── artifacts.ts         # Run artifact paths, writes, truncation, and read previews
├── doctor.ts            # Doctor diagnostics
├── run-state.ts         # Durable status.json/events.ndjson helpers
├── parallel-safety.ts   # Same-checkout mutating parallel-worker guard
├── patch-apply.ts       # Check/apply captured managed-worktree patch artifacts
├── worktrees.ts         # Managed git worktree creation, patch capture, and cleanup
├── agents/              # Bundled role pack, auto-discovered as package agents
│   ├── oracle.md        # Read-only skeptical second opinion
│   ├── planner.md       # Creates implementation plans
│   ├── reviewer.md      # Code review
│   ├── scout.md         # Fast recon, returns compressed context
│   └── worker.md        # General-purpose worker
└── prompts/             # Extension-local workflow prompt examples
    ├── implement.md
    ├── scout-and-plan.md
    └── implement-and-review.md
```

## Standalone Subagent vs Agent Teams

Use **standalone `subagent`** when you want short-lived, bounded child work:

- the parent stays in the main thread and delegates a specific task;
- the child works in an isolated context window;
- the runtime captures the child's final output, transcript, stderr, and summary JSON under deterministic artifact paths;
- the parent receives a concise summary plus report/artifact paths;
- another terminal can passively inspect foreground progress with `action: "status"` or tail `.pi/subagent-runs/<runId>/output-XX.log` while the parent call is blocked.

Use **Agent Teams** (`/team`, `team_spawn`, `team_message`, `team_status`, etc.) when you need interactive live teammates, durable multi-agent coordination, human-readable mailboxes, or agents that remain active beyond one tool call.

## Security Model

This tool executes a separate `pi` subprocess with a delegated system prompt and tool/model configuration.

Discovery order is intentional; later tiers override earlier tiers by agent name:

1. Package tier: bundled agents (`extensions/subagent/agents/*.md`) plus installed package `agents/*.md`. Bundled role names are canonical inside this tier, so stale installed package copies of `scout`, `planner`, `reviewer`, `worker`, or `oracle` are ignored.
2. User agents (`~/.pi/agent/agents/*.md`) override package-tier agents.
3. Project agents (nearest `.pi/agents/*.md`, only with `agentScope: "project"` or `"both"`) override both when enabled.

Project-local agents are repo-controlled prompts that can instruct the model to read files, run bash commands, and use available tools. Only enable them for repositories you trust.

When running interactively, the tool prompts before running project-local agents. In headless/non-UI execution, requested project-local agents are rejected unless `confirmProjectAgents: false` is explicitly supplied for a trusted repository.

### Child process identity

The spawned `pi` inherits the parent env with Agent Teams identity and spawn metadata removed (`PI_TEAM_NAME`, `PI_TEAM_AGENT_NAME`, `PI_TEAM_SPAWN_KIND`, `PI_TEAM_SUBAGENT_ROLE`, `PI_TEAM_SUBAGENT_FILE`), so a child cannot impersonate its parent teammate. `PI_TEAM_ROLE` is preserved for teammate-aware tools that must stay non-interactive in headless mode.

In place of the scrubbed identity the child gets `PI_AGENT_CLASS=<agent name>`, always overwriting any inherited value. Extensions read it to tell a spawned agent from the driving session — for example `claude-cache-ttl`, which pins spawned agents to a shorter prompt-cache TTL.

### Disabled agents

Set `subagent.disabledAgents` in `~/.pi/agent/settings.json` or the current project's `.pi/settings.json` to block agent names before any child Pi process starts:

```json
{
  "subagent": {
    "disabledAgents": ["oracle", "planner", "reviewer", "scout", "worker"]
  }
}
```

The setting is name-based and applies to package, user, and project agents. Global and project lists are additive, so project settings can disable more names but cannot re-enable globally disabled names. This is an intentional exception to Pi's normal project-overrides-global array behavior. Project settings are read from the current tool cwd's `.pi/settings.json`; unlike project-agent discovery, this setting does not walk parent directories.

Rejected calls fail before run artifacts, worktrees, project-agent confirmation, or child Pi spawn. `doctor`, run `status`, and disabled-agent errors show the active denylist.

### Parallel write guard

Parallel mode blocks multiple mutating workers that would run in the same git checkout by default. The guard treats agents with default tools, unknown tools, `write`/`edit`, and `bash`/shell-ish tools as mutating; agents whose declared tools are all read/search-only are allowed to share a checkout. `writes: false` is honored only for those proven read/search-only agents unless `allowParallelWrites: true` is set; it is not a standalone read-only override for default, unknown, or mutating agents.

Use one of these options when parallel work is intentional:

- set top-level `worktree: true` to let the tool create one managed temporary worktree per mutating parallel task;
- point each mutating task's `cwd` at a distinct worktree/checkout;
- use declared read/search-only agents for non-mutating work; `writes: false` can document that intent, but it cannot downgrade default/unknown/mutating agents unless `allowParallelWrites: true` is set;
- set top-level `allowParallelWrites: true` to opt into same-checkout parallel writes; the result includes a warning.

### Managed worktree isolation

`worktree: true` is available for parallel mode. It prepares all needed worktrees before spawning children, so dirty/non-git/worktree-creation failures fail closed without silently running mutating workers in the shared checkout.

Behavior:

- mutating parallel tasks get generated temporary worktrees under the OS temp directory (`pi-subagent-worktrees/<repo-hash>/<runId>/...`);
- tasks classified as read-only keep their normal resolved `cwd` and do not pay worktree overhead;
- mutating tasks cannot set a custom per-task `cwd` unless it resolves to the top-level run cwd;
- `worktreeSetup: "node-modules"` symlinks the base checkout `node_modules` into each mutating worktree and treats that path as synthetic;
- `worktreeSetupHook` runs a repo-relative executable once per mutating worktree after built-in setup;
- setup-created synthetic paths are excluded from intent-to-add and diff/diffstat capture;
- patch, diffstat, and worktree metadata artifacts are captured under the run artifact directory before cleanup decisions;
- cleanup defaults to current behavior: `worktreeCleanup: "always"` removes owned worktrees/temp branches after patch capture, even if the child failed;
- `worktreeCleanup: "on-success"` removes only successful child worktrees and keeps failed child worktrees/branches for recovery;
- `worktreeCleanup: "never"` captures patch artifacts but keeps all managed worktrees/branches;
- if setup fails, already-created worktrees are rolled back before children start;
- if patch capture fails, the worktree is always kept for recovery because cleanup could otherwise lose changes;
- `action: "status"` includes worktree path, branch, base commit, setup metadata, synthetic paths, patch/diffstat paths, cleanup state, cleanup policy, reason, and cleanup errors for runs in the current cwd scope;
- `action: "prune"` can use that scoped status metadata to recover and clean stale kept/failed managed worktrees before removing old run-state;
- `action: "apply"` validates selected captured patch files and runs `git apply --check` before optionally applying them.

Setup hook contract:

- `worktreeSetupHook` is resolved relative to the git repository root and must stay inside that repository.
- The hook runs with cwd set to the generated worktree root.
- The hook receives JSON on stdin with `runId`, `repoRoot`, `worktreePath`, `taskCwd`, `taskIndex`, `agentName`, branch/base metadata, and already-known `syntheticPaths`.
- The hook may write no stdout or a JSON object like `{ "syntheticPaths": [".cache/worktree-setup"] }`.
- Synthetic paths must be relative worktree paths and may not target `.git`.

Example:

```json
{
  "tasks": [
    { "agent": "anon-worker", "task": "Implement the API changes" },
    { "agent": "anon-worker", "task": "Update the matching tests" },
    { "agent": "planner", "task": "Read-only risk scan", "writes": false }
  ],
  "outputMode": "file-only",
  "worktree": true,
  "worktreeSetup": "node-modules",
  "worktreeSetupHook": "scripts/setup-worktree",
  "worktreeSetupHookTimeoutMs": 120000,
  "worktreeCleanup": "on-success"
}
```

Recovery workflow for kept worktrees:

1. Run `subagent` with `{ "action": "status", "runId": "<runId>" }` or inspect the final summary.
2. Find the kept `worktree` path and `branch` for the task.
3. Review changes directly in that worktree, or apply the captured `.patch` artifact to another checkout.
4. After recovery, remove the worktree/branch manually with `git worktree remove <path>` and `git branch -D <branch>` if still present.

V1 limitations: managed worktrees are parallel-only.

## Usage

### Single agent

```text
Use subagent with agent "scout" to find all authentication code.
```

Equivalent tool shape:

```json
{
  "agent": "scout",
  "task": "Find the authentication entry points and write a concise handoff.",
  "cwd": "/path/to/repo"
}
```

### Parallel execution

```json
{
  "tasks": [
    { "agent": "planner", "task": "Find model-layer changes", "cwd": "/path/to/repo", "writes": false },
    { "agent": "oracle", "task": "Read the diff and identify design risks only", "cwd": "/path/to/repo", "writes": false }
  ],
  "outputMode": "file-only",
  "timeoutMs": 900000,
  "idleTimeoutMs": 120000
}
```

Parallel mode accepts up to 8 tasks and runs up to 4 concurrently. For review fan-outs and other potentially long reports, pass `outputMode: "file-only"` so the parent gets compact artifact cards instead of inline child output.

For mutating parallel workers, prefer `worktree: true` or separate worktrees/cwds unless you explicitly pass `allowParallelWrites: true`. Use a user/project agent such as `anon-worker` for generic implementation examples; replace it with an agent that exists in your environment.

### Parallel with synthesis

When parallel tasks need a final reducer to synthesize their outputs, use `synthesizeWith`:

```json
{
  "tasks": [
    { "agent": "oracle", "task": "Review auth changes for security risks", "writes": false },
    { "agent": "oracle", "task": "Review auth changes for correctness", "writes": false },
    { "agent": "oracle", "task": "Review auth changes for maintainability", "writes": false }
  ],
  "outputMode": "file-only",
  "synthesizeWith": {
    "agent": "anon-worker",
    "task": "Synthesize these artifact-backed parallel reviews into one final assessment, prioritize findings, and dedupe. Compact previews are expected; use the artifact paths provided below when detail is needed, and do not ask to rerun workers solely because previews are compact.\n\n{results}",
    "maxInputChars": 20000
  }
}
```

The synthesis agent runs after all parallel tasks complete. Use the `{results}` placeholder in the synthesis task to include worker outputs, or omit it to automatically append all results. Worker output included in the synthesis prompt is capped by `maxInputChars` (default `20000`; invalid or non-positive values fall back to the default). The example uses read-only `oracle` worker tasks so the parallel write guard allows the shared checkout, and `anon-worker` as a generic reducer; replace `anon-worker` with a user/project agent that exists in your environment. Use managed worktrees, separate `cwd` values, or `allowParallelWrites: true` for mutating/default-tool parallel agents.

The synthesized output becomes the primary result. Individual worker outputs remain available in run artifacts and status. If at least one worker succeeds, synthesis still runs over the successful and failed worker outputs; if all workers fail, synthesis is skipped and the parallel failure is returned.

With `worktree: true`, managed worktrees are created only for parallel workers. The synthesizer runs afterward from its resolved `cwd` (or the top-level run cwd) and receives worktree patch/diffstat metadata in the formatted worker outputs.

Use `synthesizeWith` when:
- Multiple reviewers/investigators cover different angles and need reconciliation
- Parallel findings need deduplication, prioritization, or conflict resolution
- The raw worker outputs are too verbose for direct consumption

Keep this lightweight: `synthesizeWith` is a foreground reducer, not a workflow engine, dashboard builder, or async coordinator.

### Chained workflow

```json
{
  "chain": [
    { "agent": "scout", "task": "Find code relevant to: add Redis caching" },
    { "agent": "planner", "task": "Plan the implementation using this context:\n\n{previous}" },
    { "agent": "anon-worker", "task": "Implement this plan:\n\n{previous}" }
  ],
  "outputMode": "file-only"
}
```

### Artifact-first / file-only output

Artifact-first mode means the runtime saves each child result before it formats the parent-visible summary. Use `outputMode: "file-only"` for parallel reviews, large investigations, and other fan-out/fan-in tasks where child reports may be long. The parent result is intentionally compact: run id, artifact directory, per-worker artifact paths, capped previews, and required status/error metadata. `file-only` bounds child output previews rather than strictly capping every metadata field. In `file-only` mode, parent `details.results[].messages` are stripped; the saved artifacts are the source of truth.

On this branch, omitted `outputMode` still defaults to `"inline"`. For artifact-first workflows, pass `"file-only"` explicitly. Treat `outputMode: "inline"` as an opt-in for small outputs where you really want bounded child text in the parent result.

Single compact call:

```json
{
  "agent": "reviewer",
  "task": "Review the auth diff and write a detailed report.",
  "outputMode": "file-only",
  "output": ".pi/subagent-runs/auth-review/",
  "reads": [".pi/subagent-runs/auth-review/result.md"],
  "maxOutput": 2000
}
```

When `output` is omitted, artifacts are written to the global run capsule at `~/.pi/subagent-runs/<runId>/`. In single mode, `output` may be a file path; in parallel/chain mode, `output` must be a directory path.

Deterministic artifact layout:

- Default artifact directory: `~/.pi/subagent-runs/<runId>/`.
- Parallel/chain/custom-directory results use stable sequence names: `01-<safe-agent>.md`, `01-<safe-agent>.transcript.json`, `01-<safe-agent>.stderr.txt`, and `01-<safe-agent>.summary.json`. A synthesizer uses the next sequence number, such as `03-anon-worker.md` after two workers.
- Single-agent default artifacts are `result.md`, `result.transcript.json`, `result.stderr.txt`, and `result.summary.json`; a single-agent file `output` also mirrors the final Markdown to that explicit file path.
- Per-result `*.summary.json` files include status/path metadata and `outputSummary` stats such as bytes, chars, lines, SHA-256, preview, `truncated`, and `omittedChars`.
- Durable run state is always written to `~/.pi/subagent-runs/<runId>/status.json` and `~/.pi/subagent-runs/<runId>/events.ndjson`, even when `output` points at a custom artifact directory. Foreground runs also write per-task live output logs in the same run-state directory (`output-01.log`, `output-02.log`, etc.). For custom artifact directories, `status.json` is also mirrored into that directory for convenience. Each status records the original execution `cwd`, which remains the source of truth for repo-sensitive recovery behavior.
- Managed worktree runs can also write `worktree-<index>.patch`, `worktree-<index>.diffstat.txt`, and `worktree-<index>.worktree.json` artifacts, plus cleanup manifests when relevant.
- Run-level `index.md` and `manifest.json` summarize each result and should be the first files to read when available.

No-rerun guidance:

- Compact previews in the parent result, collapsed UI, and status output are expected.
- To recover detail, first read the artifact directory, `index.md`, `status.json`, the per-worker `*.md` result files, or the per-worker `*.summary.json` files.
- Prefer `synthesizeWith` for fan-out/fan-in review synthesis instead of rerunning workers just to expand previews.
- Rerun only when a worker failed or was canceled, the wrong task/agent/cwd was used, or an artifact path is missing, corrupt, or unreadable.

### Check or apply captured worktree patches

Use `action: "apply"` after a managed-worktree run captured `worktreePatchPath` values in status. By default it is check-only: it validates the run, selector, patch paths, and target checkout cleanliness, then runs `git apply --check` against all selected patches before writing an audit artifact.

Selectors are mutually exclusive:

- `all: true` selects all tasks in the run that have captured worktree patches;
- `taskIds: ["task-01"]` selects status task IDs;
- `taskIndexes: [0]` selects zero-based status task indexes.

Check only:

```json
{
  "action": "apply",
  "runId": "20260529T123456Z-0000",
  "taskIds": ["task-01"]
}
```

Apply after the check passes:

```json
{
  "action": "apply",
  "runId": "20260529T123456Z-0000",
  "all": true,
  "apply": true,
  "threeWay": true
}
```

Safety rules:

- `runId` is required and exactly one selector is required;
- selected tasks must have `worktreePatchPath` values;
- patch files must be readable files under the run artifact directory or `.pi/subagent-runs/<runId>/`;
- the target checkout must be clean, ignoring Pi's own untracked `.pi/` artifacts;
- all selected patches are checked together before any apply command runs;
- each attempt writes `.pi/subagent-runs/<runId>/apply-<timestamp>.json` with selected tasks and git command outcomes.

### Status, prune, and doctor

`status` and `prune` read the global run store, but only return or modify runs whose recorded `cwd` is the current tool cwd or one of its descendants. A provided `cwd` parameter is ignored for these actions. Run status shows the `subagent.disabledAgents` denylist recorded when the run started. Doctor worktree-recovery inspection remains an unscoped diagnostic utility, and doctor also reports the current active denylist plus config errors or warnings.

List recent runs (mtime-capped before JSON parsing, default limit 10):

```json
{ "action": "status", "limit": 20 }
```

Inspect one run:

```json
{ "action": "status", "runId": "20260529T123456Z-00000000" }
```

During a foreground run, `action: "status"` is the passive sidecar view. The parent tool call remains blocked, but another terminal can inspect `.pi/subagent-runs/<runId>/status.json`, read `.pi/subagent-runs/<runId>/events.ndjson`, or tail the per-task live output logs:

```bash
tail -f .pi/subagent-runs/20260529T123456Z-0000/output-01.log
```

The single-run status output summarizes live fields when present:

- output log paths such as `.pi/subagent-runs/<runId>/output-01.log`;
- current tool and path-like target (`currentTool`, `currentPath`, `currentToolStartedAt`);
- last activity (`lastActivityAt`) at the run and task level;
- recent child output (`recentOutput`) and recent tools (`recentTools`);
- live/total activity counts (`turnCount`, `toolCount`) and the child `model`.

The canonical files remain `status.json` and `events.ndjson`; the sidecar appends bounded child activity events such as `child_message`, `child_tool_started`, `child_tool_finished`, and `child_stderr` previews when available. It does not add background execution, resume/interrupt controls, or dashboard semantics.

Preview stale run cleanup. This is the safe default: it does not delete anything.

```json
{ "action": "prune", "olderThanDays": 14 }
```

Apply cleanup after reviewing the preview:

```json
{ "action": "prune", "olderThanDays": 14, "dryRun": false }
```

Prune removes stale run-state directories in the current cwd scope. For stale runs with kept/failed managed worktrees in `status.json`, it first attempts to remove the recorded owned worktree and `subagent/<runId>/...` branch. Runs still marked `running` are skipped unless `includeRunning: true` is set. If worktree cleanup fails, the run-state directory is kept so recovery metadata is not lost.

Run diagnostics without providing `agent`, `task`, `tasks`, or `chain`:

```json
{ "action": "doctor", "agentScope": "both" }
```

Check/apply actions also do not require `agent`, `task`, `tasks`, or `chain`.

## Tool Parameters

| Parameter | Description |
|-----------|-------------|
| `action` | `"run"` (default), `"status"`, `"doctor"`, `"prune"`, or `"apply"`. |
| `runId` | Run ID to inspect when `action` is `"status"`; omit it to list recent runs; required when `action` is `"apply"`. |
| `limit` | Maximum recent runs to list for `action: "status"`. Defaults to 10. |
| `olderThanDays` | For `action: "prune"`, consider run-state stale after this many days. Defaults to 14. |
| `dryRun` | For `action: "prune"`, preview without deletion. Defaults to `true`; set `false` to apply. |
| `includeRunning` | For `action: "prune"`, also prune stale runs still marked `running`. Defaults to `false`. |
| `pruneWorktrees` | For `action: "prune"`, attempt cleanup of recorded managed worktrees/branches before removing stale run-state. Defaults to `true`. |
| `taskIds` | `action: "apply"` selector: status task IDs such as `task-01`; mutually exclusive with `taskIndexes` and `all`. |
| `taskIndexes` | `action: "apply"` selector: zero-based status task indexes; mutually exclusive with `taskIds` and `all`. |
| `all` | `action: "apply"` selector: select all captured worktree patch tasks; mutually exclusive with `taskIds` and `taskIndexes`. |
| `apply` | `action: "apply"` mode: defaults to `false` for check-only; set `true` to apply after checks pass. |
| `threeWay` | `action: "apply"` mode: pass `--3way` to `git apply --check` and `git apply`. |
| `agent` + `task` | Single-agent mode. |
| `tasks` | Parallel mode: array of `{ agent, task, cwd?, writes? }`. `writes: false` is honored only for declared read/search-only agents unless `allowParallelWrites: true` is set. |
| `synthesizeWith` | Optional synthesis step for parallel mode only: `{ agent, task, cwd?, maxInputChars? }`. Runs one reducer after all parallel tasks complete. Use `{results}` placeholder in the task to include worker outputs, or omit it to automatically append results. `maxInputChars` defaults to `20000`; invalid/non-positive values use the default. The synthesized output becomes the primary result while worker artifacts and failure status are preserved. |
| `chain` | Sequential mode: array of `{ agent, task, cwd? }`; use `{previous}` to pass prior output. |
| `agentScope` | `"user"` by default; use `"both"` or `"project"` to include trusted project-local agents. |
| `confirmProjectAgents` | Prompt before running project-local agents. Defaults to `true`. |
| `cwd` | Default root for child execution, artifact output, safety validation, and managed-worktree preparation in single, parallel, and chain modes. Per-task `cwd` values resolve from it. For `apply`, it is the target checkout; `status` and `prune` ignore it and remain scoped to the tool cwd. |
| `allowParallelWrites` | Permit mutating parallel tasks in the same checkout and let explicit `writes: false` opt out of conservative mutating classifications. Defaults to `false`. |
| `worktree` | Create managed temporary git worktrees for mutating parallel tasks. Parallel mode only; requires a clean git repo except Pi's own untracked `.pi/` artifacts. Defaults to `false`. |
| `worktreeSetup` | Optional setup for managed worktrees. Use `"node-modules"` to symlink the base checkout `node_modules`; default is `"none"`. Requires `worktree: true`. |
| `worktreeSetupHook` | Repo-relative executable setup hook for each mutating managed worktree. Receives JSON on stdin and may return JSON synthetic paths on stdout. Requires `worktree: true`. |
| `worktreeSetupHookTimeoutMs` | Timeout for `worktreeSetupHook` in milliseconds. Defaults to `120000`. Requires `worktree: true`. |
| `worktreeCleanup` | Cleanup/recovery policy for `worktree: true`: `"always"` (default, remove after patch capture), `"on-success"` (keep failed child worktrees), or `"never"` (keep all managed worktrees/branches). Patch-capture failures are always kept. |
| `timeoutMs` | Hard timeout per subagent run in milliseconds. |
| `idleTimeoutMs` | Idle timeout per run in milliseconds with no stdout/stderr output. |
| `output` | Artifact destination relative to `cwd`. In single mode this may be a file path; in parallel/chain it must be a directory path. |
| `outputMode` | `"inline"` is the current default when omitted and returns bounded child output plus artifact paths; use it intentionally for small outputs. `"file-only"` is the artifact-first mode for reviews/fan-outs: concise parent summaries with artifact paths, capped child output/read previews, required metadata, and no full child messages in parent details. |
| `reads` | Parent-facing file paths to preview in the final summary. This is not a permission model. |
| `maxOutput` | Maximum inline child output/preview characters per result. Defaults to 2000. |
| `model` | Optional model override for this run, e.g. `"openai/gpt-5.6-luna:xhigh"`. Pass `"inherit"` to use the parent session's live model. Wins over the agent's frontmatter `model`; when omitted, the frontmatter value is used (which may itself be `"inherit"`). |

## Bundled Roles

| Agent | Purpose | Model | Tools |
|-------|---------|-------|-------|
| `scout` | Fast codebase recon and compressed handoff context | GPT-5.6 Luna (xhigh) | read, grep, find, ls, bash |
| `planner` | Concrete implementation plans from context and requirements | Opus 4.8 | read, grep, find, ls |
| `reviewer` | Quality/security/maintainability review | Sonnet 5 | read, grep, find, ls, bash |
| `worker` | General-purpose implementation worker | GPT-5.6 Sol | default tool profile |
| `oracle` | Read-only skeptical second opinion for design risk and smallest-good-fix pressure testing | Sonnet 5 | read, grep, find, ls |

Use `subagent.disabledAgents` to avoid invoking bundled roles without editing package files or changing discovery precedence.

## Agent Definitions

Agents are markdown files with YAML frontmatter:

```markdown
---
name: my-agent
description: What this agent does
tools: read, grep, find, ls
excludeTools: bash, write, edit
model: openai/gpt-5.6-luna
thinking: high
---

System prompt for the agent goes here.
```

`excludeTools` accepts comma-separated tool names. Values are trimmed, blanks are ignored, and the child receives one comma-joined `--exclude-tools` argument. When the field is omitted or empty, no `--exclude-tools` flag is passed.

Set `model: inherit` to make the agent use the parent session's **live model** (e.g. after a Ctrl+P switch) instead of a fixed model. When `inherit` is set and no parent model is available, the child falls back to its own default (no `--model` flag). This is useful when you run a non-Anthropic primary model (e.g. GLM 5.2) and want subagents to follow mid-session model switches rather than pinning a hardcoded model per agent.

`thinking` accepts `off`, `minimal`, `low`, `medium`, `high`, `xhigh`, or `max`. `max` requires Pi >=0.80.6. Pi applies any model-specific clamping to the requested level. Parent thinking is not inherited; use standalone agent `thinking` when a child should request a level.

The top-level tool-call model override, including any thinking suffix, applies run-wide to single, chain, parallel, and synthesis execution.

Thinking and model precedence:

| Input | Effective request |
|---|---|
| Tool-call model with a recognized terminal **syntactic** thinking suffix | Preserve the model string and suppress agent `--thinking`. |
| Tool-call model without a thinking suffix | Override the model only and retain agent thinking. |
| Tool-call `model: inherit` | Resolve the parent model as usual and retain agent thinking. |
| Blank tool-call model | Treat it as omitted and retain the agent model and thinking. |
| No tool-call model; standalone agent thinking exists | Emit `--thinking`; it wins over any agent-model suffix. |
| No standalone agent thinking | Preserve existing model-suffix, settings, and default behavior. |
| Invalid or non-string agent thinking | Keep the agent discoverable and emit no thinking flag. |

Suffix detection is syntactic rather than model-registry-aware. A model ID that exactly ends in `:high`, `:max`, or another supported level is treated as a thinking suffix because the parent process cannot mirror Pi's exact-model-first resolution.

Locations:

- `extensions/subagent/agents/*.md` — bundled package roles, loaded automatically by this extension; these role names are canonical within the package tier.
- Installed package `agents/*.md` — package agents discovered from configured packages, except names already provided by the bundled role pack.
- `~/.pi/agent/agents/*.md` — user-level agents, loaded when `agentScope` is `"user"` or `"both"`, and allowed to override package-tier names.
- `.pi/agents/*.md` — project-level agents, loaded only with `agentScope: "project"` or `"both"`, and allowed to override package/user names when enabled.

## Output Display

Collapsed view shows status, agent name, recent tool calls/text, and usage stats. Expanded view (Ctrl+O) shows full task text, all tool calls, final Markdown output, and per-task usage.

Parent-visible child output/previews are bounded, and `file-only` summaries are intentionally compact while still including required artifact/status metadata. Do not rerun workers solely to recover preview text; see run artifacts or use `synthesizeWith` for the full captured output/transcripts. For in-flight foreground runs, use `action: "status"` for compact live fields or tail the task's `output-XX.log` file for human-readable output as it is appended.

## Error Handling

- **Exit code != 0**: Tool returns an error with stderr/output.
- **stopReason `"error"`**: Model error is propagated with the error message.
- **stopReason `"aborted"`**: User abort kills subprocesses and reports the abort.
- **Hard/idle timeout**: Lifecycle-safe runner terminates the child process and reports the timeout reason.
- **Chain mode**: Stops at the first failing step and reports which step failed.
- **Parallel write guard**: Blocks same-checkout mutating parallel workers before spawning children.
- **Managed worktree preparation**: `worktree: true` fails closed on dirty/non-git/worktree-creation/setup errors before spawning children and rolls back already-created worktrees.
- **Patch capture / cleanup**: Patch capture failures keep the worktree for recovery; `worktreeCleanup` controls post-capture removal; cleanup failures are reported in result/status metadata without hiding the child result.
- **Patch check/apply**: Missing run IDs, ambiguous selectors, missing/unsafe patch paths, dirty target checkouts, or `git apply --check` failures fail before applying patches.
- **Disabled agents**: Requests for names in `subagent.disabledAgents` fail before run artifacts, worktrees, project-agent confirmation, or child Pi spawn; malformed global config fails closed, while project settings load failures warn and proceed with global policy.
- **Status lookup**: Missing or corrupt `status.json` returns a clear `action: "status"` error.
- **Prune**: Defaults to dry-run; skips runs still marked `running`; keeps run-state if recorded worktree cleanup fails; scoped tool calls skip corrupt/unknown statuses that cannot be proven inside the current cwd scope.

## Limitations

- Output is truncated in collapsed view, and `file-only` summaries show compact previews by design. Read artifacts/status for full output; rerun only on failure, wrong task, or missing/corrupt artifacts. `action: "status"` also bounds `recentOutput`; tail `output-XX.log` for fuller live foreground output.
- Agents are discovered fresh on each invocation, so edits are picked up mid-session.
- Parallel mode is limited to 8 tasks, 4 concurrent.
- Reusing the same explicit output directory can overwrite prior result filenames.
- `action: "status"` is read-only observability for completed/in-flight foreground tool calls; it is not an async job system, interrupt/resume mechanism, or dashboard.
- `action: "prune"` only cleans managed worktrees when stale run status has enough metadata (`worktreePath` and owned `subagent/<runId>/...` branch); otherwise it reports the leftover for manual cleanup.
- Standalone `subagent` is not a durable team coordinator; use Agent Teams for persistent teammates and live coordination.
