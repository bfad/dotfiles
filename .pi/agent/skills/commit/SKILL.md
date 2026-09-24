---
name: commit
description: "Write a git commit message and commit staged or unstaged changes. Use when asked to commit, write a commit message, or prepare changes for commit."
---

# Git Commit

Create a git commit with a well-crafted message following classic git conventions
(tpope + Chris Beams style).

## Rules

### Subject line

1. **Imperative mood** — write as a command: "Fix bug", not "Fixed bug" or "Fixes bug".
   The subject should complete the sentence: *"If applied, this commit will ___."*
2. **Capitalize** the first word.
3. **No trailing period.**
4. **Target ~60 characters.** Going up to 72 is acceptable when needed for clarity,
   but never exceed 72. If you're struggling to fit, the commit may be too large —
   mention this to the user.
5. **Be specific and concise.** Avoid vague subjects like "Update code" or "Fix stuff".

### Body (optional but encouraged for non-trivial changes)

6. **Separate from subject with a blank line.** This is critical — tools like `log`,
   `shortlog`, and `rebase` rely on it.
7. **Wrap at 72 characters.**
8. **Summarize *what* and explain *why*, not *how*.** The diff shows how; the body provides
   context a future reader won't be able to reconstruct from code alone:
   - What was the motivation / problem?
   - Why this approach over alternatives?
   - Any non-obvious consequences or side effects?
9.  Be concise, specific, and direct. In other words, follow the general principles of ASD-STE100 but don't slavishly follow the spec. 
10. **Use bullet points freely** (hyphen + space). Blank lines between bullets.
11. **Formatting references to code** — wrap class names, method names, and
    identifiers in backticks (e.g., `PreviewGenerationJob`). Wrap literal
    values in double-quotes (e.g., "storefront_generation_preview").
12. **Issue references** go at the bottom of the body, after a blank line.
    Use GitHub keywords: `Closes #<number>` or `Connects #<number>` (see Steps).

### What NOT to do

- Do NOT use Conventional Commits format (`feat:`, `fix:`, etc.) — just write plain English.
- Do NOT add `Signed-off-by` or other trailers unless the user asks.
- Do NOT push. Only commit.

## Steps

### 1. Understand what changed

Parse the user's prompt for:
- Specific **file paths or globs** → only stage/commit those files.
- A **specific commit SHA** → use `git show <sha>` instead of the working tree diff
  (for amending or rewriting a message).
- **Freeform instructions** → incorporate into subject/body.
- A **GitHub issue number or URL** → note it for step 3.

Then gather context:

- Run `git status` and `git diff --stat` to understand the change set
  (scoped to specified files if any).
- Run `git diff` (or `git diff --cached` if files are already staged) to read the
  actual changes. For a specified commit, use `git show <sha>`.
- **Read the changed files** (not just the diff) when the diff alone doesn't make
  the purpose clear. Understanding the surrounding code helps write a meaningful message.
- Optionally run `git log -n 20 --pretty=format:'%s'` to see recent message style
  for consistency.

### 2. Gather issue context

**Always prompt for a GitHub issue** if the user didn't provide one:

> _"Is there a GitHub issue associated with this change? (Enter an issue
> number, URL, or 'no' to skip.)"_

If an issue is provided (by the user up front or in response to the prompt):

- Fetch it with `gh issue view <number> --json title,body,labels` (or from the URL's repo).
- Read the issue title and body to understand the motivation and acceptance criteria.
- Use this context to inform the subject, body, and especially the *why*.
- Ask the user whether this commit **closes** or merely **connects to** the issue:

> _"Does this commit fully resolve #<number>, or is it partial progress?
> (closes / connects)"_

Then include the appropriate keyword at the bottom of the body:
- `Closes #<number>` — if the commit fully resolves the issue.
- `Connects #<number>` — if the commit is related but doesn't fully resolve it.

### 3. Assess the "why" — ask if uncertain

After reading the diff, the changed files, and any linked issue, decide whether the
**motivation** for the change is clear.

If the "why" is **clear** (e.g., the issue explains it, or it's obvious from context
like fixing a typo or removing dead code), proceed to compose the message.

If the "why" is **uncertain**, present the user with a short summary of *what* changed
and ask about *why*:

> _"Here's what I see this commit doing:_
>
> - _Added a retry loop around the webhook delivery call_
> - _Added a max-retries config option with a default of 3_
>
> _What's the motivation? (e.g., Were webhook deliveries failing intermittently?
> Is this for a reliability initiative?)"_

Incorporate the user's answer into the commit body.

### 4. Stage and open editor for review

- If there are unrelated unstaged files and the user didn't specify which to include,
  **ask before staging them**.
- Stage the intended files (`git add`).
- **Decide whether to use `--verbose`:** Run `git diff --cached --stat | tail -1` to get
  the summary line (e.g., "15 files changed, 200 insertions(+), 50 deletions(-)").
  Parse the total number of insertions + deletions. If under 10,000 changed lines,
  add `--verbose` (`-v`) to the commit command so the diff appears in the editor
  for review. If 10,000 or more, omit it — the diff would be too large to be useful.
- **Never bypass the editor.** Do NOT set `GIT_EDITOR=true`, `EDITOR=true`, or similar overrides that auto-accept the message. The user must be able to edit or abort.
- Write the composed message (subject, blank line, body) to a temporary file.
  - Run `git commit --edit --file=<tmpfile>` (with `-v` if the diff is small enough) so the user's editor opens with the pre-filled message.
  - **If you get an error about non-interactive/headless:** do **not** run `git commit` in bypass mode. Instead, print the exact command for the user to run in their own terminal:
    - `git commit --edit --file=<tmpfile> -v` (or without `-v` for large diffs)
  - **Do NOT use `git commit -m`.** The user must always get a chance to review and edit the message before the commit is created.

## Examples

### Subject-only (simple change, no issue)

```
Fix off-by-one error in pagination offset
```

### With issue — closes

```
Redirect user to the requested page after login

Users were being redirected to the home page after login, which
was frustrating when they had followed a deep link. Now we store
the originally requested URL in the session and redirect there
after successful authentication.

Closes #123
```

### With issue — connects (partial progress)

```
Add retry loop around webhook delivery

Webhook deliveries to merchant endpoints were failing
intermittently due to transient network errors, causing missed
notifications. This adds exponential backoff with a configurable
max-retries (default 3).

The failure alerting and dead-letter queue will follow in a
subsequent commit.

Connects #456
```

### Explaining why (no issue)

```
Remove the obsolete warehouse sync job

This job was added in 2019 to backfill data from the legacy
warehouse system. That system was fully decommissioned last month
and the job has been no-opping since. Removing it to reduce
confusion and CI time.
```
