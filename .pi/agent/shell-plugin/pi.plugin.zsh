#!/usr/bin/env zsh
# pi.plugin.zsh — colon-command integration and mypi function for pi
#
# Intercepts lines starting with ":" at the ZLE level, passing the raw text
# to mypi without shell expansion.  No escaping needed.
#
# Syntax:
#   : message                  code profile, resume session
#   :profile message           specific profile, resume session
#   :new message               code profile, new session
#   :profile-new message       specific profile, new session
#   :                          code profile, interactive (resume)
#   :profile                   specific profile, interactive
#
# Direct usage:
#   mypi [--new] [profile] [pi-args...]
#
# Multiline input:
#   Shift+Enter inserts a newline when the buffer starts with ":".
#   Works natively in Ghostty, Kitty, WezTerm.
#   For iTerm2, add a key mapping: Shift+Enter → Send Escape Sequence → [13;2u
#   For Apple Terminal: any key → Send text → \033[27;2;13~
#   Pasting multi-line text also works (via ZLE bracketed paste).
#
# All modes use Pi's normal resource discovery and settings.
# code (default) and base use the current directory; notes adds its vault
# directory and system prompt. Legacy names/session paths stay compatible.

# ── Per-shell session identity ─────────────────────────────────────────────
# Fresh UUID for every interactive shell.  Generated unconditionally so that
# Zellij/tmux panes, nested shells, etc. each get their own session.
export PI_SHELL_SESSION="$(uuidgen)"

# ── Knobs ──────────────────────────────────────────────────────────────────
typeset -ga PI_KNOWN_PROFILES=(code notes base)
typeset -ga PI_KNOWN_COMMANDS=(new)

# ── mypi — session-aware pi launcher with a notes shortcut ─────────────────
# Defined as a shell function so --new can rotate PI_SHELL_SESSION in the
# current shell.  An external script (child process) cannot do this.
#
# Session path: ~/.pi/agent/sessions/<cwd-encoded>/<profile>-<uuid>.jsonl
# Pi creates the file on first run and resumes it on subsequent runs.
function mypi() {
    local PI_AGENT_DIR="$HOME/.pi/agent"

    # ── Parse --new ────────────────────────────────────────────────────────
    if [[ "${1:-}" == "--new" ]]; then
        export PI_SHELL_SESSION="$(uuidgen)"
        shift
    fi

    # ── Help ───────────────────────────────────────────────────────────────
    if [[ "${1:-}" == "help" || "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
        echo "Usage: mypi [--new] [profile] [pi args...]"
        echo ""
        echo "Modes (all use your normal Pi configuration):"
        echo "  code   (default) Current directory"
        echo "  notes  Obsidian PKM — vault working dir, notes system prompt"
        echo "  base   Same configuration as code, keeps its legacy session slot"
        echo ""
        echo "Flags:"
        echo "  --new  Start a fresh session"
        echo ""
        echo "Examples:"
        echo "  mypi                          # Code profile, resume session"
        echo "  mypi code                     # Same as above"
        echo "  mypi notes                    # Notes profile"
        echo "  mypi --new code               # Fresh code session"
        echo "  mypi code \"fix the tests\"     # Code profile with initial prompt"
        return 0
    fi

    # ── Profile selection (default: code) ──────────────────────────────────
    local profile="code"
    case "${1:-}" in
        code|notes|base) profile="$1"; shift ;;
    esac

    # ── Notes context (resources still come from normal Pi discovery) ──────
    local effective_cwd="$PWD"
    local notes_prompt="$PI_AGENT_DIR/profiles/notes/system-prompt.md"
    if [[ "$profile" == "notes" ]]; then
        effective_cwd="$HOME/Documents/Knowledge Base"
        if [[ ! -d "$effective_cwd" ]]; then
            print -u2 -r -- "mypi: notes vault not found: $effective_cwd"
            return 1
        fi
        # Pi treats a missing prompt path as literal text, so fail explicitly.
        if [[ ! -f "$notes_prompt" || ! -r "$notes_prompt" || ! -s "$notes_prompt" ]]; then
            print -u2 -r -- "mypi: notes prompt must be a readable, non-empty file: $notes_prompt"
            return 1
        fi
    fi

    # ── Deterministic session path ─────────────────────────────────────────
    local -a pi_args=()
    if [[ -n "${PI_SHELL_SESSION:-}" ]]; then
        local cwd_encoded=$(echo "$effective_cwd" | sed 's|^/||; s|/|-|g; s|^|--|; s|$|--|')
        local session_dir="${PI_AGENT_DIR}/sessions/${cwd_encoded}"
        local session_file="${session_dir}/${profile}-${PI_SHELL_SESSION}.jsonl"

        mkdir -p "$session_dir" || return 1
        pi_args+=(--session "$session_file")
    fi

    # ── Launch ─────────────────────────────────────────────────────────────
    case "$profile" in
        notes)
            (
                cd -- "$effective_cwd" || return 1
                pi --append-system-prompt "$notes_prompt" "${pi_args[@]}" "$@"
            )
            ;;
        code|base)
            pi "${pi_args[@]}" "$@"
            ;;
    esac
}

# ── ZLE widget: accept-line override ───────────────────────────────────────
function pi-accept-line() {
    # Non-colon lines → normal shell behaviour
    if [[ ! "$BUFFER" =~ "^:" ]]; then
        zle accept-line
        return
    fi

    local original="$BUFFER"
    local profile="code"
    local new_session=false
    local message=""

    # Everything after the leading ":"
    local raw="${BUFFER#:}"

    if [[ "$raw" == "" ]]; then
        # Bare ":" → defaults (code, interactive)
        :
    elif [[ "$raw" == [[:space:]]* ]]; then
        # ": message…" — whitespace after colon, rest is message
        message="${raw#[[:space:]]}"
    else
        # ":token…" — first word might be a profile, command, or profile-command
        if [[ "$raw" =~ "^([a-zA-Z][a-zA-Z0-9_-]*)" ]]; then
            local token="${match[1]}"
            local remainder="${raw#${token}}"

            # Strip one leading whitespace char between token and message
            [[ "$remainder" == [[:space:]]* ]] && remainder="${remainder#?}"

            local parsed=false

            # Try "profile-command" (e.g. notes-new)
            if [[ "$token" == *-* ]]; then
                local head="${token%%-*}"
                local tail="${token#*-}"
                if (( ${PI_KNOWN_PROFILES[(Ie)$head]} )) && \
                   (( ${PI_KNOWN_COMMANDS[(Ie)$tail]} )); then
                    profile="$head"
                    [[ "$tail" == "new" ]] && new_session=true
                    message="$remainder"
                    parsed=true
                fi
            fi

            if ! $parsed; then
                if (( ${PI_KNOWN_PROFILES[(Ie)$token]} )); then
                    profile="$token"
                    message="$remainder"
                elif (( ${PI_KNOWN_COMMANDS[(Ie)$token]} )); then
                    [[ "$token" == "new" ]] && new_session=true
                    message="$remainder"
                else
                    # Unknown token → everything after ":" is the message
                    message="$raw"
                fi
            fi
        else
            # First char is non-alpha → whole thing is message
            message="$raw"
        fi
    fi

    # ── History ────────────────────────────────────────────────────────────
    print -s -- "$original"

    # Keep typed line visible until command output starts
    CURSOR=${#BUFFER}
    zle redisplay

    # ── Build command ──────────────────────────────────────────────────────
    local -a cmd=(mypi)
    $new_session && cmd+=(--new)
    cmd+=("$profile")
    [[ -n "$message" ]] && cmd+=("$message")

    # ── Execute ────────────────────────────────────────────────────────────
    # Redirect stdin/stdout/stderr to the real terminal — ZLE replaces them
    # with its own pipes, which breaks interactive/TUI programs.
    echo
    "${cmd[@]}" </dev/tty >/dev/tty 2>/dev/tty

    # ── Reset prompt ───────────────────────────────────────────────────────
    BUFFER=""
    CURSOR=0
    zle -I
    zle reset-prompt
}

# ── ZLE widget: Shift+Enter newline ────────────────────────────────────────
# In ":" mode → insert a literal newline (multiline input).
# Otherwise   → same as Enter (accept line).
function pi-shift-enter() {
    if [[ "$BUFFER" =~ "^:" ]]; then
        LBUFFER+=$'\n'
    else
        zle accept-line
    fi
}

# ── Register and bind ──────────────────────────────────────────────────────
zle -N pi-accept-line
zle -N pi-shift-enter

bindkey '^M'       pi-accept-line    # Enter
bindkey '^J'       pi-accept-line    # Enter (alternate)
bindkey '\e[27;2;13~' pi-shift-enter  # Shift+Enter (xterm modifyOtherKeys — Ghostty)
bindkey '\e[13;2u'    pi-shift-enter  # Shift+Enter (kitty keyboard protocol — Kitty, WezTerm)
