##################
# History Config #
##################
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zhistory

# Append to history file (don't over-write)
# Ignore a command if it duplicates the previous command
# Expire duplicate history lines first
# Don't store 'history' or 'fc' commands
# Don't store function definitions
setopt APPEND_HISTORY HIST_IGNORE_DUPS HIST_EXPIRE_DUPS_FIRST HIST_NO_STORE HIST_NO_FUNCTIONS


##################
# General Config #
##################
# Stops background tasks from being killed if you exit the shell
setopt NO_HUP

# If given a directory path with no command cd to the directory
setopt AUTO_CD

# If you mispell a command, ask if you meant a similar one
setopt CORRECT

# Make cd push the old directory onto the directory stack
setopt AUTO_PUSHD

# PATH setup
# This gets rid of the right-most duplicate entries
typeset -U path
path=('/usr/local/bin' $path)

# Function Path Setup
fpath=($HOME/.zsh/func $fpath)
typeset -U fpath

EDITOR="$(which emacs) -nw --no-desktop"


###############
# Completions #
###############
# Turn completions on
autoload -U compinit && compinit

# Set Completion Options
# Turn off beeping for ambiguous completions
setopt NO_LIST_BEEP
# Set completion to take place at the cursor position of a word
setopt COMPLETE_IN_WORD
# After completion, move the cursor to the end
setopt ALWAYS_TO_END
# Start menu selection if there are at least 4 possibilities for ambiguous completion
zstyle ':completion:*' menu select=4

# Tab completion for PID in kill command
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always


##########
# precmd #
##########
precmd () {
    vcs_info
}


###########
# Prompts #
###########
# We want prompts to have access to functions / themes and readable colors
autoload -U promptinit colors && promptinit && colors
# Allow for parameter expansion, command substitution and arithmetic expansion in the prompt
setopt PROMPT_SUBST
# Set my PS1 prompt
PROMPT='%B[%b%{${fg[blue]}%}${PWD/#$HOME/~}%B%{$reset_color%}%B]%b
${vcs_info_msg_0_} %(?..%{${fg[red]}%})%(!.#.>)%{$reset_color%} '
RPROMPT='%(2L,,[%n@%M])'


#############
# VCS Setup #
#############
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable bzr git hg svn
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' use-prompt-escapes true
# without colors:
# formats "[%s:%r(%b)%m%c%u]"
# actionformats "[%s:%r(%b)(%a)%m%c%u]"
zstyle ':vcs_info:*' formats "[%{$fg[cyan]%}%s%{$reset_color%}:%{$fg[magenta]%}%r%{$reset_color%}(%{$fg[yellow]%}%b%{$reset_color%})%m%{$fg[green]%}%c%{$fg[red]%}%u%{$reset_color%}]"
zstyle ':vcs_info:*' actionformats "[%{$fg[cyan]%}%s%{$reset_color%}:%{$fg[magenta]%}%r%{$reset_color%}(%{$fg[yellow]%}%b%{$reset_color%})(%{$fg[blue]%}%a%{$reset_color%})%m%{$fg[green]%}%c%{$fg[red]%}%u%{$reset_color%}]"
# Git Setup
zstyle ':vcs_info:git*' stagedstr   'M'
zstyle ':vcs_info:git*' unstagedstr 'M'


##########
# Open new tabs in same directory
#########
if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]]; then
  function chpwd {
    # Should have better percent-encoding then just space replacement
    printf '\e]7;%s\a' "file://$HOSTNAME${PWD// /%20}"
  }
  chpwd
fi


###########
# Aliases #
###########
alias ls='ls -G -h -p '
alias ll='ls -l -G -h -p '

# Get setup-specific configuration if it exists
if [[ -e ~/.zshlocal ]]; then; source ~/.zshlocal; fi;


#################
# ENV Variables #
#################
# Enable iex shell history (Elixir REPL)
export ERL_AFLAGS="-kernel shell_history enabled"

###############
# ZSH Plugins #
###############
source ~/.zshplugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=23'
#bindkey '^[^m' autosuggest-accept
bindkey '^[^m' autosuggest-execute
bindkey '^x' autosuggest-execute

source ~/.zshplugins/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

source /usr/local/opt/chruby/share/chruby/chruby.sh
#To enable auto-switching of Rubies specified by .ruby-version files
source /usr/local/opt/chruby/share/chruby/auto.sh
