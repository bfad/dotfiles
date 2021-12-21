# Conditionals for M1 versions of Homebrew vs. Intel
if [ -f /opt/homebrew/opt/chruby/share/chruby/chruby.sh ]; then
    source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
    source /opt/homebrew/opt/chruby/share/chruby/auto.sh
elif [ -f /usr/local/opt/chruby/share/chruby/chruby.sh ]; then
    source /usr/local/opt/chruby/share/chruby/chruby.sh
    source /usr/local/opt/chruby/share/chruby/auto.sh
fi

# PATH setup
path=(/usr/local/bin /usr/local/sbin $path)

# Function Path Setup
fpath=($HOME/.zsh/func $fpath)

####
# Homebrew setup
###
path=(/opt/homebrew/bin /opt/homebrew/sbin $path)
manpath=(/opt/homebrew/share/man $manpath)
infopath=(/opt/homebrew/share/info $infopath)
HOMEBREW_PREFIX="/opt/homebrew"
HOMEBREW_CELLAR="/opt/homebrew/Cellar"
HOMEBREW_REPOSITORY="/opt/homebrew"


#EDITOR="$(which emacs) -nw --no-desktop"
EDITOR="subl -w"

# Get setup-specific configuration if it exists
if [[ -e ~/.zshenv-local ]]; then; source ~/.zshenv-local; fi;

# This gets rid of the right-most duplicate entries
typeset -U path
typeset -U fpath
typeset -U manpath
typeset -U infopath
