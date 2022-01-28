# PATH setup
path=(/opt/homebrew/bin /opt/homebrew/sbin /usr/local/bin /usr/local/sbin $path)

# Get setup-specific configuration if it exists
if [[ -e ~/.zprofile-local ]]; then; source ~/.zprofile-local; fi;
