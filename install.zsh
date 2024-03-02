#!/bin/zsh

cd "$(pwd)/$(dirname ${0})"
typeset -r from_path="$(pwd)"

typeset -r script_name="$(basename ${0})"

# Other installed tools may also use the `.config` folder, so just symlink the contents.
mkdir -p ~/.config
ln -sf "$from_path/.config/"* ~/.config/

# Only link the sublime folder if the file folder exists
if [ -d "$HOME/Library/Application Support/Sublime Text" ]; then
    ln -sf "$from_path/sublime" "$HOME/Library/Application Support/Sublime Text/User"
fi

# Symlink each file
ln -sf "$from_path/.emacs.d" ~/
# I haven't used tmux in a while, and I think some of the config options no longer apply.
# ln -sf "$from_path/.tmux.conf" ~/
ln -sf "$from_path/.vim" ~/
ln -sf "$from_path/.vim_tmp" ~/
ln -sf "$from_path/.vimrc" ~/
ln -sf "$from_path/.zprofile" ~/
ln -sf "$from_path/.zshenv" ~/
ln -sf "$from_path/.zshplugins" ~/
ln -sf "$from_path/.zshrc" ~/

# Older implementation that might be a good idea to somehow revive?
# # Iterate through all the top-level files/dirs tracked by git and create
# # symlinks to the home directory as long as the file isn't this installer file
# for file in $(git ls-tree --name-only master)
# do
#     if [[ ! ($file == $script_name) ]]; then
#         ln -s "$from_path/$file" "$HOME/"
#     fi
# done
