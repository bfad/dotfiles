#!/bin/zsh

cd "$(pwd)/$(dirname ${0})"
typeset -r from_path="$(pwd)"

typeset -r script_name="$(basename ${0})"


# Iterate through all the top-level files/dirs tracked by git and create
# symlinks to the home directory as long as the file isn't this installer file
for file in $(git ls-tree --name-only master)
do
    if [[ ! ($file == $script_name) ]]; then
        ln -s "$from_path/$file" "$HOME/"
    fi
done
