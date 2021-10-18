#!/bin/zsh

bundle exec rubocop $@

# For some reason, when this runs directly in the linter the .zshenv file isn't
# sourced, but it is when telling it to run this shell script.
# source /usr/local/opt/chruby/share/chruby/chruby.sh \
#     && source /usr/local/opt/chruby/share/chruby/auto.sh \
#     && bundle exec rubocop $@
