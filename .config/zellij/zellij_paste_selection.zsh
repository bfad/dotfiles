#!/bin/zsh

/usr/local/bin/zellij -s "$KMVAR_ZellijSession" action write-chars "$(cat /tmp/zellij_selection)"

