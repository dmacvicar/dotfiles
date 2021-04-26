#!/bin/bash

HISTCONTROL=ignoredups:ignorespace

for dir in $HOME/.config/emacs/bin; do
    test -d "$dir" && export PATH=$dir:$PATH
done

emacscmd=$(which emacs)
if [ -x "$emacscmd" ]; then
    export EDITOR="$emacscmd -nw"
fi

alias df="df -h"
