#!/bin/bash

exists()
{
    command -v "$1" >/dev/null 2>&1
}

HISTCONTROL=ignoredups:ignorespace

for dir in $HOME/.config/emacs/bin $HOME/go/bin $HOME/.npm-global/bin; do
    test -d "$dir" && export PATH=$dir:$PATH
done

emacscmd=$(which emacs)
if [ -x "$emacscmd" ]; then
    export EDITOR="$emacscmd -nw"
fi

alias df="df -h"
alias ls="ls --color=auto"

if exists direnv; then
    eval "$(direnv hook bash)"
fi

if exists starship; then
    eval "$(starship init bash)"
fi
