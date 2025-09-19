#!/bin/bash
exists()
{
    command -v "$1" >/dev/null 2>&1
}

HISTCONTROL=ignoredups:ignorespace

for dir in $HOME/.local/bin $HOME/.config/emacs/bin $HOME/go/bin $HOME/.npm-global/bin $HOME/.nix-profile/bin; do
    test -d "$dir" && export PATH=$dir:$PATH
done

emacscmd=$(which emacs)
if [ -x "$emacscmd" ]; then
    export EDITOR="$emacscmd -nw"
fi

alias df="df -h"
alias ls="ls --color=auto"

if [[ "$XDG_CURRENT_DESKTOP" =~ .*KDE.* ]]; then
    for f in $HOME/.config/plasma-workspace/env/*; do
        test -f "$f" && source "$f"
    done
fi

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"

# avoid enabling starship in emacs terminal unless in eat
if exists starship && [[ ( -z "$INSIDE_EMACS" || "$INSIDE_EMACS" =~ .*eat.* ) && -z "$CURSOR_TRACE_ID" ]]; then
    eval "$(starship init bash)"
fi

if exists mise; then
    eval "$(mise activate bash)"
    eval "$(mise activate bash --shims)"
fi

export NPM_CONFIG_PREFIX="$HOME/.npm-global"

