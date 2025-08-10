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

if exists direnv; then
    eval "$(direnv hook bash)"
fi

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


export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - bash)"

export PATH="$HOME/.tfenv/bin:$PATH"
export TFENV_INSTALL_DIR="~/.tfenv"
export NPM_CONFIG_PREFIX="$HOME/.npm-global"

