local homefs=$(cat /proc/self/mounts | grep $HOME | cut -f 3 -d ' ')
# On NFS I lets put rbenv locally
if [ "$homefs" = "nfs" ]; then
  export RBENV_ROOT=/space/rbenv
fi

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
alias iosc="osc -A https://api.suse.de"
alias pry="rbenv exec pry"
alias irb="rbenv exec pry"

# -n in gnome3 sends the window to the background
# why?
export TERM=xterm-256color

#export VISUAL="emacs-nox"
export EDITOR="vim"
export VISUAL="vim"

alias e="${EDITOR}"
#alias vi="emacs-nox"
