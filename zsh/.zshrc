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
alias e="emacs-nox"
alias vi="emacs-nox"

export EDITOR="emacs-nox"
export VISUAL="emacs-nox"


