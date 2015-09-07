local homefs=$(cat /proc/self/mounts | grep $HOME | cut -f 3 -d ' ')
# On NFS I lets put rbenv locally
if [ "$homefs" = "nfs" ]; then
  export RBENV_ROOT=/space/rbenv
  export PYENV_ROOT=/space/pyenv
fi

# if rbenv is present, configure it for use
if which rbenv &> /dev/null; then
    eval "$(rbenv init -)"
fi

if which pyenv &> /dev/null; then
    eval "$(pyenv init - --no-rehash zsh)"
fi

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

