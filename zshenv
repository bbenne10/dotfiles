# if you're here looking for $PATH, you're in the wrong spot. Check ~/.zprofile
# (long story short: Arch devs won't properly support ~/.zshenv)

export LANG=en_US.utf8
export EDITOR='emacsclient -t'

export _Z_DATA="$HOME/.config/z"

export HISTSIZE=2000
export SAVEHIST=$HISTSIZE

export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2

if [ -f ${HOME}/.termcap ]; then
  export TERMCAP=$(< ${HOME}/.termcap)
fi

export WORKON_HOME=~/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2

# Colored man support
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;38;5;74m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[38;5;246m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;146m'
