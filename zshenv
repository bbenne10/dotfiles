export LANG=en_US.utf8
export EDITOR=vim

export HISTSIZE=2000
export SAVEHIST=$HISTSIZE

export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2

export PYENV_ROOT="$HOME/.pyenv"

PATH=$PYENV_ROOT/bin:$HOME/.bin:/bin:/usr/bin:/opt/bin:/sbin:/usr/sbin
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
    PATH=$PATH:$HOME/.gem/ruby/2.1.0/bin
fi
export PATH

if [ -f ${HOME}/.termcap ]; then
  export TERMCAP=$(< ${HOME}/.termcap)
fi


