export LANG=en_US.utf8

export EDITOR=vim

export HISTSIZE=2000
export SAVEHIST=$HISTSIZE
export HISTFILE='/home/bryan/.history'

export PATH=/home/bryan/.bin:/opt/java/jre/bin:$PATH:/sbin:/usr/sbin:/home/bryan/.gem/ruby/2.1.0/bin/
export PYTHONSTARTUP="$HOME/.pythonrc" # must be $HOME. ~ isn't expanded

if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

which keychain >/dev/null
if [ $? = 0 ]; then
    eval $(keychain --eval --agents ssh -Q --quiet id_rsa)
fi

if [[ "$TTY" == "/dev/tty1" ]]; then
    startx;
    logout;
fi
