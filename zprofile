export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export JAVA_HOME="/usr/lib/jvm/java-7-openjdk/"
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

export CDDA_DEVICE=/dev/sr0

export EDITOR=vim
export BROWSER=vimb

export HISTSIZE=2000
export HISTFILE='/home/bryan/.history'
export SAVEHIST=$HISTSIZE
export OOO_FORCE_DESKTOP=gnome

export PAGER=vimpager
export PANEL_FIFO=/home/bryan/.config/bspwm/panel_fifo

export PATH=/home/bryan/.bin:/opt/java/jre/bin:$PATH:/sbin:/usr/sbin:/home/bryan/.gem/ruby/2.1.0/bin/

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
