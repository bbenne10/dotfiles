if which mopidy >/dev/null; then
  mopidy --config ~/.config/mopidy>!/tmp/mopidy_bryan.log&
fi

PATH=~/.bin:$PATH
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
    PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
fi
export PATH

if [[ "$TTY" == "/dev/tty1" ]]; then
  startx;
  logout;
fi
