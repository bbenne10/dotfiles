PATH=~/.bin:$PATH
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
    PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
fi
export PATH

if [[ "$TTY" == "/dev/tty1" ]]; then
  startx;
  logout;
fi
