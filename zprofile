PATH=~/.bin:$PATH
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
    PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
    eval $(keychain --eval id_ed25519 apiary gtri)
fi
export PATH


if [[ "$TTY" == "/dev/tty1" ]]; then
  startx;
  logout;
fi
