if which keychain >/dev/null; then
    eval $(keychain --eval --agents ssh -Q --quiet id_rsa)
fi

if [[ "$TTY" == "/dev/tty1" ]]; then
    startx;
    logout;
fi
