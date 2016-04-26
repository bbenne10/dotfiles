echo "==> Installing packages with Pacman"
sudo pacman -Syu --noconfirm \
    bitlbee \
    dunst \
    emacs \
    feh \
    firefox \
    fsniper \
    git \
    htop \
    jq \
    mopidy \
    mpv \
    msmtp \
    mutt \
    ncmpcpp \
    notmuch \
    python2 \
    python3 \
    the_silver_searcher \
    tmux \
    unrar \
    unzip \
    weechat \
    xorg-server \
    xorg-server-utils \
    xorg-xinit \
    xorg-xkill \
    xorg-xprop \
    zathura \
    zathura-djvu \
    zathura-pdf-poppler \
    zip \
    zsh 

touch ~/.fresh/build/did_install
