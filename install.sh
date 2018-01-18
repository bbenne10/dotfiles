#!/bin/sh

BIN_DIR="$HOME/.bin"
BUILD_DIR="../"  # assume we want builds/ alongside our dotfiles repo

SRANDRD_URI="git@github.com:bbenne10/srandrd.git"
XINPUTD_URI="git@github.com:bbenne10/xinputd.git"
DWM_URI="git@github.com:bbenne10/dwm.git"
DWMSTATUS_URI="git@github.com:bbenne10/dwmstatus.git"
XSS_URI="git@github.com:9wm/xss.git"

error () {
  printf "%s\n" "$*" >&2;
}

install_generic () {
  uri="$1"
  target="$2"

  printf "Installing %s\n" "$target"

  git clone "$uri" "$target"
  cd "$target"
    if make; then
        cp "$target" "$BIN_DIR"
    else
      error "Failed to build $target."
    fi
  cd -
}

install_srandrd () {
  install_generic "$SRANDRD_URI" srandrd
}

install_xinputd () {
  install_generic "$XINPUTD_URI" xinputd
}

install_xss () {
  install_generic "$XSS_URI" xss
}

install_dwm () {
  install_generic "$DWM_URI" dwm
}

install_dwmstatus () {
  install_generic "$DWMSTATUS_URI" dwmstatus
}

tangle_dotfiles () {
  find . -name "*.org" -exec emacs --batch -l org --eval "(org-babel-tangle-file \"{}\")" \; 2>&1 | grep "^Tangled"
}

main () {
  test -d "$BIN_DIR" || mkdir -p "$BIN_DIR"
  test -d "$BUILD_DIR" || mkdir -p "$BUILD_DIR"
  start_dir="$PWD"

  if [ echo "$OSTYPE" | grep -vq "darwin" ]; then
    # If we're not on osx
    cd "$BUILD_DIR"
      install_srandrd
      install_xinputd
      install_xss
      install_dwm
      install_dwmstatus
    cd "$start_dir"

    if [ -e $HOME/.xinitrc ]; then
      rm $HOME/.xinitrc
    fi

    ln -s $HOME/.xsession $HOME/.xinitrc
  fi

  tangle_dotfiles

}

main
