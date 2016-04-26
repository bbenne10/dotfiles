if [ "$PWD" != "$HOME/.dotfiles" ]; then
  printf "==> Dotfiles must be cloned into ~/.dotfiles; exiting...\n"
  exit
fi

if [ ! -d "$HOME/.fresh" ]; then
  printf "==> Installing Fresh\n" 
  git clone https://github.com/freshshell/fresh ~/.fresh/source/freshshell/fresh > /dev/null
fi

if [ ! -r "$HOME/.freshrc" ]; then
  printf "==> Linking freshrc\n"
  ln -s $PWD/freshrc "$HOME/.freshrc"
fi

"$HOME"/.fresh/source/freshshell/fresh/bin/fresh
