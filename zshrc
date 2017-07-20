. ~/.zaliases
if [ ! -f ~/.bin/zgen/zgen.zsh ]; then
  pushd ~/.bin
  git clone http://github.com/tarjoilija/zgen
  popd
fi

source ~/.bin/zgen/zgen.zsh

if [ ! -d ~/.pyenv ]; then
  git clone http://github.com/yyuu/pyenv ~/.pyenv
  pushd ~/.pyenv/plugins
  git clone http://github.com/yyuu/pyenv-virtualenv ~/.pyenv/plugins/virtualenv
  popd
fi

if ! zgen saved; then
  echo "Creating a zgen save..."
  zgen oh-my-zsh
  zgen load unixorn/autoupdate-zgen
  zgen load djui/alias-tips
  zgen load zdharma/fast-syntax-highlighting
  zgen load Tarrasch/zsh-autoenv
  zgen load chrissicool/zsh-256color
  zgen load zsh-users/zsh-completions src
  zgen load zsh-users/zsh-autosuggestions
  zgen load uvaes/fzf-marks

  zgen oh-my-zsh plugins/command-not-found
  zgen oh-my-zsh plugins/httpie
  zgen oh-my-zsh plugins/pyenv
  zgen oh-my-zsh plugins/supervisor
  zgen oh-my-zsh plugins/vagrant

  zgen load bbenne10/antigen-themes themes/bbennett2
  zgen save
fi

bindkey '^ ' autosuggest-accept
