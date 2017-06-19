. ~/.zaliases

autoload colors zsh/terminfo
autoload -U compinit promptinit
compinit
promptinit

setopt appendhistory
setopt sharehistory
setopt autocd
setopt correct
setopt noclobber
setopt extendedglob

# make cd not select parent dir and ignore some files during completion
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# ignore *.pyc and *~ files for completion for everything but rm and ls
zstyle ':completion:*:(all-|)files' ignored-patterns "(*.pyc|*~)"
zstyle ':completion:*:ls:*:(all-|)files' ignored-patterns
zstyle ':completion:*:rm:*:(all-|)files' ignored-patterns

#-LOAD AND INITIALIZE ZGEN ----------------------------------------------------
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
  zgen load zsh-users/zsh-syntax-highlighting
  zgen load Tarrasch/zsh-autoenv
  zgen oh-my-zsh plugins/command-not-found
  zgen oh-my-zsh plugins/httpie
  zgen oh-my-zsh plugins/pyenv
  zgen oh-my-zsh plugins/supervisor
  zgen oh-my-zsh plugins/vagrant

  zgen load bbenne10/antigen-themes themes/bbennett2
  zgen save
fi

#-ZLE / BINDKEY CHANGES--------------------------------------------------------
if [[ $TERM == "st-256color" ]]; then
    function zle-line-init() {
        echoti smkx;
    }

    function zle-line-finish() {
        echoti rmkx
    }

    zle -N zle-line-init
    zle -N zle-line-finish
fi

#-SANE VIM HISTORY SEARCHING---------------------------------------------------
bindkey -M vicmd '/' history-incremental-pattern-search-backward
bindkey -M vicmd '?' history-incremental-pattern-search-forward
bindkey -M viins '^R' history-incremental-pattern-search-backward
bindkey -M viins '^S' history-incremental-pattern-search-forward

#-CUSTOM COMPLETION------------------------------------------------------------
compctl -g '*.tar.gz *.tgz *.tar.bz2 *.bz2 *.tar *.tbz2 *.tbz *.gz *.rar *.zip *.7z *.lzo *.xz *.txz *.lzma *.tlz *.Z' + -g '*(-/)' ad al
compctl -g '*.mp3 *.ogg *.mod *.wav *.avi *.mpg *.mpeg *.wmv' + -g '*(-/)' mplayer
compctl -g '*.py' python
compctl -g '*(-/)' mkdir

#-COMMAND PARAMETER COMPLETION-------------------------------------------------
compctl -z fg
compctl -j kill
compctl -j disown
compctl -u chown
compctl -u su
compctl -c sudo
compctl -c which
compctl -c type
compctl -c hash
compctl -c unhash
compctl -o setopt
compctl -o unsetopt
compctl -a alias
compctl -a unalias
compctl -A shift
compctl -v export
compctl -v unset
compctl -v echo
compctl -b bindkey

#-"USER MODE" SHELL CUSTOMIZATION----------------------------------------------
if [ -f $HOME/.dircolors ]; then
    eval $(dircolors ~/.dircolors);
fi;
