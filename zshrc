#-ZSH LEVEL CHANGES------------------------------------------------------------
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

#-LOAD AND INITIALIZE ANTIGEN--------------------------------------------------
export WORKON_HOME=~/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2
source ~/.bin/antigen/antigen.zsh
antigen-use oh-my-zsh
antigen-bundle zsh-users/zsh-syntax-highlighting
antigen-bundle command-not-found
antigen-bundle pip
antigen-bundle virtualenvwrapper
antigen-theme bbenne10/antigen-themes themes/bbennett2
antigen-apply

#-ZLE / BINDKEY CHANGES--------------------------------------------------------
#(THESE ARE MAINLY FOR ST)
if [[ $TERM == "st-256color" ]]; then
    bindkey ${terminfo[khome]} beginning-of-line
    bindkey ${terminfo[kend]}  end-of-line
    bindkey ${terminfo[kich1]} overwrite-mode
    bindkey ${terminfo[kdch1]} delete-char
    bindkey ${terminfo[kcuu1]} up-line-or-history
    bindkey ${terminfo[kcud1]} down-line-or-history
    bindkey ${terminfo[kcub1]} backward-char
    bindkey ${terminfo[kcuf1]} forward-char

    function zle-line-init() {
        if [[ "$TERM" == "st-256color" ]]; then
            echoti smkx;
        fi;
    }

    function zle-line-finish() {
        if [[ "$TERM" == "st-256color" ]]; then
            echoti rmkx
        fi;
    }

    zle -N zle-line-init
    zle -N zle-line-finish

    # Set erase to bkspc
    stty erase 
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

#-ALIASES----------------------------------------------------------------------

if [[ $(uname -s) == "Linux" ]]; then
    # OSX's ls and rm don't have these options
    alias ls='ls --color=auto'
    alias rm="rm -Iv"
fi

alias rmr="rm -r"
alias mkdir='mkdir -p'
alias zipfldr='zip -9 -r'
alias grep='grep --color=ALWAYS'
alias gr='ag'
alias less=$PAGER

function sprunge() {
    cat $1 | curl -F 'sprunge=<-' http://sprunge.us
}

function ix() {
    cat $1 | curl -n -F 'f:1=<-' http://ix.io
}

function build_done() {
    notification="Build Completed"
    if [ -n "$1" ]; then
        notification=$1
    fi
    ~/.bin/pushover_notify {{PUSHOVER_BUILD_DONE_APP_KEY }} $notification >/dev/null
}

function push_bf() {
    rsync -a $HOME/code/black_forest/neo4j-schema-tests/ bf:/users/bbennett37/black_forest/
}

