if &diff || exists("vimpager")
    " We're running as 'vimpager' or 'vimdiff'
    set diffopt+=iwhite
    set noloadplugins
else

    "We're running as either "vim" or "gvim"

    " Make vim not work EXACTLY as vi did
    set nocompatible
    filetype plugin indent on

    " Always use utf-8
    set encoding=utf-8

    " use language's specification file to autoindent
    set autoindent

    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    " What plugins are we using?
    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    let g:simpleburn_transparent_term=1
    let g:airline_powerline_fonts = 1
    let g:syntastic_python_python_exec = "/usr/bin/python2.7"
    let g:syntastic_python_checkers = ['pylama']

    let g:jsx_ext_required = 0

    let g:user_emmet_leader_key='<c-z>'

    set rtp+=~/.vim/bundle/neobundle.vim
    call neobundle#begin(expand('~/.vim/bundle'))
    NeoBundleFetch 'Shougo/neobundle.vim'

    " 'interface' plugins
    NeoBundle 'bling/vim-airline'
    NeoBundle 'kien/ctrlp.vim'
    "NeoBundle 'Shougo/unite.vim'

    " 'utility' plugins
    NeoBundle 'bufkill.vim'
    NeoBundle 'airblade/vim-rooter'
    NeoBundle 'rking/ag.vim'
    NeoBundle 'tpope/vim-fugitive'
    NeoBundle 'ervandew/supertab'
    NeoBundle 'mattn/emmet-vim'

    " 'syntax' plugins
    NeoBundle 'scrooloose/syntastic'
    NeoBundle 'hynek/vim-python-pep8-indent'
    NeoBundle 'MarcWeber/vim-addon-mw-utils'
    NeoBundle 'tomtom/tlib_vim'
    NeoBundle 'garbas/vim-snipmate'
    NeoBundle "honza/vim-snippets"
    NeoBundle 'voithos/vim-python-matchit'
    NeoBundle 'rodjek/vim-puppet'
    NeoBundle 'jsx/jsx.vim'

    " colorschemes
    NeoBundle 'bbenne10/simpleburn'
    NeoBundle 'endel/vim-github-colorscheme'
    NeoBundle 'jeetsukumaran/vim-mochalatte'
    NeoBundle 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}

    call neobundle#end()
    NeoBundleCheck

    "Change filetype for HTML to htmldjango - this colors some additional syntax
    au BufNewFile,BufRead *.html set filetype=htmldjango

    "Supertab options.
    au FileType python set omnifunc=pythoncomplete#Complete
    let g:SuperTabDefaultCompletionType = 'context'
    set completeopt=menuone,longest

    " Set Rooter to run on sh files as well
    au BufEnter *.sh :Rooter

    " Add the virtualenv's site-packages to vim path
    py << EOF
import os.path
import sys
import vim
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    sys.path.insert(0, project_base_dir)
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF

    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    " How should tabs work?
    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    " how large is a existing \t character?
    set tabstop=4

    "When reindenting, how many spaces to shift?
    set shiftwidth=4

    "Round to nearest shiftwidth when using << or >>
    set shiftround

    "When you hit tab, how many characters will be inserted?
    set softtabstop=4

    "tell vim to insert spaces rather than \t characters
    set expandtab

    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    " How should searching/completion work?
    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    "Case insensitivity
    set ignorecase

    "Make case insensitivity 'smarter' (regex matching is stricter in some cases)
    set smartcase

    "Which files should we ignore for completions?
    set wildignore=*.pyc,*.bak,*.swp,*.so,*.zip,*.tgz,*.tar.*

    "Hilight search results
    set hlsearch

    "search incrementally
    set incsearch

    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    " How should navigation around the file work?
    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    "don't wrap long lines
    set nowrap

    "start scrolling 5 lines from the screen edge
    set scrolloff=5

    "start scrolling 5 columns from the screen edge
    set sidescrolloff=5

    "Fold the text based on it's indentation level (works well with python code)
    set foldmethod=indent

    "Allow obscuring un-saved buffers
    set hidden

    "How should the backspace be handled?
    "This is somewhat complex. Please google for what this does.
    set backspace=indent,eol,start

    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    " Misc small changes
    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    " change the leader key
    let mapleader = "\<Space>"

    "Don't add a space when joining
    set nojoinspaces

    "Turn off both backing up files and swap files - these are just annoying
    set nobackup
    set noswapfile

    "persistent undo
    set undodir=~/.vim/undo
    set undofile
    set undolevels=10000
    set undoreload=100000

    " shorten the required time between keypresses in some scenarios
    set timeout ttimeoutlen=50

    "decent clipboard
    "This adds integration to Xorg's 'Ctrl+V' type buffer
    set clipboard=unnamedplus

    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    " How should the UI look?
    " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    "color/syntax nicieties
    set background=dark
    syntax enable

    colorscheme Tomorrow-Night

    " Now modify the colorscheme like so...
    if g:colors_name == "github"
        highlight CursorLine          cterm=None
    elseif g:colors_name == "simpleburn"
        highlight ColorColumn         guibg=#2e2e2e
    endif

    "draw a bar at 80 characters
    set colorcolumn=80

    "show line numbers
    set number

    "hilight the line with the cursor
    set cursorline

    "Remove unnecessary GUI options when using gvim.
    "menubar
    set go-=m
    "toolbar
    set go-=T
    "right hand scrollbar
    set go-=r
    "Left hand scrollbar
    set go-=L
    "Use console rather than popups
    set go+=c

    "change gui font
    set guifont=TamzenForPowerlineMod\ 13

    "Always show the status line
    set laststatus=2

    "Fix the ugly vertical split thing
    set fillchars=vert:│

    "How should whitespace be shown when it is being shown?
    set listchars=trail:·,precedes:«,extends:»,eol:↲,tab:➝\

    "sane filename matching when searching the filesystem (:e and the like)
    set wildmenu
    set wildmode=longest:full,full

    "KEYBINDS / COMMAND MODE TWEAKS
    "==============================================================================
    "
    " Insert blank line before/after current line
    nnoremap <leader>[ :silent :call append(line('.')-1, '')<CR>
    nnoremap <leader>] :silent :call append(line('.'), '')<CR>

    "toggle hilighting search results. Keychain: <leader>+n
    nmap <silent> <leader>n :silent :set hls!<CR>

    "Toggle showing whitespace. Keychain: <leader>+s
    nmap <silent> <leader>s :set nolist!<CR>

    "when pressing / to initiate search, turn on search hilighting
    nnoremap / :set hlsearch<CR>/

    "When saving, make :W work like :w
    cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))

    "Turn off the help system's keybind (and bind it to toggle paste mode)
    nnoremap <F1> :set paste!<CR>

    "Convert markdown to html. This doesn't work in the reverse, but you may
    "simply undo the change to 'preview' the changes before saving.
    "Keychain: <leader>+m
    function! PreviewMarkdown()
    silent update
    let output_name = '/tmp/' . expand('%:t') . '.html'

    let file_header = ['<html>', '<head>',
        \ '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">',
        \ '<title>'.expand('%:p').'</title>',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssreset/reset-min.css">',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssbase/base-min.css">',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssfonts/fonts-min.css">',
        \ '<style>body{padding:20px;}',
        \ '    div#container{background-color:#F2F2F2;padding:0 20px;margin:0 auto;border:solid #D0D0D0 1px;width: 90%; max-width: 80em}',
        \ '    .section1, .section2{border-bottom: 1px solid #D0D0D0;}',
        \ '    .section2{margin-left: 1em;} .section3{ margin-left: 2em;}',
        \ '    p { margin-left: 2em; }</style>',
        \ '</head>', '<body>', '<div id="container">']

    call writefile(file_header, output_name)

    silent exec '!python -m markdown -o html4 -x tables -x outline ' . expand('%:p') . ' >> ' . output_name
    silent exec '!echo "</div></body></html>" >> "' . output_name . '"'
    silent exec '!firefox "' . output_name . '" &'
    silent exec '!redraw'
    endfunction

    nmap <leader>m :call PreviewMarkdown()<CR>
    nmap <leader>M :!python -m markdown -o html4 -x tables -x outline<cr>

    "Run line under cursor in user's shell (or in python if it's a py "script)
    autocmd BufRead *.sh nmap <buffer> <leader>r :.w !$SHELL<CR>
    autocmd BufRead *.py nmap <buffer> <leader>r :.w !python2 %<CR>

    "Strip trailing whitespace (and save cursor position) when saving files
    fun! <SID>StripTrailingWhitespaces()
        let l = line(".")
        let c = col(".")
        %s/\s\+$//e
        call cursor(l, c)
    endfun
    autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

    "Allow us to launch Rainbow (to inspect colors)
    nnoremap <leader>R :silent !rainbow < % &<cr>

    "Search buffers with <leader>w
    nnoremap <leader>w :CtrlPBuffer<cr>

    "<leader>bd to delete a window
    nnoremap <leader>bd :BD<CR>
endif
