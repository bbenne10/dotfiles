" NEOBUNDLE SETUP
" This adds all specified plugins to vim, but needs some setting changed first
" =============================================================================
" Add some modern nicieties to VIM (i.e make it not work EXACTLY like vi)
set nocompatible
set encoding=utf-8
"load plugins for specific filetypes
filetype plugin on

"load indent rules for specific filetypes
filetype indent on

"These are needed here, since the plugins read them on startup
let g:simpleburn_transparent_term=1
let g:airline_powerline_fonts = 1
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_auto_colors = 0

set rtp+=~/.vim/bundle/neobundle.vim
call neobundle#begin(expand('~/.vim/bundle'))
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'bufkill.vim'
NeoBundle 'voithos/vim-python-matchit'
NeoBundle 'bling/vim-airline'

NeoBundle 'klen/python-mode', {'branch': 'develop'}
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'airblade/vim-rooter'
NeoBundle 'rodjek/vim-puppet'
NeoBundle 'rking/ag.vim'

" And now some colorschemes I like
NeoBundle 'bbenne10/simpleburn'
NeoBundle 'endel/vim-github-colorscheme'
NeoBundle 'jeetsukumaran/vim-mochalatte'
NeoBundle 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}


call neobundle#end()
NeoBundleCheck

set timeout ttimeoutlen=50

"color/syntax nicieties
set background=dark
syntax enable

colorscheme Tomorrow-Night

" Now modify the colorscheme like so...
if g:colors_name == "github"
    highlight CursorLine          cterm=None 
elseif g:colors_name == "zenburn"
    highlight ColorColumn         guibg=#2e2e2e
    highlight IndentGuidesOdd     guibg=#303030 ctermbg=Gray
    highlight IndentGuidesEven    guibg=#333333 ctermbg=DarkGrey
endif

"decent clipboard
"This adds integration to Xorg's 'Ctrl+V' type buffer
set clipboard=unnamedplus

"SEARCH TWEAKS
"=============================================================================
"Case insensitivity
set ignorecase

"Make case insensitivity 'smarter' (regex matching is stricter in some cases)
set smartcase

"TAB CHARACTER SETTINGS
"==============================================================================
"use language's specification file to autoindent
set autoindent

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

"MISC
"==============================================================================
"draw a bar at 80 characters
set colorcolumn=80

"show line numbers relative to the current line
set number
set relativenumber

"hilight the line with the cursor
set cursorline

"don't wrap long lines 
set nowrap

"start scrolling 5 lines from the screen edge
set scrolloff=5

"start scrolling 5 columns from the screen edge
set sidescrolloff=5

"Allow obscuring un-saved buffers
set hidden

" change the leader key
let mapleader = ","

"Turn off both backing up files and swap files - these are just annoying
set nobackup
set noswapfile

"Which files should we ignore for completions?
set wildignore=*.pyc,*.bak,*.swp,*.so,*.zip,*.tgz,*.tar.*

"Hilight search results
set hlsearch

"search incrementally
set incsearch

"Fold the text based on it's indentation level (works well with python code)
set foldmethod=indent

"How should whitespace be shown when it is being shown?
set listchars=trail:·,precedes:«,extends:»,eol:↲,tab:➜\ 

"How should the backspace be handled?
"This is somewhat complex. Please google for what this does. 
set backspace=indent,eol,start

"sane filename matching when searching the filesystem (:e and the like)
set wildmenu
set wildmode=longest:full,full

"persistent undo
set undodir=~/.vim/undo
set undofile
set undolevels=10000
set undoreload=100000

"Change filetype for HTML to htmldjango - this colors some additional syntax
au BufNewFile,BufRead *.html set filetype=htmldjango

"UI TWEAKS
"==============================================================================

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

"change font
set guifont=Meslo\ LG\ M\ for\ Powerline\ 9

"Always show the status line
set laststatus=2

"Fix the ugly vertical split thing
set fillchars=vert:│

"PLUGIN TWEAKS
"==============================================================================

"Plugin specific variables. These control how colorschemes / plugins behave.
let g:pymode_lint_on_fly=1
let g:pymode_rope_lookup_project=0
let g:pymode_doc=0
let g:pymode_folding=1

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

"Run line under cursor in user's shell
nmap <leader>r :.w !$SHELL<CR>

"Allow us to launch Rainbow (to inspect colors) 
nnoremap <leader>R :silent !rainbow < % &<cr>

"Map to compile less files to css
nnoremap <leader>l :w <BAR> !lessc % > %:t:r.css<CR><space>

"Search buffers with <leader>w
nnoremap <leader>w :CtrlPBuffer<cr>

"<leader>bd to delete a window
nnoremap <leader>bd :BD<CR>


"HOW WERE WE CALLED?
"==============================================================================
if &diff || exists("vimpager")
    "If we've made it here, we've run vim as 'vimdiff' or 'vimpager'. This means
    "we're not going to load plugins and we're going to set some specific
    "vim options that tell vim to ignore white space in diffs.
    set diffopt+=iwhite
    set noloadplugins
else
    "We're running as either "vim" or "gvim"

    "Supertab options.
    au FileType python set omnifunc=pythoncomplete#Complete
    let g:SuperTabDefaultCompletionType = 'context'
    set completeopt=menuone,longest
    
    " Set Rooter to run on sh files as well
    au BufEnter *.sh :Rooter
endif
