" vim:foldmethod=marker:foldlevel=0:ts=2:sts=2:sw=2

" Vimpager or Diff {{{
if &diff || exists("vimpager")
  set diffopt+=iwhite
  set noloadplugins
  " }}}
else
  " "Normal" Run {{{
  " Plugin setup {{{
  " Make vim not work EXACTLY as vi did
  set nocompatible
  filetype plugin indent on

  " Always use utf-8
  set encoding=utf-8

  " use language's specification file to autoindent
  set autoindent

  " Plugin variables {{{
  " Colorschemes {{{
  let g:simpleburn_transparent_term=1
  " }}}
  " Syntastic {{{
  let g:syntastic_python_python_exec = "/usr/bin/python2.7"
  let g:syntastic_python_checkers = ['pylama']
  let g:syntastic_javascript_jshint_exec='jsxhint'
  let g:syntastic_go_checkers = ["go", "gofmt"]
  let g:syntastic_puppet_checkers = ['puppetlint']
  let g:syntastic_always_populate_loc_list = 1
  let g:syntastic_auto_loc_list = 1
  let g:syntastic_check_on_open = 1
  let g:syntastic_loc_list_height = 5
  let g:syntastic_stl_format = "← %t err(s) @ ln %F"
  " }}}"
  " Unite {{{
  let g:unite_split_rule = "botright"
  let g:unite_winheight = 10
  let g:unite_source_file_async_command =
    \ 'ag --follow --nocolor --nogroup --hidden -g ""'
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts =
    \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
    \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'' ' .
    \ '--ignore ''**/*.pyc'''
  let g:unite_source_rec_async_command =
    \ 'ag --follow --nocolor --nogroup --hidden ' .
    \ '--ignore ''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'' ' .
    \ '--ignore ''**/*.pyc'' -g ""'
  let g:unite_source_grep_recursive_opt = ''
  " }}}
  " Misc (JSX, Emmet, SuperTab) {{{
  let g:jsx_ext_required = 0

  let g:user_emmet_leader_key='<c-z>'

  let g:SuperTabDefaultCompletionType = 'context'
  " }}}
  " }}}

  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
          \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall
  endif

  call plug#begin('~/.vim/bundle')

  " }}}
  " Plugins {{{
  " 'interface' plugins {{{
  Plug 'Shougo/unite.vim'
  Plug 'Shougo/deoplete.nvim'
  " }}}

  " 'utility' plugins {{{
  Plug 'airblade/vim-rooter'
  Plug 'rking/ag.vim'
  Plug 'tpope/vim-fugitive'
  Plug 'mattn/emmet-vim'
  Plug 'junegunn/vim-easy-align'
  " }}}

  " 'syntax' plugins {{{
  Plug 'scrooloose/syntastic'
  Plug 'hynek/vim-python-pep8-indent'
  Plug 'MarcWeber/vim-addon-mw-utils'
  Plug 'voithos/vim-python-matchit'
  Plug 'rodjek/vim-puppet'
  Plug 'jsx/jsx.vim'
  Plug 'groenewege/vim-less'
  " }}}

  " colorschemes {{{
  Plug 'bbenne10/simpleburn'
  Plug 'morhetz/gruvbox'
  " }}}

  call plug#end()

  " }}}
  " Spaces and Tabs {{{
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
  " }}}
  " {{{ Search
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
  " }}}
  " {{{ Navigation
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

  "When joining commented lines, delete the leading comment char.
  set formatoptions+=j
  " }}}
  " Misc {{{
  " change the leader key
  let mapleader = "\<Space>"

  "Don't add a space when joining
  set nojoinspaces

  "Turn off both backing up files and swap files - these are just annoying
  set nobackup
  set noswapfile

  "persistent undo
  set undodir=$HOME/.vim/undo
  set undofile
  set undolevels=10000
  set undoreload=100000

  " shorten the required time between keypresses in some scenarios
  set timeout ttimeoutlen=50

  "decent clipboard
  "This adds integration to Xorg's 'Ctrl+V' type buffer
  set clipboard=unnamedplus

  " 'grep' should call ag instead of grep
  set grepprg=ag\ --nogroup\ --nocolor
  " }}}
  " UI Look and Behavior {{{

  "color/syntax nicieties
  set background=dark
  syntax enable

  colorscheme gruvbox

  " Modify colorschemes when they don't quite work right
  if g:colors_name == "gruvbox"
    hi StatusLine   cterm=NONE ctermbg=237 ctermfg=15
    hi StatusLineNC cterm=NONE ctermbg=237 ctermfg=240
  endif

  " used for statusline coloring
  hi User1        cterm=NONE ctermbg=4   ctermfg=0
  hi User2        cterm=NONE ctermbg=237 ctermfg=9
  hi User3        cterm=NONE ctermbg=237 ctermfg=6

  "draw a bar at 80 characters
  set colorcolumn=80

  "show line numbers
  set number

  "hilight the line with the cursor
  set cursorline

  "Remove unnecessary GUI options when using gvim.
  set go=aegitc

  "change gui font
  set guifont=Input\ 10

  "Always show the status line
  set laststatus=2

  "Fix the ugly vertical split thing
  set fillchars=vert:│

  "How should whitespace be shown when it is being shown?
  set listchars=tab:→\ ,eol:¬,trail:·,precedes:«,extends:»

  "sane filename matching when searching the filesystem (:e and the like)
  set wildmenu
  set wildmode=longest:full,full

  " When completing in a menu, always show the menu and only complete as
  " much as is unique
  set completeopt=menuone,longest

  "Don't show mode in line below status
  set noshowmode

  " Status line {{{
  function! Status(winnum)
    let active = a:winnum == winnr()
    let bufnum = winbufnr(a:winnum)
    let stat = ''

    function! Color(active, num, content)
      if a:active
        return '%' . a:num . '*' . a:content . '%*'
      else
        return a:content
      endif
    endfunction

    let stat .= Color(active, 1, " " . toupper(mode()) . " ")
    let stat .= '%1*'
    let stat .= '%*'

    " file name
    let stat .= Color(active, 2, active ? ' »' : ' «')
    let stat .= ' %<%f'
    let stat .= ' ' . Color(active, 2, active ? '«' : '»')

    " file modfied
    let modified = getbufvar(bufnum, '&modified')
    let stat .= Color(active, 2, modified ? ' +' : '')

    " read only
    let readonly = getbufvar(bufnum, '&readonly')
    let stat .= Color(active, 2, readonly ? ' ‼' : '')

    if active && &paste
      let stat .= ' %2*' . 'p' . '%*'
    endif
    let stat .= '%='

    " git branch
    if active && exists('*fugitive#head')
      let head = fugitive#head()
      if empty(head) && exists('*fugitive#detect') && !exists('b:git_dir')
        call fugitive#detect(getcwd())
        let head = fugitive#head()
      endif
    endif

    if active && !empty(head)
      let stat .= Color(active, 3, ' ← ') . head . ' '
    endif

    " file type
    let ft = getbufvar(bufnum, '&ft')
    if active && !empty(ft)
      let stat .= Color(active, 3, '← ') . ft . ' '
    endif

    " Syntax errs
    let sf = "%{SyntasticStatuslineFlag()}"
    if !empty(sf)
      let stat .= Color(active, 2, sf)
    endif

    return stat
  endfunction

  function! s:RefreshStatus()
    for nr in range(1, winnr('$'))
      call setwinvar(nr, '&statusline', '%!Status(' . nr . ')')
    endfor
  endfunction
  " }}}

  " }}}
  " Key Remapping {{{

  " Toggle hilighting search results. Keychain: <leader>+n
  nmap <silent> <leader>n :silent :set hls!<CR>

  " Toggle showing whitespace. Keychain: <leader>+s
  nmap <silent> <leader>s :set nolist!<CR>

  " When pressing / to initiate search, turn on search hilighting
  nnoremap / :set hlsearch<CR>/

  " When saving, make :W work like :w
  cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))

  " Turn off the help system's keybind (and bind it to toggle paste mode)
  nnoremap <F1> :set paste!<CR>

  " Allow us to launch Rainbow (to inspect colors)
  nnoremap <leader>R :silent !rainbow < % &<cr>

  " Search buffers with <leader>w
  nnoremap <leader>w :Unite -start-insert buffer<cr>

  " Open new buffers w/ fuzzy finding
  nnoremap <leader>e :Unite -start-insert file_rec/async:!<cr>

  " Call 'grep' (really ag)
  nnoremap <leader>y :Unite grep:.<cr>

  " Jump to the next or previous in the location window (works with pylama)
  nnoremap <leader>ln :lnext<cr>
  nnoremap <leader>lp :lprev<cr>

  " Leave insert mode without hitting esc
  imap jk <Esc>

  " Convert markdown to html.
  nmap <leader>m :silent !~/.bin/compile_markdown %:p<cr>

  " Start interactive EasyAlign for a motion/text object (e.g. gaip)
  nmap ga <Plug>(EasyAlign)

  " }}}
  " Auto Commands {{{
  " Run line under cursor in user's shell (or in python if it's a py "script)
  autocmd BufRead *.sh nmap <buffer> <leader>r :.w !$SHELL<CR>
  autocmd BufRead *.py nmap <buffer> <leader>r :.w !python2 %<CR>

  " Strip trailing whitespace (and save cursor position) when saving files {{{
  fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
  endfun
  autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()
  " }}}

  " Change filetype for HTML to htmldjango - this colors some additional syntax
  au BufNewFile,BufRead *.html set filetype=htmldjango

  " Make Unite close when we hit escape {{{
  function! s:UniteSettings()
    imap <buffer> <Esc> <Plug>(unite_exit)
  endfunction
  autocmd FileType unite :call s:UniteSettings()
  " }}}
  " Update the status bar when window is changed {{{
  augroup status
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter * call <SID>RefreshStatus()
  augroup END

  au FileType python setlocal formatprg=autopep8\ -
endif
" }}}
