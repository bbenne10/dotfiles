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
  let g:solarized_italic = 0
  let base16colorspace=256
  " }}}
  " Syntastic {{{
  let g:syntastic_python_python_exec = "/usr/bin/python2.7"
  let g:syntastic_python_checkers = ['pylama']
  let g:syntastic_javascript_jshint_exec='jsxhint'
  " }}}
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
  " NeoComplete {{{
  " (taken straight from the example)
  let g:acp_enableAtStartup = 0
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1
  let g:neocomplete#sources#syntax#min_keyword_length = 3

  " }}}
  " Misc (JSX, Emmet, SuperTab) {{{
  let g:jsx_ext_required = 0

  let g:user_emmet_leader_key='<c-z>'

  let g:SuperTabDefaultCompletionType = 'context'
  " }}}
  " }}}

  set rtp+=~/.vim/bundle/neobundle.vim
  call neobundle#begin(expand('~/.vim/bundle'))
  NeoBundleFetch 'Shougo/neobundle.vim'

  " }}}
  " Plugins {{{
  " 'interface' plugins {{{
  NeoBundle 'Shougo/unite.vim'
  NeoBundle 'Shougo/vimproc.vim', { 'build': { 'linux': 'make' } }
  NeoBundle 'Shougo/neocomplete.vim'
  " }}}

  " 'utility' plugins {{{
  NeoBundle 'airblade/vim-rooter'
  NeoBundle 'rking/ag.vim'
  NeoBundle 'tpope/vim-fugitive'
  NeoBundle 'mattn/emmet-vim'
  " }}}

  " 'syntax' plugins {{{
  NeoBundle 'scrooloose/syntastic'
  NeoBundle 'hynek/vim-python-pep8-indent'
  NeoBundle 'MarcWeber/vim-addon-mw-utils'
  NeoBundle 'voithos/vim-python-matchit'
  NeoBundle 'rodjek/vim-puppet'
  NeoBundle 'jsx/jsx.vim'
  NeoBundle 'groenewege/vim-less'
  " }}}

  " colorschemes {{{
  NeoBundle 'bbenne10/simpleburn'
  NeoBundle 'chriskempson/base16-vim'
  " }}}

  call neobundle#end()
  NeoBundleCheck

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

  colorscheme base16-ocean

  " used for statusline coloring
  hi StatusLine ctermbg=18    ctermfg=blue  guibg=#a7adba guifg=#343d46
  hi User1      ctermbg=blue  ctermfg=black guibg=green   guifg=red
  hi User2      ctermbg=18    ctermfg=red   guibg=18      guifg=red
  hi User3      ctermbg=18    ctermfg=blue  guibg=blue    guifg=green


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
    let stat .= ' %<'
    let stat .= '%f'
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
    if exists('*fugitive#head')
      let head = fugitive#head()
      if empty(head) && exists('*fugitive#detect') && !exists('b:git_dir')
        call fugitive#detect(getcwd())
        let head = fugitive#head()
      endif
    endif

    if !empty(head)
      let stat .= Color(active, 3, ' ← ') . head . ' '
    endif

    let ft = getbufvar(bufnum, '&ft')
    if !empty(ft)
      let stat .= Color(active, 3, '← ') . ft . ' '
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

  " delete a window
  nnoremap <leader>bd :BD<CR>

  " neocomplete maps {{{
  inoremap <expr><C-g> neocomplete#undo_completion()
  inoremap <expr><C-l> neocomplete#complete_common_string()
  inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
  inoremap <expr><Esc> pumvisible() ? neocomplete#close_popup() : "\<Esc>"
  " }}}


  " Convert markdown to html. This doesn't work in the reverse, but you may
  " simply undo the change to 'preview' the changes before saving.
  nmap <leader>m :silent !~/.bin/compile_markdown %:p<cr>
  ":redraw!<cr>

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

  " Set Rooter to run on sh files as well
  au BufEnter *.sh :Rooter

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
  " }}}
  " NeoComplete's AutoCommands (enable omnicomplete) {{{
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
  " }}}

  " }}}
  " Python VirtualEnv {{{
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
    " }}}
  endif
  " }}}
