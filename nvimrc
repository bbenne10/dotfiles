" vim:foldmethod=marker:foldlevel=0:ts=2:sts=2:sw=2

" Vimpager or Diff {{{
if &diff || exists("vimpager")
  set diffopt+=iwhite
  set noloadplugins
  " }}}
else
  source ~/.nvim/nvimrc.plugin_settings
  source ~/.nvim/nvimrc.plugins
  source ~/.nvim/nvimrc.base
  source ~/.nvim/nvimrc.misc
  source ~/.nvim/nvimrc.theme
  source ~/.nvim/nvimrc.status
  source ~/.nvim/nvimrc.keys
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

  " Make Unite close when we hit escape {{{
  function! s:UniteSettings()
    imap <buffer> <Esc> <Plug>(unite_exit)
  endfunction
  autocmd FileType unite :call s:UniteSettings()
  " }}}
endif
" }}}
