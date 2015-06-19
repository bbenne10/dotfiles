" Vimpager or Diff {{{
if &diff || exists("vimpager")
  set diffopt+=iwhite
  set noloadplugins
else
  source ~/.nvim/nvimrc.plugin_settings
  source ~/.nvim/nvimrc.plugins
  source ~/.nvim/nvimrc.base
  source ~/.nvim/nvimrc.misc
  source ~/.nvim/nvimrc.theme
  source ~/.nvim/nvimrc.status
  source ~/.nvim/nvimrc.keys
  source ~/.nvim/nvimrc.autocmds
endif
