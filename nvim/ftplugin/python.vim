setlocal tabstop=4
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal formatprg=autopep8\ -
autocmd BufWritePost <buffer> Neomake
