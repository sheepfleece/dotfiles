set expandtab
set shiftwidth=4
set softtabstop=4

function Spaces(...)
    if a:0 == 1
        let l:width = a:1
    else
        let l:width = 4
    endif
    setlocal expandtab
    let &l:shiftwidth = l:width
    let &l:softtabstop = l:width
endfunction
command! T setlocal noexpandtab shiftwidth=8 softtabstop=0
command! -nargs=? S call Spaces(<args>)
autocmd BufNewFile,BufRead ~/src/linux/* T
autocmd BufNewFile,BufRead ~/src/git/* T
autocmd FileType html S 2
autocmd FileType tex S 2

set backupdir=~/.vim/backups,.
set directory=~/.vim/swapfiles,.
