nnoremap gh :GHCi<CR>

" ghci
command! -nargs=0 GHCi
\ | execute ':terminal ghci %'

" cabal
command -nargs=0 Repl
\ | execute ':terminal cabal repl'

" Haskell abbreviations
iabbrev iq import qualified
iabbrev hh <-
iabbrev ll ->
iabbrev ir import


" haskell.vim
let g:haskell_enable_quantification = 1   
let g:haskell_enable_recursivedo = 1     
let g:haskell_enable_arrowsyntax = 1    
let g:haskell_enable_pattern_synonyms = 1 
let g:haskell_enable_typeroles = 1       
let g:haskell_enable_static_pointers = 1
let g:haskell_backpack = 1             

let g:haskell_classic_highlighting = 1
let g:haskell_indent_in = 0
let g:haskell_indent_if = 2

