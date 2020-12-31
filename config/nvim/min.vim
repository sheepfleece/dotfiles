" Sane defaults.
set encoding=utf-8
set history=9000
set number relativenumber
set splitbelow splitright
set nojoinspaces
set backspace=indent,eol,start
set textwidth=0
set scrolloff=8
set hidden
set formatoptions+=j
filetype plugin on
set linebreak

set foldlevel=2

nnoremap j gj
nnoremap k gk

" Escaping from the terminal mode.
tnoremap <Esc> <C-\><C-n>


" Refresh syntax highlighting on each keystroke.
" Haskell syntax breaks otherwise.
syntax on
set noshowmode
au Syntax * syntax sync fromstart


" 1 tab == 2 spaces
" Do not use tabs, indent nicely.
set autoindent expandtab
set shiftround
set shiftwidth=2
set smarttab
set softtabstop=0
set tabstop=2

" A poor man's file explorer.
"   Now I use fzf instead.
set path+=src/**
set wildmenu
set wildignore+=*.o,*.obj,*.hi,*.dyn_o,*.dyn_hi
set wildignorecase

" Do not highlight search results, because 
"   it is annoying for navigation.
set nohlsearch
set incsearch
set ignorecase

""" Maps

nnoremap zo za
nnoremap n nzz
nnoremap G Gzz

noremap L g_
noremap H ^

""" Tabs

noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt

""" Buffers

" Navigating open buffers
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

""" Leader maps
" Not remapped marks are still usable. 
let mapleader = "m"


" Copying and pasting
noremap <space> "+

" Fuzzy-navigation
nnoremap s :GFiles<CR>
nnoremap <C-s> :Files<CR>
nnoremap S :Buffers<CR>

nnoremap <C-Q> :bdelete<CR>

" re-undo
nnoremap U <C-r> 
nnoremap Q :q<CR>

" 
nnoremap <space><space> :vert sb<CR>


let rumap = 'йцукенгшщзхъфывапролджэёячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ'
let enmap = 'qwertyuiop[]asdfghjkl;''\zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>' 
let mapLen = strchars(rumap)
let i = 0
while i < mapLen
    let ruChar = matchstr(rumap, ".", byteidx(rumap, i))
    let enChar = enmap[i]
    execute 'map '.ruChar.' '.enChar
    execute 'cmap '.ruChar.' '.enChar
    let i += 1
endwhile
