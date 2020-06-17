" Sane defaults.
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
set nowrap

set foldlevel=2

" Escaping from the terminal mode.
tnoremap <Esc> <C-\><C-n>

nmap ga <Plug>(EasyAlign)

" iceberg is a default theme. 
" Refresh syntax highlighting on each keystroke.
" Haskell breaks otherwise.
let g:solarized_termcolors=256
let g:airline_theme='minimalist'
set background=dark
colorscheme iceberg
syntax on
set noshowmode
au Syntax * syntax sync fromstart

" Show bufferline
let g:airline_section_x = ''
let g:airline_section_y = ''
let g:bufferline_echo = 0

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

" Save and put the cursor where it was before
nnoremap <leader>w mq:wa<CR>`q
let g:SignatureIncludeMarks = 'abcdefghijklmnoprstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

" Toggle NERDTree 
nnoremap <leader>t :NERDTreeToggle<CR>

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
