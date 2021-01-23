" Colors & Themes
" au ColorScheme farout  hi Comment           ctermfg=242 guifg=#6b7089
" au ColorScheme nord    hi SignatureMarkText ctermfg =1 ctermbg=234
" au ColorScheme iceberg hi SignatureMarkText ctermfg =1 ctermbg=234
set background=dark
colorscheme gruvbox

source ~/.config/nvim/coc.vim

" Sane defaults.
set encoding=utf-8
set noemoji
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


" Move intuitively through wrapped lines
nnoremap j gj
nnoremap k gk

" Escaping from the terminal mode.
" Not the best solution for using Ranger.
tnoremap <Esc> <C-\><C-n>


" Refresh syntax highlighting on each keystroke.
" Haskell andd CPP syntax breaks otherwise.
syntax on
set noshowmode
au Syntax * syntax sync fromstart

" Orthography highlighting
noremap <leader>o :setlocal spell! spelllang=en_us<CR>

" 1 tab == 2 spaces
" Do not use tabs, indent nicely.
" TODO: Mostly used for haskell, move to specific ftplugin
set autoindent expandtab
set shiftround
set shiftwidth=2
set smarttab
set softtabstop=0
set tabstop=2

" Do not highlight search results, 
" because it is annoying for navigation.
set nohlsearch
set incsearch
set ignorecase
set nowrapscan


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
nnoremap <C-p> <C-w>p

" Closing buffers
nnoremap <C-Q> :bdelete<CR>

" Opening buffers
nnoremap <space><space> :vert sb<CR>

""" Leader maps

" Not remapped marks are still usable. 
let mapleader = "m"

" Copying and pasting
noremap <space> "+

" Fuzzy-navigation
nnoremap sg :GFiles<CR>
nnoremap sf :Files<CR>
nnoremap sb :Buffers<CR>
nnoremap sc :Colors<CR>
nnoremap sh :History:<CR>


" re-undo
nnoremap U <C-r> 
nnoremap Q :q<CR>

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


""" Plugins 

" EasyAlign
nmap ga <Plug>(EasyAlign)

" Show bufferline
let g:airline_section_x = ''
let g:airline_section_y = ''
let g:bufferline_echo = 0
let g:airline_theme='minimalist'

" IndentLine
let g:indentLine_leadingSpaceChar = '·'
let g:indentLine_leadingSpaceEnabled = 1

" Colors for solarized theme
let g:solarized_termcolors=256

" Mark signatures
let g:SignatureIncludeMarks = 'abcdefghijklmnoprstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

" Easier navigation with f/t
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
highlight QuickScopePrimary ctermfg=3 cterm=underline
highlight QuickScopeSecondary ctermfg=1 cterm=underline

" Ranger
" Disable netrw
let loaded_netrwPlugin = 1
let g:ranger_replace_netrw = 1

