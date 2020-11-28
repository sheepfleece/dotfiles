{ config, pkgs, lib, ... }:

with pkgs;
let 
  sources = import ./nix/sources.nix;

  nixpkgsPlugins = with vimPlugins; [
    ReplaceWithRegister # gr<object>
    fzf-vim fzfWrapper  # :Files, :Rg, :Colors
    nerdtree            # :NERDTreeToggle 
    vim-commentary      # gc<motion>
    vim-easy-align      # ga<object><char>
    vim-eunuch          # :Delete, :Move, :Rename
    vim-fugitive        # :Git
    vim-repeat          # .
    vim-sort-motion     # gs<motion>
    vim-surround        # ys<object><char>
    vim-matchup         # new motions: g%, [%, ]%, z%

    # Language server
    coc-nvim

    # Status line
    vim-airline        # New status line
    vim-airline-themes # 'minimalist' theme
    vim-bufferline     # Show open buffers

    # Declaring objects painlessly
    vim-textobj-user    


    # Themes
    iceberg-vim
    nord-vim
    vim-colors-solarized
    vim-colorschemes

    # vim-polyglot
    

    # Haskell IDE
    haskell-vim
    vim-stylishask
    vim-textobj-haskell # <action>ah
  ];

  mkPlugin = name: vimUtils.buildVimPlugin {
    pname = name;
    version = "0.0.0";
    src = builtins.fetchTarball {
      url = sources.${name}.url;
      sha256 = sources.${name}.sha256;
    };
  };

  nivPlugins = map mkPlugin [
    "startuptime.vim"
    "neuron.vim"      

    "indentLine"
    "vim-smoothie"     # Smooth-scrolling for <C-d> <C-u>
    "quick-scope"

    "vim-haskellFold"
    "vim-move"         # <A-j> <A-k>
    "vimroom"          # <leader>V
    "vim-signature"    # Signatures for navigation marks
    "vim-textobj-line" # <action>al

    # Themes
    "gruvbox"
    "vim-farout"

    # Unused now (So I won't forget later)
    # "vim-bookmarks"
    # "vim-haskell"
    # "vim-hoogle"  
    # "vim-ripgrep"
  ];


  coc-default-config = pkgs.writeText "coc-default" ''
    " CoC-defaults
    set nobackup
    set nowritebackup

    set cmdheight=1
    set updatetime=300

    set shortmess+=c

    set signcolumn=yes
    let g:vimroom_width=100
    let g:vimroom_sidebar_height=0

    autocmd VimResized * wincmd =

    inoremap <silent><expr> <TAB>
          \ pumvisible() ? "\<C-n>" :
          \ <SID>check_back_space() ? "\<TAB>" :
          \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    " Use <c-space> to trigger completion.
    inoremap <silent><expr> <c-space> coc#refresh()

    " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
    " position. Coc only does snippet and additional edit on confirm.
    " <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
    if exists('*complete_info')
      inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
    else
      inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
    endif

    " Use `[g` and `]g` to navigate diagnostics
    nmap <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <silent> ]g <Plug>(coc-diagnostic-next)

    " GoTo code navigation.
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gD mq:tabnew %<CR>`q<Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)
    nmap <silent> gb <C-o>


    au TabLeave * let g:lasttab = tabpagenr()
    nnoremap <silent> <tab> :exe "tabn ".g:lasttab<cr>



    " Use K to show documentation in preview window.
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction

    " Highlight the symbol and its references when holding the cursor.
    autocmd CursorHold * silent call CocActionAsync('highlight')

    " Symbol renaming.
    nmap <leader>rn <Plug>(coc-rename)

    " Formatting selected code.
    " xmap <leader>f  <Plug>(coc-format-selected)
    " nmap <leader>f  <Plug>(coc-format-selected)

    augroup mygroup
      autocmd!
      " Setup formatexpr specified filetype(s).
      autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
      " Update signature help on jump placeholder.
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

    " Applying codeAction to the selected region.
    " Example: `<leader>aap` for current paragraph
    " xmap <leader>a  <Plug>(coc-codeaction-selected)
    " nmap <leader>a  <Plug>(coc-codeaction-selected)

    " Remap keys for applying codeAction to the current line.
    nmap <leader>ac  <Plug>(coc-codeaction)
    " Apply AutoFix to problem on the current line.
    nmap <leader>qf  <Plug>(coc-fix-current)

    " Map function and class text objects
    " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
    " TODO: Nothing works for haskell
    xmap if <Plug>(coc-funcobj-i)
    omap if <Plug>(coc-funcobj-i)
    xmap af <Plug>(coc-funcobj-a)
    omap af <Plug>(coc-funcobj-a)
    xmap ic <Plug>(coc-classobj-i)
    omap ic <Plug>(coc-classobj-i)
    xmap ac <Plug>(coc-classobj-a)
    omap ac <Plug>(coc-classobj-a)

    " Use CTRL-S for selections ranges.
    " Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
    " nmap <silent> <C-s> <Plug>(coc-range-select)
    " xmap <silent> <C-s> <Plug>(coc-range-select)

    " Add `:Format` command to format current buffer.
    command! -nargs=0 Format :call CocAction('format')

    " Add `:Fold` command to fold current buffer.
    command! -nargs=? Fold :call     CocAction('fold', <f-args>)

    " Add `:OR` command for organize imports of the current buffer.
    command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

    " Add (Neo)Vim's native statusline support.
    " NOTE: Please see `:h coc-status` for integrations with external plugins that
    " provide custom statusline: lightline.vim, vim-airline.
    set statusline^=%{coc#status()}%{get(b:,'coc_current_function',\'\')}

    " Mappings using CoCList:
    " Show all diagnostics.
    "  nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
    " Manage extensions.
    "  nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
    " Show commands.
    "  nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
    " Find symbol of current document.
    "  nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
    " Search workspace symbols.
    "  nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
    " Do default action for next item.
    "  nnoremap <silent> <space>j  :<C-u>CocNext<CR>
    " Do default action for previous item.
    "  nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
    " Resume latest coc list.
    "  nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
  '';

  macro-repeat = pkgs.writeText "macro-repeat" ''
    " When . repeats g@, repeat the last macro.
    fun! AtRepeat(_)
        " If no count is supplied use the one saved in s:atcount.
        " Otherwise save the new count in s:atcount, so it will be
        " applied to repeats.
        let s:atcount = v:count ? v:count : s:atcount
        " feedkeys() rather than :normal allows finishing in Insert
        " mode, should the macro do that. @@ is remapped, so 'opfunc'
        " will be correct, even if the macro changes it.
        call feedkeys(s:atcount.'@@')
    endfun

    fun! AtSetRepeat(_)
        set opfunc=AtRepeat
    endfun

    " Called by g@ being invoked directly for the first time. Sets
    " 'opfunc' ready for repeats with . by calling AtSetRepeat().
    fun! AtInit()
        " Make sure setting 'opfunc' happens here, after initial playback
        " of the macro recording, in case 'opfunc' is set there.
        set opfunc=AtSetRepeat
        return 'g@l'
    endfun

    " Enable calling a function within the mapping for @
    nno <expr> <plug>@init AtInit()
    " A macro could, albeit unusually, end in Insert mode.
    ino <expr> <plug>@init "\<c-o>".AtInit()

    fun! AtReg()
        let s:atcount = v:count1
        let c = nr2char(getchar())
        return '@'.c."\<plug>@init"
    endfun

    nmap <expr> @ AtReg()
  '';

  git-move = pkgs.writeText "git-move" ''
    " git
    fun! GitMove(where)
      silent execute '!git ' . a:where . ' 1>/dev/null 2>&1'
      checktime
      NERDTreeRefreshRoot
      !git log -1 --pretty=\%B
    endfunction

    nnoremap gN :call GitMove("next")<CR>
    nnoremap gP :call GitMove("prev")<CR>

    " go file is too useful to remap
    nnoremap gL :call GitMove("first")<CR>
    nnoremap gl :call GitMove("last")<CR>
  '';

  nvim = neovim.override {
    viAlias = true;
    vimAlias = true;
    configure = {
      customRC = '' 
        let loaded_netrwPlugin = 1
        au ColorScheme farout hi Comment ctermfg=242 guifg=#6b7089
        source ~/.vimrc
        source ${coc-default-config}
        source ${macro-repeat}
      '';
      packages.myVimPackage = {
        start = nixpkgsPlugins ++ nivPlugins;
        opt = [ ];
      };
    };
  };

in
{
  home.packages = [
    nvim 
    xsel   # X clipboard
    nodejs # coc-nvim
  ];
}

