{ config, pkgs, lib, ... }:

with pkgs;
let 
  plugins = with vimPlugins; [
    fzf-vim fzfWrapper  # :Files, :Rg, :Colors
    nerdtree            # :NERDTreeToggle 
    vim-commentary      # gc<motion>
    ReplaceWithRegister # gr<object>
    vim-fugitive        # :Git
    vim-repeat          # .
    vim-sort-motion     # gs<motion>
    vim-surround        # ys<object><char>
    vim-room            # <leader>V
    vim-eunuch          # :Delete, :Move, :Rename
    vim-sneak           # s<char><char>, ;, <action>z<char><char>
    vim-easy-align      # ga<object><char>
  
    # Language server
    coc-nvim

    # Status line
    vim-airline
    vim-airline-themes
    vim-bufferline

    # Declaring objects painlessly
    vim-textobj-user    
    vim-textobj-line   # <action>al

    # Signatures for navigation marks
    vim-signature

    # Themes
    iceberg-vim
    vim-colors-solarized
    vim-farout
    gruvbox 

    # Language specific plugins
    vim-nix
    rust-vim

    # Haskell IDE
    vim-textobj-haskell # <action>ah
    haskell-vim
    vim-haskellFold
    vim-stylishask
    hlint-refactor-vim   

    ### Unused now
    # vim-bookmarks     # mm, mp, mn, mi
    # vim-easymotion
    # tagbar
    # vim-hoogle
  ];

  vim-haskellFold = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-haskellFold";
    version = "1.0.0";
    src = fetchFromGitHub {
      owner = "sheepfleece";
      repo = "vim-haskellFold";
      rev = "42dbb64c79ee4c55ee87d2615c483b23e9a28cd1";
      sha256 = "0hdw0535d59zmfcg0ly9krkk7gyz20yy9013v20llnf748m28ada";
    };
    dependencies = [];
  };

  vim-ripgrep = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-ripgrep";
    version = "1.0.0";
    src = fetchFromGitHub {
      owner = "jremmen";
      repo = "vim-ripgrep";
      rev = "ec87af6b69387abb3c4449ce8c4040d2d00d745e";
      sha256 = "1by56rflr0bmnjvcvaa9r228zyrmxwfkzkclxvdfscm7l7n7jnmh";
    };
    dependencies = [];
  };

  vim-farout = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-farout";
    version = "1.0.0";
    src = fetchFromGitHub {
      owner = "fcpg";
      repo = "vim-farout";
      rev = "3975c275a01224db29247cb61a67111ec3729f12";
      sha256 = "1k3i5g9x1ygiz2lq6nzwp5dy43f13s298vf8hkkrrd8vs74c90qq";
    };
    dependencies = [];
  };

  gruvbox = vimUtils.buildVimPluginFrom2Nix {
    pname = "gruvbox";
    version = "1.0.0";
    src = fetchFromGitHub {
      owner = "morhetz";
      repo = "gruvbox";
      rev = "040138616bec342d5ea94d4db296f8ddca17007a";
      sha256 = "0qk2mqs04qlxkc1ldgjbiv1yisi2xl2b8svmjz0hdp9y2l5vfccw";
    };
    dependencies = [];
  };

  vim-hoogle = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-hoogle";
    version = "1.0.0";
    src = fetchFromGitHub {
      owner = "Twinside";
      repo = "vim-hoogle";
      rev = "871d104c92e33cb238506f2805f1652561978cc8";
      sha256 = "17qvi57g72ijgk7nczczli3kcphvdf625fzqbqcmqpsawgvfd07n";
    };
    dependencies = [];
  };

  vim-bookmarks = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-bookmarks";
    version = "1.3.0";
    src = fetchFromGitHub {
      owner = "MattesGroeger";
      repo = "vim-bookmarks";
      rev = "93904787c61805334895662feb6672a9164820be";
      sha256 = "1xfcjb82yzcjiixcrfrlbn9a7r4wxbrqq4jrv17f5l88srw6n1by";
    };
    dependencies = [];
  };

  vim-room = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-room";
    version = "0.7.0";
    src = fetchFromGitHub {
      owner = "mikewest";
      repo = "vimroom";
      rev = "a39a90f15bead8567d46b324db0c5f8861738fa0";
      sha256 = "0zhlacvkkdvfqdpvhwn7kvyr1w84vl76kqhm5rmhgdgyfmi6zh2v";
    };
    dependencies = [];
  };

  vim-signature = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-signature";
    version = "1.0.0";
    src = fetchFromGitHub {
      owner = "kshenoy";
      repo = "vim-signature";
      rev = "6bc3dd1294a22e897f0dcf8dd72b85f350e306bc";
      sha256 = "08m5dg77yavria7n7iajkj4kqaw848763680003j2gbrjlhpprpm";
    };
    dependencies = [];
  };

  vim-textobj-line = vimUtils.buildVimPluginFrom2Nix {
    pname = "vim-textobj-line";
    version = "0.0.2";
    src = fetchFromGitHub {
      owner = "kana";
      repo = "vim-textobj-line";
      rev = "0a78169a33c7ea7718b9fa0fad63c11c04727291";
      sha256 = "0mppgcmb83wpvn33vadk0wq6w6pg9cq37818d1alk6ka0fdj7ack";
    };
    dependencies = [];
  };

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
    xmap <leader>f  <Plug>(coc-format-selected)
    nmap <leader>f  <Plug>(coc-format-selected)

    augroup mygroup
      autocmd!
      " Setup formatexpr specified filetype(s).
      autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
      " Update signature help on jump placeholder.
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

    " Applying codeAction to the selected region.
    " Example: `<leader>aap` for current paragraph
    xmap <leader>a  <Plug>(coc-codeaction-selected)
    nmap <leader>a  <Plug>(coc-codeaction-selected)

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
    nmap <silent> <C-s> <Plug>(coc-range-select)
    xmap <silent> <C-s> <Plug>(coc-range-select)

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
    nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
    " Manage extensions.
    nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
    " Show commands.
    nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
    " Find symbol of current document.
    nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
    " Search workspace symbols.
    nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
    " Do default action for next item.
    nnoremap <silent> <space>j  :<C-u>CocNext<CR>
    " Do default action for previous item.
    nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
    " Resume latest coc list.
    nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
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
    vimAlias = true;
    configure = {
      customRC = '' 
        au ColorScheme farout hi Comment ctermfg=242 guifg=#6b7089
        source ${./home/vimrc.vim}
        source ${coc-default-config}
      '';
      vam.pluginDictionaries = lib.singleton { 
        names = plugins;
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
