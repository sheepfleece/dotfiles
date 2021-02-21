{ config, pkgs, lib, ... }:

with pkgs;
let 
  sources = import ./nix/sources.nix;

  eagerPlugins = with vimPlugins; [
    ReplaceWithRegister # gr<object>
    fzf-vim fzfWrapper  # :Files, :Rg, :Colors
    vim-commentary      # gc<motion>
    vim-easy-align      # ga<object><char>
    vim-eunuch          # :Delete, :Move, :Rename
    vim-fugitive        # :Git
    vim-repeat          # .
    vim-sort-motion     # gs<motion>
    vim-surround        # ys<object><char>
    vim-matchup         # new motions: g%, [%, ]%, z%

    # Status line
    vim-airline        # New status line
    vim-airline-themes # 'minimalist' theme
    vim-bufferline     # Show open buffers

    # Language pack
    vim-polyglot

    vim-textobj-user    

    # Language server
    coc-nvim
  ];

  lazyPlugins  = with vimPlugins; [
    # Themes
    iceberg-vim
    nord-vim
    vim-colors-solarized
    vim-colorschemes
  ];

  haskellPlugins = with vimPlugins; [
    haskell-vim
    vim-stylishask
    # vim-textobj-haskell # <action>ah
  ] ++ map mkPlugin [
    "vim-haskellFold"
  ];

  cppPlugins = map mkPlugin [
    "nvim-gdb"
    "vim-lsp-cxx-highlight"
    "syntastic"
    "vim-clang-format"
  ];

  mkPlugin = name: vimUtils.buildVimPlugin {
    pname = name;
    version = "0.0.0";
    src = builtins.fetchTarball {
      url = sources.${name}.url;
      sha256 = sources.${name}.sha256;
    };
  };

  nivLazyPlugins = map mkPlugin [
    # Themes
    "gruvbox"
    "vim-farout"
  ];

  nivEagerPlugins = map mkPlugin [
    "startuptime.vim"
    "ranger.vim"
    "bclose.vim"

    "indentLine"
    "vim-smoothie"     # Smooth-scrolling for <C-d> <C-u>
    "quick-scope"

    "vim-move"         # <A-j> <A-k>
    "vim-signature"    # Signatures for navigation marks
    "vim-textobj-line" # <action>al

    "firenvim"
  ];
  nivUnusedPlugins = map mkPlugin [
    "vimroom"          # <leader>V
    "vim-bookmarks"
    "vim-haskell"
    "vim-hoogle"  
    "vim-ripgrep"
  ];


  nvim = neovim.override {
    viAlias = true;
    vimAlias = true;
    configure = {
      customRC = "source ~/.config/nvim/init.vim";
      packages.myVimPackage = {
        start = eagerPlugins ++ nivEagerPlugins ++ lazyPlugins ++ nivLazyPlugins ++ haskellPlugins ++ cppPlugins;
        opt = [];
      };
    };
  };

in
{
  home.packages = [
    nvim 
    xsel   # X clipboard
    nodejs # coc-nvim


    ccls   # c++ language server
    cpplint
    llvm
  ];
}

