{ config, pkgs, ... }:
let
  touchpad-toggle = pkgs.writeScriptBin "touchpad-toggle" ''
    #!/bin/sh

    device='SYNA3081:00 06CB:826F Touchpad'

    # true || echo 1 && echo 2 == (true || echo 1) && echo 2 == 2 
    # in C it is the other way around
    [[ "$(xinput list-props "$device" | grep -P ".*Device Enabled.*\K.(?=$)" -o)" == "1" ]] &&
        xinput disable "$device" ||
        xinput enable "$device"
  '';
in {
  imports =
    [
      ./mpv.nix
      ./zathura.nix
      ./vim.nix
      ./fish.nix
    ];


  xdg.configFile."." = {
    source = ./config;
    recursive = true;
  };

  home.file.".xbindkeysrc" = {
    text = builtins.readFile ./home/xbindkeysrc;
  };

  home.file.".ghci" = {
    text = builtins.readFile ./home/ghci;
  };

  home.file.".xmonad/xmonad.hs" = {
    text = builtins.readFile ./home/xmonad/xmonad.hs;
  };

  home.file.".cabal/config" = {
    text = builtins.readFile ./home/cabal/config;
  };

  home.file.".ncmpcpp/bindings" = {
    text = builtins.readFile ./home/ncmpcpp/bindings;
  };

  home.file.".ssh" = {
    source = ./home/ssh;
    recursive = true;
  };

  services.lorri.enable = true;
  services.mpd.enable = true;
  services.mpd.musicDirectory = "/home/sheep/Music";

  home.packages = with pkgs; [
    mpd 
    mpc_cli
    ncmpcpp       # music player
    atool         # Archive helper
    canto-curses  # RSS Reader
    gdb           # GNU Debugger
    haskellPackages.graphmod
    graphviz      # Graphs builder
    irssi         # IRC Channel
    libav         # fork of ffmpeg
    ffmpeg        # convert audio, etc.
    lshw          # list hardware
    mp3val        # Repair mp3 songs
    mtr           # traceroute, ping
    ntfs3g        # mounting ntfs stuff

    ormolu        # haskell source code formatter
    stylish-haskell

    ueberzug        # Ranger image previewer
    rofi-unwrapped  # windows switcher
    # rxvt-unicode
    shotgun         # XScreenshots
    skypeforlinux   
    sysstat         # performance tools
    texlive.combined.scheme-full
    # ghostscript
    inkscape xfig gnuplot # temp
    xbindkeys
    xkb-switch
    python37Packages.pygments
    xorg.xkbcomp
    xorg.xmodmap
    xss-lock
    youtube-dl

    touchpad-toggle
];

fonts.fontconfig.enable = true;

programs.home-manager = {
  enable = true;
};

# This value determines the Home Manager release that your
# configuration is compatible with. This helps avoid breakage
# when a new Home Manager release introduces backwards
# incompatible changes.
#
# You can update Home Manager without changing this value. See
# the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";
}
