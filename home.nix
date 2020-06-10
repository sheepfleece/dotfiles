{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./scripts.nix
      ./mpv.nix
      ./zathura.nix
      ./vim.nix
      ./fish.nix
    ];

  xdg.configFile."." = {
    source = ./config;
    recursive = true;
  };

  home.file = with lib.attrsets; let
    files  = builtins.readDir prefix;
    prefix = ./home;
    each   = name: value: nameValuePair ("." + name) (config (prefix + ("/" + name)) value); 

    config = name: value: 
      if value == "directory" 
        then { source = name; recursive = true; }
        else { text = builtins.readFile name;   };
    in mapAttrs' each files;


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

    stylish-haskell

    ueberzug        # Ranger image previewer
    rofi-unwrapped  # windows switcher
    skypeforlinux   
    sysstat         # performance tools
    texlive.combined.scheme-full
    xbindkeys
    xkb-switch
    python37Packages.pygments
    xorg.xkbcomp
    xorg.xmodmap
    xss-lock
    youtube-dl

    taskwarrior

    # Friendship ended with C
    # Now Rust is my best friend
    bat 
    exa 
    bandwhich
    shotgun 
  ];

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
