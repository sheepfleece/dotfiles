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

  # Copy everything from `./config` into `~/.config`
  xdg.configFile."." = {
    source = ./config;
    recursive = true;
  };

  # Copy everything from `./home` into `~`.
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

  services.dunst.enable = true;

  home.packages = with pkgs; [
    # Music
    mpv 
    mpd           # music server
    mpc_cli       # music front-end
    ncmpcpp       # music player
    libav         # fork of ffmpeg
    ffmpeg        # convert audio, etc.
    mp3val        # Repair mp3 songs

    # Applications

      # Terminal
    irssi         # IRC Channel
    youtube-dl

      # UI 
    # krita         # graphics editor
    # blender       # 3D creation suite
    # aseprite      

    # Tools
    # lshw          # list hardware
    mtr           # traceroute, ping
    bandwhich
    haskellPackages.graphmod
    graphviz      # Graphs builder
    sysstat       # performance tools
    atool         # Archive helper
    # gdb           # GNU Debugger
    ntfs3g        # mounting ntfs stuff
    ueberzug      # Ranger image previewer
    shotgun 

    stylish-haskell

    rofi-unwrapped  # windows switcher
    texlive.combined.scheme-full
    xbindkeys
    xkb-switch
    xorg.xkbcomp
    xorg.xmodmap
    xss-lock

    niv

    # (let neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/master.tar.gz";
    #   in import neuronSrc {})

    wine
    # sdcv
    # artha
    # scala
    pandoc

    # marble # maps
    (tor-browser-bundle-bin.override {
      extraPrefs = ''
        lockPref("browser.tabs.remote.autostart", false);
        lockPref("browser.tabs.remote.autostart.2", false);
      '';
    })
    steam
    # Broken
    # python27Packages.howdoi
    # diskonaut

    firefox 
    # tixati
    zathura jrnl anki
    gimp imagemagick libreoffice



    sxiv feh
    ifuse libimobiledevice 
    unetbootin usbmuxd woeusb
    # wlroots
    # tdesktop
    traceroute
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
