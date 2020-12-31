{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
  };

  home.packages = with pkgs; let

    # neuron = let 
    #   neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/master.tar.gz";
    # in import neuronSrc {};

    graphics = [
      krita         # graphics editor
      blender       # 3D creation suite
      aseprite      
      darktable
    ];

    x-related = [
      xkb-switch
      xorg.xkbcomp
      xss-lock
      xbindkeys
      xorg.xmodmap

      feh
      rofi-unwrapped  # windows switcher
      shotgun         # Screenshots 
    ];

    utils = [
      doas
      SDL_mixer # for fheroes2
      tdesktop
      gnome3.gnome-sound-recorder
      modemmanager
      networkmanager

      exa           # ls replacement in Rust
      libxml2       # xmllint
      ffmpeg        # convert audio, etc.
      libav         # fork of ffmpeg

      tldr          # man with examples
      file          # types of files
      sysstat       # iostat, pidstat
      atool         # Archive helper
      ueberzug      # Ranger image previewer

      niv           # Nix autoupdate
      vimpager-latest   # print vim-highlited text

      texlive.combined.scheme-full

      gitAndTools.gitflow # opinionated branch manager

      youtube-dl  # download video
      gallery-dl  # download photo

      pandoc        # Convert documents

      usbutils      # lsusb and similar
      lshw          # list hardware
      mtr           # traceroute, ping
      ntfs3g        # mounting ntfs stuff
      go-mtpfs      # connect Android devices
      qt5.qttools   # QT interface

      graphviz      # Graphs builder
      haskellPackages.graphmod # Haskell Modules graphs

      stylish-haskell    # haskell code prettifier
      wpa_supplicant_gui # wifi connecter

      tmsu          # file tagging

      imagemagick  # reverse and edit image from terminal

      highlight    # ???
      gron        # greppable JSON
      ripgrep-all # greppable pdfs, images, subtitles, all

      mpdscribble
      gnupg

      # ranger previewers
      transmission      # .torrent
      poppler_utils     # .pdf
      djvulibre         # .djvu
      ffmpegthumbnailer # .mkv | .mp4 | ...

      parallel-full # execute jobs in parallel 
      wine          # Windows layer emulator
      winetricks
      steam-run     # environment for games
      kid3        # audio tagger
    ]; 

    unusedUtils = [
      calibre           # .epub
      jq          # neuron needs it

      gdb           # GNU Debugger
      httrack     # web-site mirroring
      cshatag     # silent data corruption detecter
      mp3val        # Repair mp3 songs
      unixtools.xxd
    ];

    games = [
      steam
    ];

    apps = [
      lynx          #  web browser
      gimp          #  image manipulator!
      deluge        # Torrent client
      mpd           # music server
      ncmpcpp       # music player
      (zathura.override {
        useMupdf = false;
      })
      sxiv        # Simple X Image Viewer
      anki        # cards

    ];
    unusedApps = [
      jq neuron      # bigger notes

      khal          #  calendar
      libreoffice   #  documents and presentations
      gnome3.cheese #  webcam
      jitsi         #  webconferences
      jrnl        # diary
      joplin      # notes
      irssi         # IRC Channel
      newsboat      # RSS Feed
      mysql-workbench #  Schema builder
      testdisk        #  file recovery
      extundelete     #  file recovery
      mpv 
      mpc_cli     
    ];
  in 
    apps ++ games ++ utils ++ x-related;
}
