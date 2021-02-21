{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
  };

  home.packages = with pkgs; let

    graphics = [
      gimp          # image manipulator!
      grafx2 # Bitmap paint
      krita         # graphics editor
      blender       # 3D creation suite
      aseprite      
      darktable
      obs-studio

      wesnoth
    ];


    x-related = [
      xkb-switch
      xorg.xkbcomp
      xss-lock
      xbindkeys
      xorg.xmodmap
      slop

      feh
      rofi-unwrapped  # windows switcher
      shotgun         # Screenshots 
    ];

    utils = [
      pass
      sublime3

      enca # guess and change encodings ; enca -l ru -x UTF-8 <FILE>
      ipfs # decentralized web
      wgetpaste # send file to paste service
      nix-index # search for derivation with specific folder
      doas # sudo alternative
      gnome3.gnome-sound-recorder # record my beautiful voice

      exa           # ls replacement in Rust
      ffmpeg        # convert audio, etc.
      # libav         # fork of ffmpeg

      tldr          # man with examples
      file          # types of files
      sysstat       # iostat, pidstat
      atool         # Archive helper
      ueberzug      # Ranger image previewer

      niv           # Nix autoupdate
      vimpager-latest   # print vim-highlited text

      texlive.combined.scheme-full


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
      beets         # music tagging

      imagemagick  # reverse and edit image from terminal

      highlight    # ???
      gron        # greppable JSON
      ripgrep-all # greppable pdfs, images, subtitles, all

      gnupg

      # ranger previewers
      transmission      # .torrent
      poppler_utils     # .pdf
      djvulibre         # .djvu
      ffmpegthumbnailer # .mkv | .mp4 | ...

      parallel-full # execute jobs in parallel 
      kid3        # audio tagger
      wine
      steam-run-native

      httpie
      curlie
    ]; 

    unusedUtils = [
      gitAndTools.gitflow # opinionated branch manager
      kdeApplications.kmag # Screen magnifier
      libxml2       # xmllint
      calibre           # .epub
      jq          # neuron needs it

      gdb           # GNU Debugger
      httrack     # web-site mirroring
      cshatag     # silent data corruption detecter
      mp3val        # Repair mp3 songs
      unixtools.xxd
    ];

    social = [
      tdesktop
      # signal-desktop
    ];

    apps = [
      artha
      skype
      sent          # presentations
      lynx          # web browser
      deluge        # Torrent client
      mpd           # music server
      ncmpcpp       # music player
      (zathura.override {
        # useMupdf = false;
      })
      sxiv        # Simple X Image Viewer
      anki        # cards
      torsocks
      jrnl        # diary
      irssi         # IRC Channel
    ];
    unusedApps = [

      jq neuron      # bigger notes

      khal          #  calendar
      libreoffice   #  documents and presentations
      gnome3.cheese #  webcam
      jitsi         #  webconferences
      joplin      # notes
      newsboat      # RSS Feed
      mysql-workbench #  Schema builder
      testdisk        #  file recovery
      extundelete     #  file recovery
      mpv 
      mpc_cli     
    ];
  in 
    apps ++ social ++ utils ++ x-related ++ graphics;
}
