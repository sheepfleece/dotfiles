{ pkgs, ... }:
{
  programs.firefox = {
    enable = true;
  };

  home.packages = with pkgs; let

    neuron = let 
      neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/master.tar.gz";
    in import neuronSrc {};

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
      shotgun       # Screenshots 
    ];

    utils = [
      ffmpeg        # convert audio, etc.
      libav         # fork of ffmpeg
      mp3val        # Repair mp3 songs
      tldr

      file
      sysstat       # performance tools
      atool         # Archive helper
      gdb           # GNU Debugger
      ueberzug      # Ranger image previewer

      niv           # Nix autoupdate
      vimpager-latest      # print vim-highlited text

      texlive.combined.scheme-full

      httrack     # web-site mirroring
      cshatag     # silent data corruption detecter

      youtube-dl

      wine          # Windows layer emulator
      pandoc        # Convert documents

      usbutils      # lsusb and similar
      lshw          # list hardware
      mtr           # traceroute, ping
      ntfs3g        # mounting ntfs stuff
      go-mtpfs      # connect Android devices
      qt5.qttools   # QT interface

      graphviz      # Graphs builder
      haskellPackages.graphmod

      stylish-haskell
      wpa_supplicant_gui

      steam-run     # environment for games
      tmsu          # file tagging

      imagemagick 
      highlight
      gron        # greppable JSON
      jq          # neuron needs it

      testdisk    # file recovery
      extundelete # file recovery

      mpdscribble

      # ranger previewers
      transmission      # .torrent
      poppler_utils     # .pdf
      djvulibre         # .djvu
      ffmpegthumbnailer # .mkv | .mp4 | ...
      calibre           # .epub
      gitAndTools.gitflow
    ]; 

    # mpv mpc_cli   # music front-end  
    apps = [
      khal
      steam
      lynx            # web browser
      mysql-workbench # Schema builder
      # tdesktop        # Messaging app
      gnome3.cheese   # webcam

      gimp            # image manipulator!
      libreoffice    

      irssi         # IRC Channel
      newsboat      # RSS Feed

      deluge        # Torrent client

      mpd           # music server
      ncmpcpp       # music player

      (zathura.override {
        useMupdf = false;
      })

      sxiv        # Simple X Image Viewer
        
      jrnl        # diary
      anki        # cards
      joplin      # notes
      neuron      # bigger notes
    ];
  in 
    apps ++ utils ++ x-related;
}
