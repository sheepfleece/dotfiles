{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # Music
    mpd           # music server
    mpv mpc_cli   # music front-end  
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
    lshw          # list hardware
    mtr           # traceroute, ping
    graphviz      # Graphs builder
    sysstat       # performance tools
    atool         # Archive helper
    # gdb           # GNU Debugger
    ntfs3g        # mounting ntfs stuff
    ueberzug      # Ranger image previewer
    shotgun       # Screenshots 
    qt5.qttools   # QT interface

    haskellPackages.graphmod
    stylish-haskell

    rofi-unwrapped  # windows switcher
    texlive.combined.scheme-full

    # X Windows
    xbindkeys
    xkb-switch
    xorg.xkbcomp
    xorg.xmodmap
    xss-lock

    niv           # Nix autoupdate

    (let neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/master.tar.gz";
      in import neuronSrc {})
    jq

    wine          # Windows layer emulator
    pandoc        # Convert documents

    # (tor-browser-bundle-bin.override {
    #   # Tor crashes without those
    #   extraPrefs = ''
    #     lockPref("browser.tabs.remote.autostart", false);
    #     lockPref("browser.tabs.remote.autostart.2", false);
    #   '';
    # })

    steam
    tdesktop

    wpa_supplicant_gui

    # Broken
    # python27Packages.howdoi
    # diskonaut

    firefox 
    zathura jrnl anki
    gimp imagemagick libreoffice

    sxiv
    # unetbootin      # ???
    # wlroots
  ];
}
