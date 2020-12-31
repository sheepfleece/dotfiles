{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./mpv.nix
      ./vim.nix
      ./fish.nix
      ./packages.nix
    ];

  # Copy everything from `./config` into `~/.config`
  xdg.configFile."." = {
    source = ./config;
    recursive = true;
  };

  # Copy everything from `./home` into `~` and prepend `.` to it.
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
