{
  programs.fish.enable = true;
  programs.fish.shellAliases = {
    "l."   = "exa -ld .* --group-directories-first";
    "ll"   = "exa -l --group-directories-first --git";
    "la"   = "exa -l --group-directories-first --time=atime";
    "ls"   = "exa --group-directories-first";
    "free" = "free -h";
    "du"   = "du -h";
    "df"   = "df -h";
    "di"   = "df -ih";
    "cp"   = "cp -iv";
    "mv"   = "mv -iv";
    "rm"   = "rm -v";
    "mkd"  = "mkdir -pv";
    "info" = "info --vi-keys";
    "tail" = "less +F";
    "tree" = "exa --tree";
    "fg"   = "fg 1>/dev/null 2>&1";
    "cat"  = "bat";

    "j"    = "pop";   # j-pop, yes
    "jj"   = "pop 2";
    "jjj"  = "pop 3";
    "jjjj" = "pop 4";

    "yd"   = ''
      youtube-dl --yes-playlist -o $HOME'/Videos/%(playlist_title)s/%(title)s.%(ext)s'
      '';
    "yda"  = ''
      youtube-dl --yes-playlist -x --audio-quality 0
    '';
  };
  programs.fish.shellAbbrs = {
    "ns"   = "nix-shell --command fish";

    "nr"   = "sudo nixos-rebuild switch";
    "ne"   = "sudo vim /etc/nixos"; # sudoedit doesn't open directories
    "nE"   = "vim -R /etc/nixos";

    "hr"   = "home-manager switch";
    "he"   = "vim ~/.config/nixpkgs";
    "hE"   = "vim -R ~/.config/nixpkgs";

    "mnt"  = "udisksctl mount";
    "jc"   = "journalctl";
    "sc"   = "systemctl";

    "v"    = "vim -R";
    "vm"   = "vim"; # vim mutable
    "ve"   = "vim ~/.config/nixpkgs/home/vimrc.vim";
    "vs"   = "sudoedit";
    "ka"   = "killall";
    "r"    = "ranger";

    "wd"   = "pwd";  # p is hard to type
    "to"   = "htop"; # any problems with that?
    "ot"   = "opt";

    # Notice spaces. 
    "note" = " jrnl Note.";
    "day"  = " jrnl A though of the day.";

    # Spawn a new terminal instance.
    "c"    = "$TERM & ; disown";

    "cb" = "cabal build";
    "cr" = "cabal run";
    "cg" = "cabal repl"; # ghci

    "sb" = "stack build";
    "sr" = "stack run";

    "nc" = "ncmpcpp";

    # list sizes
    "lss"  = "du -sh * | sort -rh | column -t";

    # query system-wide packages
    "nq"   = ''
      find /run/current-system/sw/bin/ -type l -exec readlink {} \; | sed -E 's|[^-]+-([^/]+)/.*|\1|g' | sort -u
    '';
  };

  programs.fish.shellInit = ''
    bind \co 'fg 1>&2 2>/dev/null ; commandline -f repaint' 

    set -x DIRENV_LOG_FORMAT ""
    set -x fish_color_error 'e27878'
    set -x fish_color_command '84a0c6'
    set -x fish_color_param '84a0c6'
    set -x fish_color_quote 'e2a478'
    set -x fish_color_operator 'e2a478'
    set FZF_DEFAULT_COMMAND 'fd --type f'
    # set PATH $HOME/dotfiles/scripts $PATH
    eval (direnv hook fish)
  '';
}
