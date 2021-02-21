{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  katawa-shoujo = pkgs.callPackage ./default.nix {};
in
  pkgs.mkShell {
    buildInputs = [ katawa-shoujo ];
  }

