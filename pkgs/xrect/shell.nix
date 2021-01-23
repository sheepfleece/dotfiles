{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  xrect = pkgs.callPackage ./default.nix {};
in
  pkgs.mkShell {
    buildInputs = [ xrect ];
  }

