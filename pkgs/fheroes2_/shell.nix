{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  package = pkgs.callPackage ./default.nix {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [ package ];
  }

