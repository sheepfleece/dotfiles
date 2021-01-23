{ stdenv, fetchurl, libX11, libXrandr, libXrender, xorgproto }:

stdenv.mkDerivation rec {
  pname = "xrect";
  version = "0.1";

  src = ./src;  

  buildPhase = ''
    gcc "$src/main.c" -o ./xrect  -lX11
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    cp ./xrect "$out/bin/"
  '';

  buildInputs = [ libX11 ];
}
