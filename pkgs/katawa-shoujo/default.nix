{ stdenv, autoPatchelfHook, renpy, python2 }:

stdenv.mkDerivation rec {
  pname = "katawa-shoujo";
  version = "1.3.1";

  src = ./src;

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ python2 renpy ];

  # configurePhase = "true";
  # buildPhase = "true";

  installPhase = ''
    mkdir -p "$out/bin"
    cp -r * "$out"
  '';

}
