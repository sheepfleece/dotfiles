{ stdenv, fetchFromGitHub, fetchurl
, SDL2_mixer, SDL2_ttf, SDL2_image, SDL2
, zlib, gettext
, unzip
}:


stdenv.mkDerivation rec {
  pname = "fheroes2";
  version = "0.9.0";

  buildInputs = [ SDL2_mixer SDL2_ttf SDL2_image SDL2 ];
  nativeBuildInputs = [ zlib unzip gettext ];
  makeFlags = [ 
    "WITH_SDL2=1"
  ];

  src = fetchFromGitHub {
    owner = "ihhub";
    repo = pname;
    rev = "b069e0711307a8acaa0dc414eab544ba6c971f73";
    sha256 = "sha256-fDsDu+h1vhs1kfsj7Z+HVZS1IMOslDj5NIsg6M2iZyU=";
  };

  game = fetchurl {
    url = "https://archive.org/download/HeroesofMightandMagicIITheSuccessionWars_1020/h2demo.zip";
    sha256 = "sha256-EgSMiwOHXIHmlTSjgTqvY0CXXne3YtwbeaT/VRQkDjw=";
  };

  installPhase = ''
    mkdir -p $out/bin
    unzip ${game} -d $out
    mv ./fheroes2 $out

    cat 1>$out/bin/fheroes2 <<EOF
    #!/usr/bin/env bash
    $out/fheroes2
    EOF

    chmod +x $out/bin/fheroes2

  '';
}
