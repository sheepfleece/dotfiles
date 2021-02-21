{ stdenv, fetchFromGitHub
, cmake, git, dpkg
, boost
}:


stdenv.mkDerivation rec {
  pname = "cpp-dependencies";
  version = "0.8.2";

  buildInputs = [ boost ];
  nativeBuildInputs = [ cmake git dpkg ];

  src = fetchFromGitHub {
    owner = "tomtom-international";
    repo = pname;
    rev = "515f82b1fdd5b05aa18f1f41311b9e4294d262bf";
    sha256 = "sha256-OYXjixXZh1AaU/wj8xB7DaD/Rq+AUHqjv8REL6bg18o=";
  };
}
