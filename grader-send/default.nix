with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "grader-send";

  src = ./.;

  buildInputs = with python3Packages; [
    python3
    GitPython
  ];
}
