{ mkDerivation, aeson, base, bytestring, containers, directory
, email-validate, filepath, gitlib, gitlib-libgit2, monad-logger
, mtl, stdenv, text, time, transformers, unix, yaml
}:
mkDerivation {
  pname = "grader";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory email-validate filepath
    gitlib gitlib-libgit2 monad-logger mtl text time transformers unix
    yaml
  ];
  executableHaskellDepends = [
    base containers email-validate mtl text
  ];
  description = "Grader for programming assignments";
  license = stdenv.lib.licenses.bsd3;
}
