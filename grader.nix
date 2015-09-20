{ mkDerivation, aeson, base, bytestring, containers, directory
, email-validate, exceptions, extra, filepath, gitlib
, gitlib-libgit2, monad-control, monad-logger, mtl, stdenv, text
, time, transformers, transformers-base, unix, yaml
}:
mkDerivation {
  pname = "grader";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory email-validate
    exceptions extra filepath gitlib gitlib-libgit2 monad-control
    monad-logger mtl text time transformers transformers-base unix yaml
  ];
  executableHaskellDepends = [
    base containers email-validate mtl text
  ];
  description = "Grader for programming assignments";
  license = stdenv.lib.licenses.bsd3;
}
