{ mkDerivation, aeson, base, bytestring, containers, directory
, email-validate, encoding, filepath, gitlib, gitlib-libgit2, mtl
, sandi, stdenv, tagged, text, text-icu, time, transformers, unix
, yaml
}:
mkDerivation {
  pname = "grader";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory email-validate encoding
    filepath gitlib gitlib-libgit2 mtl sandi tagged text time
    transformers unix yaml
  ];
  executableHaskellDepends = [
    base bytestring containers directory email-validate encoding
    filepath mtl text text-icu transformers unix
  ];
  description = "Grader for programming assignments";
  license = stdenv.lib.licenses.bsd3;
}
