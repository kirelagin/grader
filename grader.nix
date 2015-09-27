{ mkDerivation, aeson, base, bytestring, containers, directory
, email-validate, encoding, filepath, gitlib, gitlib-libgit2, mime
, mtl, sandi, stdenv, tagged, text, text-icu, time, transformers
, unix, yaml
}:
mkDerivation {
  pname = "grader";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory email-validate filepath
    gitlib gitlib-libgit2 mtl tagged text time transformers unix yaml
  ];
  executableHaskellDepends = [
    base bytestring containers directory email-validate encoding
    filepath mime mtl sandi text text-icu transformers unix
  ];
  description = "Grader for programming assignments";
  license = stdenv.lib.licenses.bsd3;
}
