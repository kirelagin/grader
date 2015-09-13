{ mkDerivation, aeson, base, containers, directory, email-validate
, extra, filepath, gitlib, mtl, stdenv, text, unix, yaml
}:
mkDerivation {
  pname = "grader";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base containers directory email-validate extra filepath
    gitlib mtl text unix yaml
  ];
  description = "Grader for programming assignments";
  license = stdenv.lib.licenses.bsd3;
}
