{ mkDerivation, base, bytestring, containers, directory, filepath
, lib, megaparsec, optparse-applicative, text
}:
mkDerivation {
  pname = "yip";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers directory filepath megaparsec
    optparse-applicative text
  ];
  description = "A very simple preprocessor";
  license = lib.licenses.agpl3Plus;
  mainProgram = "yip";
}
