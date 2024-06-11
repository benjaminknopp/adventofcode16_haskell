{ mkDerivation, base, bytestring, containers, cryptonite, ghc, lib
, regex-pcre, regex-posix, sort, split
}:
mkDerivation {
  pname = "adventofcode16-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers cryptonite ghc regex-pcre regex-posix
    sort split
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "adventofcode16-haskell";
}
