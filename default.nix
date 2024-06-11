{ mkDerivation, base, lib }:
mkDerivation {
  pname = "adventofcode16-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "adventofcode16-haskell";
}
