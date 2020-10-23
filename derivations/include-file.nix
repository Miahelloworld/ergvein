{ mkDerivation, base, bytestring, criterion, random, stdenv
, template-haskell
}:
mkDerivation {
  pname = "include-file";
  version = "0.1.0.3";
  sha256 = "208f1f3bdc717f5f953cb7c9935c84d6a6291b7cd5ed8a22fa8567184be33d29";
  setupHaskellDepends = [ base random];
  libraryHaskellDepends = [
    base bytestring random template-haskell
  ];
  testHaskellDepends = [ base bytestring ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  description = "Inclusion of files in executables at compile-time";
  license = stdenv.lib.licenses.bsd3;
}