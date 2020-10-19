{ mkDerivation, aeson, base, base58string, binary, bitcoin-block
, bitcoin-script, bitcoin-tx, bitcoin-types, bytestring, exceptions
, fetchgit, hexstring, hspec, http-client, http-types, lens
, lens-aeson, stdenv, text, unordered-containers, wreq
}:
mkDerivation {
  pname = "bitcoin-api";
  version = "0.13.0";
  doCheck = false;
  src = fetchgit {
    url = "https://github.com/hexresearch/haskell-bitcoin-api";
    sha256 = "17bys057871bagx01l8ydx30fprjnyj5l4nxw75zj3kqda8m2jgk";
    rev = "b43daa284c712236f2b3260aaf2205e7e2af0852";
    fetchSubmodules = true;
  };
  /* src = ../../haskell-bitcoin-api; */
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base base58string binary bitcoin-block bitcoin-script
    bitcoin-tx bitcoin-types bytestring exceptions hexstring
    http-client http-types lens lens-aeson text unordered-containers
    wreq
  ];
  testHaskellDepends = [
    base base58string bitcoin-script bitcoin-tx bytestring hspec
    http-client lens text wreq
  ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Provides access to the RPC API of Bitcoin Core";
  license = stdenv.lib.licenses.mit;
}
