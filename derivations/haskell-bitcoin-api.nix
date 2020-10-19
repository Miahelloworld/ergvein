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
    sha256 = "1xl920mcqkz6h20xh3w2xf3igh743c6c2d5sykfar5m8yc9f3h6i";
    rev = "0410140fee69ad42fe2467f453137badf9e26244";
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
