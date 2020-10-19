{ mkDerivation, base, bytestring, case-insensitive, containers
, data-default, exceptions, fetchgit, ghcjs-dom, http-api-data
, http-media, jsaddle, mtl, network-uri, reflex, reflex-dom-core
, safe, servant, servant-auth, stdenv, string-conversions, text
, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.4";
  src = fetchgit {
    url = "https://github.com/imalsogreg/servant-reflex.git";
    sha256 = "159xb1xfj6bj6aylkarpznjqq7cg4yx3js8a3yqm8jjgh563ziga";
    rev = "dac560cdb65ffaaa78d5ab800b8aa52ed011f44a";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
    reflex-dom-core safe servant servant-auth string-conversions text
    transformers
  ];
  doHaddock = false;
  doCheck = false;
  description = "servant API generator for reflex apps";
  license = stdenv.lib.licenses.bsd3;
}
