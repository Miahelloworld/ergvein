self: super:
let pkgs = super;
in rec {
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  secp256k1Sys = self.callPackage ../derivations/secp256k1Sys.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdbSys.nix {};
  leveldb = self.callPackage ../derivations/leveldb.nix {};
  # Haskell overlay
  haskell = super.haskell // {
    packageOverrides = haskOverrides;
  };
  haskOverrides = hsNew: hsOld:
    let
      lib = pkgs.haskell.lib;
      hschainRev = {
        "url" = "https://github.com/hexresearch/hschain";
        "rev" = "dfb3f81379b984c6d9288e93b90d8d29179fa191";
        "ref" = "master";
      };
      callHSChain = name: hsNew.callCabal2nixWithOptions name
        (builtins.fetchGit hschainRev)
        ("--subpath " + name)
        {};
      # Hschain-utxo
      hschainUtxoRev = {
        "url" = "git@github.com:hexresearch/hschain-utxo.git";
        "rev" = "595104ac8d0889652c6c8e5477933f7b073c94b1";
        "ref" = "master";
      };
      callHSChainUtxo = name: hsNew.callCabal2nixWithOptions name
        (builtins.fetchGit hschainUtxoRev)
        ("--subpath " + name)
        {};
    in
      {
        hschain-control  = callHSChain "hschain-control";
        hschain-config   = callHSChain "hschain-config";
        hschain-mempool  = callHSChain "hschain-mempool";
        hschain-merkle   = callHSChain "hschain-merkle";
        hschain-logger   = callHSChain "hschain-logger";
        hschain-crypto   = callHSChain "hschain-crypto";
        hschain-types    = callHSChain "hschain-types";
        hschain-net      = callHSChain "hschain-net";
        hschain-db       = callHSChain "hschain-db";
        hschain-PoW      = callHSChain "hschain-PoW";
        # UTXO
        hex-common                = callHSChainUtxo "hex-common";
        hindley-milner-type-check = callHSChainUtxo "hindley-milner-type-check";
        hschain-utxo-lang         = callHSChainUtxo "hschain-utxo-lang";
        hschain-utxo-pow-node     = callHSChainUtxo "hschain-utxo-pow-node";
        hschain-pow-func          =
          let drv = callHSChainUtxo "hschain-pow-func";
          in
            lib.appendConfigureFlags drv
              [ "-f-use-pkg-config"
                "--extra-lib-dirs=${pkgs.openssl.out.out}/lib"
                "--extra-include-dirs=${pkgs.openssl.out.dev}/include"
              ];
        # Newer versions
        semigroups    = hsNew.callPackage ../derivations/hackage-semigroups.nix {};
        direct-sqlite = hsNew.callPackage ../derivations/hackage-direct-sqlite.nix {};
        sqlite-simple = hsNew.callPackage ../derivations/hackage-sqlite-simple.nix {};
        data-fix      = hsNew.callPackage ../derivations/hackage-data-fix.nix {};
        vector        = hsNew.callPackage ../derivations/hackage-vector.nix {};
      };
}
