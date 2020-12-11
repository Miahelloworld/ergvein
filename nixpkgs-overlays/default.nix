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
      hschainRev = {
        "url" = "https://github.com/hexresearch/hschain";
        "rev" = "4fa995363cc5d88da69927aefe990b8a110eb71c";
        "ref" = "master";
      };
      callHSChain = name: hsNew.callCabal2nixWithOptions name
        (builtins.fetchGit hschainRev)
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
        # Newer versions
        semigroups    = hsNew.callPackage ../derivations/hackage-semigroups.nix {};
        direct-sqlite = hsNew.callPackage ../derivations/hackage-direct-sqlite.nix {};
        sqlite-simple = hsNew.callPackage ../derivations/hackage-sqlite-simple.nix {};
        data-fix      = hsNew.callPackage ../derivations/hackage-data-fix.nix {};
        vector        = hsNew.callPackage ../derivations/hackage-vector.nix {};
      };
}
