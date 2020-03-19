self: super: rec
{
  android-activity = self.callPackage ../derivations/android-activity.nix {};
  /* jdk = self.callPackage ./derivations/jdk.nix {}; */
  secp256k1 = self.callPackage ../derivations/secp256k1.nix {};
  zlibSys = self.callPackage ../derivations/zlibSys.nix {};
  lmdbSys = self.callPackage ../derivations/lmdb.nix {};

  postgresql_ergvein = let 
    pg = (import ../derivations/postgresql self).postgresql_9_6;
    in pg.override { this = postgresql_ergvein; };
}
