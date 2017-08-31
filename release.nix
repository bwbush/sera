{ compiler ? "ghc7103" }:

let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              sera =
                haskellPackagesNew.callPackage ./default.nix { };
              daft =
                haskellPackagesNew.callPackage ../daft/default.nix { };
              singletons =
                haskellPackagesNew.callPackage ../daft/singletons.nix { };
              raft =
                haskellPackagesNew.callPackage ../raft/default.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    sera = pkgs.haskell.packages.${compiler}.sera;
  }
