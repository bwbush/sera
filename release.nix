{ compiler ? "ghc7103" }:

let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              sera       = haskellPackagesNew.callPackage ./default.nix    { };
              daft       = haskellPackagesNew.callPackage ../daft/default.nix       { };
              graft      = haskellPackagesNew.callPackage ../graft/default.nix      { };
              singletons = haskellPackagesNew.callPackage ./singletons.nix { };
              heaps      = haskellPackagesNew.callPackage ./heaps.nix      { };
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
