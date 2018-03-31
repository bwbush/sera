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
              daft       = haskellPackagesNew.callPackage ../../../code.functionally.io/daft/default.nix       { };
              graft      = haskellPackagesNew.callPackage ../../../code.functionally.io/graft/default.nix      { };
              singletons = haskellPackagesNew.callPackage ./singletons.nix { };
              heaps      = haskellPackagesNew.callPackage ./heaps.nix      { };
            };
          };
        };
      };
    };
  };

  fetchNixpkgs = import /home/bbush/.config/nixpkgs/fetchNixpkgs.nix;
  pin1709  = import (
    fetchNixpkgs {
      rev = "b62c50ce5d3b6053f6f4afa10f2c4013ac0bfe9c";
      sha256 = "0maw671jf54nx6gdlqhr5srl8kk78951mj847r325824f5bg8rsj";
    }
  );
  pkgs = pin1709 { inherit config; };

in
  {
    sera = pkgs.haskell.packages.${compiler}.sera;
  }
