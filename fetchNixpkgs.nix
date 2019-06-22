# See <https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH>.
# 
# Example:
#
#### let
####   fetchNixpkgs = import ./fetchNixpkgs.nix;
#### 
####   nixpkgs = fetchNixpkgs {
####      rev          = "3389f23412877913b9d22a58dfb241684653d7e9";
####      sha256       = "1zf05a90d29bpl7j56y20r3kmrl4xkvg7gsfi55n6bb2r0xp2ma5";
####      outputSha256 = "0wgm7sk9fca38a50hrsqwz6q79z35gqgb9nw80xz7pfdr4jy9pf8";
####   };
#### 
####   pkgs = import nixpkgs { config = {}; };
#### in
####   ...

{ rev                             # The Git revision of nixpkgs to fetch
#, sha256                          # The SHA256 of the downloaded data
, system ? builtins.currentSystem # This is overridable if necessary
}:

builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}

