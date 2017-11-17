{ mkDerivation, aeson, base, bytestring, containers, data-default
, deepseq, fetchgit, hashable, mtl, raft, stdenv, text, tostring
, type-list, unordered-containers, vinyl
}:
mkDerivation {
  pname = "daft";
  version = "0.4.14.3";
  src = fetchgit {
    url = "https://github.nrel.gov/haskell/daft.git";
    sha256 = "0q80m3pq2jmv24kxkx9f3kqlz1q86204m786hhn4p1c6adan0i7z";
    rev = "9927628f63b5f3f868604769474734d21584a503";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers data-default deepseq hashable mtl
    raft text tostring type-list unordered-containers vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
