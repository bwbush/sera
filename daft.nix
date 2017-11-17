{ mkDerivation, aeson, base, bytestring, containers, data-default
, deepseq, fetchgit, hashable, mtl, raft, stdenv, text, tostring
, type-list, unordered-containers, vinyl
}:
mkDerivation {
  pname = "daft";
  version = "0.4.14.3";
  src = fetchgit {
    url: "https://github.nrel.gov/haskell/daft.git";
    rev: "ebcc5e955beef198a6dd56f62cdf7caebc0ed56e";
    sha256: "0b5zvgpf1fsiljy7c9fkygakvp8249cvx6hyza3mnham7zixkxi7";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers data-default deepseq hashable mtl
    raft text tostring type-list unordered-containers vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
{
  "url": "https://github.nrel.gov/haskell/daft.git",
  "rev": "ebcc5e955beef198a6dd56f62cdf7caebc0ed56e",
  "date": "2017-11-17T09:02:32-07:00",
  "sha256": "0b5zvgpf1fsiljy7c9fkygakvp8249cvx6hyza3mnham7zixkxi7",
  "fetchSubmodules": true
}
