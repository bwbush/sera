{
  mkDerivation, stdenv, fetchgit
, aeson, base, bytestring, containers, data-default, deepseq, hashable, mtl, raft, text, tostring, type-list, unordered-containers, vinyl
}:
mkDerivation {
  pname = "daft";
  version = "0.4.14.6";
  src = fetchgit {
    url = "git://github.com/NREL/daft.git";
    rev = "920ed86232141d576e6bfc2ebbc68d7a1c8d69f2";
    sha256 = "1rk68sys0db0q4p90lnz3nkk1dcfk8w06jas40mf76q2ji0nk844";
  };
  preConfigure = ''
    mv daft/* .
  '';
  libraryHaskellDepends = [
    aeson base bytestring containers data-default deepseq hashable mtl raft text tostring type-list unordered-containers vinyl
  ];
  doCheck = false;
  doHaddock = false;
  license = stdenv.lib.licenses.mit;
}
