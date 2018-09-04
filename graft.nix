{
  mkDerivation, stdenv, fetchgit
, base, containers, heaps, monoid-absorbing, mtl, non-negative, raft, stringbuilder
, fgl, fgl-arbitrary, hmatrix-glpk, QuickCheck
}:
mkDerivation {
  pname = "graft";
  version = "0.2.1.0";
  src = fetchgit {
    url = "git://github.com/bwbush/graft.git";
  # url = "git://bitbucket.org/functionally/graft.git";
    rev = "64f4224c4eea3b5165f66980b25a36fb9b81b5b7";
    sha256 = "0ixczmpi06sdm2a8kw7y1skqsjmd8npiij1n2csv50qpax6qbw8v";
  };
  libraryHaskellDepends = [
    base containers heaps monoid-absorbing mtl non-negative raft stringbuilder
  ];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
  testHaskellDepends = [
    fgl fgl-arbitrary hmatrix-glpk QuickCheck
  ];
  doCheck = false;
  doHaddock = false;
}
