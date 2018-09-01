{ mkDerivation, fetchgit, base, containers, heaps, raft, stringbuilder, stdenv }:
mkDerivation {
  pname = "graft";
  version = "0.1.2.11";
  src = fetchgit {
    url = "git://10.40.9.156/";
    rev = "49475b5d32a3d957c00d78d444f6f2a4c647130b";
    sha256 = "1zgpc8h9766cg92a1w88bxj9bfrb60b9dkmfscp765walfgf8w5w";
  };
  libraryHaskellDepends = [ base containers heaps raft stringbuilder ];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
}
