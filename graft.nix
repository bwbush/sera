{ mkDerivation, fetchgit, base, containers, heaps, raft, stringbuilder, stdenv }:
mkDerivation {
  pname = "graft";
  version = "0.1.2.0";
  src = fetchgit {
    url = "git://10.40.9.156/";
    rev = "8eeeeead6fb734cf2b43f77e4245d85fb83193bf";
    sha256 = "181c1yml0zl7qr6wb6fq6lihj3bhkwh02bl5xjfdy0jcp1wa2mlw";
  };
  libraryHaskellDepends = [ base containers heaps raft stringbuilder ];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
}
