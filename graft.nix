{ mkDerivation, fetchgit, base, containers, heaps, raft, stringbuilder, stdenv }:
mkDerivation {
  pname = "graft";
  version = "0.1.2.10";
  src = fetchgit {
    url = "git://10.40.9.156/";
    rev = "6b4a8e6f66765f5053cdceaced990ac613bafd9f";
    sha256 = "14h6w21yypzqipih6b1gxxz52nwx2aw0cnfjb81x7r52j7m7hdnq";
  };
  libraryHaskellDepends = [ base containers heaps raft stringbuilder ];
  homepage = "https://bitbucket.org/functionally/graft";
  description = "Graph algorithms";
  license = stdenv.lib.licenses.mit;
}
