{ mkDerivation, base, Cabal, cabal-doctest, directory, doctest
, filepath, stdenv
}:
mkDerivation {
  pname = "heaps";
  version = "0.3.5";
  sha256 = "1p1nsglsf8hric63cn3n1iw1nlbiv3lgk3n5gq0znajj7j7s64qv";
  revision = "1";
  editedCabalFile = "05avm1b16gj3rlm9sjqkxb0flq055r6gqhnacp7yzw4j1bghm5j7";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base directory doctest filepath ];
  doCheck = false;
  homepage = "http://github.com/ekmett/heaps/";
  description = "Asymptotically optimal Brodal/Okasaki heaps";
  license = stdenv.lib.licenses.bsd3;
}
