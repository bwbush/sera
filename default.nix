{
  mkDerivation, stdenv
, aeson, base, bytestring, cmdargs, containers, daft, data-default, directory, file-embed, filepath, graft, hmatrix-glpk, logging-effect, MonadRandom, monoid-extras, mtl, parallel, pqueue, raft, regex-posix, split, statistics, template-haskell, text, tostring, vinyl, void, yaml, zip-archive
, heaps, monoid-absorbing, non-negative, stringbuilder, fgl, fgl-arbitrary, QuickCheck
}:
mkDerivation {
  pname = "sera";
  version = "3.3.1.14";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers daft data-default file-embed hmatrix-glpk logging-effect MonadRandom monoid-extras mtl parallel pqueue raft regex-posix split statistics template-haskell text tostring vinyl void yaml zip-archive
  ];
  executableHaskellDepends = [
    aeson base bytestring cmdargs containers daft data-default directory file-embed filepath hmatrix-glpk logging-effect monoid-extras mtl pqueue raft regex-posix split statistics template-haskell text tostring vinyl void yaml zip-archive
    heaps monoid-absorbing non-negative stringbuilder fgl fgl-arbitrary QuickCheck
  ];
  enableSharedExecutables = false;
  doHaddock = false;
  homepage = "https://github.nrel.gov/haskell/sera";
  description = "the synopsis";
  license = stdenv.lib.licenses.unfree;
}
