{ mkDerivation, aeson, base, bytestring, cmdargs, containers, daft
, data-default, directory, file-embed, filepath, MonadRandom
, monoid-extras, mtl, parallel, pqueue, raft, regex-posix, split
, statistics, stdenv, template-haskell, text, tostring, vinyl, void
, yaml, zip-archive
}:
mkDerivation {
  pname = "sera";
  version = "3.1.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers daft data-default file-embed
    MonadRandom monoid-extras mtl parallel pqueue raft regex-posix
    split statistics template-haskell text tostring vinyl void yaml
    zip-archive
  ];
  executableHaskellDepends = [
    aeson base bytestring cmdargs containers daft data-default
    directory file-embed filepath monoid-extras mtl pqueue raft
    regex-posix split statistics template-haskell text tostring vinyl
    void yaml zip-archive
  ];
  enableSharedExecutables = false;
  doHaddock = false;
  homepage = "https://github.nrel.gov/haskell/sera";
  description = "the synopsis";
  license = stdenv.lib.licenses.unfree;
}
