configure:
	cabal configure --enable-profiling --enable-library-profiling

reinstall-daft:
	-killall hdevtools
	cabal sandbox hc-pkg unregister daft
	cabal install --ghc-option=-fprof-auto -p --enable-executable-profiling ../../daft
	cabal clean

record-versions:
	cabal freeze
	mv cabal.config sera.config

profile-examples: profile-example-1 profile-example-2

profile-example-1:
	cabal --jobs=8 run --ghc-options="-prof -auto-all -caf-all" -- +RTS -p -RTS stock examples/vehicle-stock/configuration.yaml

profile-example-2:
	cabal --jobs=8 run --ghc-options="-prof -auto-all -caf-all" -- +RTS -p -RTS invert-stock examples/invert-vehicle-stock/configuration.yaml

profile-vision:
	cabal --jobs=8 run --ghc-options="-prof -auto-all -caf-all" -- +RTS -p -RTS stock vision/configuration.yaml
	mv sera.prof vision/
