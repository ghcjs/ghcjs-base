.PHONY= update build clean buildjs

all: update build

js: clean buildjs

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build 

clean:
	cabal clean 

buildjs:
	cabal build ghcjs-base --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

testjs:
	cabal build exe:tests --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg --with-hsc2hs=javascript-unknown-ghcjs-hsc2hs
