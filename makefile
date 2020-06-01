#TODO it's 2020 - use Shake or something

HS_SRC_DIRS = $(shell find haskell/src -type d)
HS_SRC_FILES = $(shell find haskell/src -type f -name '*')

ELM_SRC_DIRS = $(shell find elm/src -type d)
ELM_SRC_FILES = $(shell find elm/src -type f -name '*')

RSC_DIRS = $(shell find haskell/rsc -type d)
RSC_FILES = $(shell find haskell/rsc -type f -name '*')

# TODO there's something weird going on here - this really shouldn't install 'web-gamepad-linux'...
dist/web-gamepad-test: elm $(RSC_DIRS) $(RSC_FILES) $(HS_SRC_DIRS) $(HS_SRC_FILES) haskell/web-gamepad.cabal
	mkdir -p dist
	cd haskell && cabal install web-gamepad-test --installdir ../dist --install-method copy --overwrite always --flags="release"

dist/debug/web-gamepad-test: elm $(RSC_DIRS) $(RSC_FILES) $(HS_SRC_DIRS) $(HS_SRC_FILES) haskell/web-gamepad.cabal
	mkdir -p dist/debug
	cp -r haskell/rsc dist/debug
	cd haskell && cabal install web-gamepad-test --installdir ../dist/debug --install-method copy --overwrite always

haskell/rsc/dist/elm.js: $(ELM_SRC_DIRS) $(ELM_SRC_FILES) elm/elm.json
	cd elm && elm make src/Main.elm --optimize --output ../haskell/rsc/dist/elm.js

# just aliases
debug: dist/debug/web-gamepad-test
elm: haskell/rsc/dist/elm.js
clean:
	rm -rf haskell/rsc/dist
