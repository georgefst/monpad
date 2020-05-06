#TODO it's 2020 - use Shake or something

HS_SRC_DIRS = $(shell find haskell/src -type d)
HS_SRC_FILES = $(shell find haskell/src -type f -name '*')

ELM_SRC_DIRS = $(shell find elm/src -type d)
ELM_SRC_FILES = $(shell find elm/src -type f -name '*')

MANUAL_DIRS = $(shell find src-other -type d)
MANUAL_FILES = $(shell find src-other -type f -name '*')

dist/web-gamepad-test: elm/build/elm.js src-other/ $(MANUAL_FILES) $(MANUAL_FILES) haskell/src/ $(HS_SRC_DIRS) $(HS_SRC_FILES) haskell/server.cabal
	cd haskell && cabal v2-build web-gamepad-test
	find haskell/dist-newstyle -name 'web-gamepad-test' -type f -exec cp {} dist \;

elm/build/elm.js: elm/src/ $(ELM_SRC_DIRS) $(ELM_SRC_FILES) elm/elm.json
	cd elm && elm make src/Main.elm --optimize --output build/elm.js
