#TODO it's 2020 - use Shake or something

HS_SRC_DIRS = $(shell find server/src -type d)
HS_SRC_FILES = $(shell find server/src -type f -name '*')

ELM_SRC_DIRS = $(shell find client/src -type d)
ELM_SRC_FILES = $(shell find client/src -type f -name '*')

MANUAL_DIRS = $(shell find client/manual -type d)
MANUAL_FILES = $(shell find client/manual -type f -name '*')

dist/web-gamepad-test: build/elm.js client/manual $(MANUAL_FILES) $(MANUAL_FILES) server/src $(HS_SRC_DIRS) $(HS_SRC_FILES) server/server.cabal
	cd server && cabal v2-build web-gamepad-test
	find server/dist-newstyle -name 'web-gamepad-test' -type f -exec cp {} dist \;

build/elm.js: client/src/ $(ELM_SRC_DIRS) $(ELM_SRC_FILES) client/elm.json
	cd client && elm make src/Main.elm --optimize --output ../build/elm.js
