Build instructions
==================

Prerequisites:
--------------

[Haskell](https://www.haskell.org/):
- `cabal-install` (often just referred to as 'cabal') ≥ 2.4
- `ghc` ≥ 8.6.5

[Elm](https://elm-lang.org/):
- `elm` ≥ 0.19.1

Server
------

Open a terminal in the `server` directory. Run `cabal v2-update` to grab the latest package index from [hackage](https://hackage.haskell.org/), then `cabal v2-run`. This will download all dependencies, then build and run the server. The first time you run this, it could take a while.

Note that if your version of `cabal` is at least `3.0` you can just use `cabal update`, `cabal run` etc. as the `v2` build system is now the default.

Client
------

Open a terminal in the `client` directory. Run `elm reactor`. In a web browser, navigate to the address shown in the terminal (probably `http://localhost:8000`).

To access from an external device (eg. a phone, so that the controls actually work properly), you'll need to use your IP address in place of `localhost`. You will also need to update the value of `urlBase` in `src/Config.elm` to match (don't change the port!), so that the client knows where to find the server. Note that if you are using `elm reactor`, you don't need to do anything to propagate changes to the code - just refresh the web page.

Advanced
--------

It is worth familiarising yourself with both build tools, [here](https://www.haskell.org/cabal/users-guide/nix-local-build-overview.html) and [here](https://guide.elm-lang.org/install/elm.html).

For building a standalone executable and HTML file, you may want commands such as `cabal v2-install --installdir $HOME/bin --install-method copy --overwrite-policy always` and `elm make src/Main.elm --optimize --output dist/main.html`, respectively.
