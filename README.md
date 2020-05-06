Prerequisites:
--------------

[Haskell](https://www.haskell.org/):
- `cabal-install` (often just referred to as 'cabal') ≥ 2.4
- `ghc` ≥ 8.6.5

[Elm](https://elm-lang.org/):
- `elm` ≥ 0.19.1

Build
------

If you haven't done so before, run `cabal v2-update` to grab the latest package index from [hackage](https://hackage.haskell.org/) (if your `cabal --version` is at least `3.0` you can just use `cabal update` as the `v2` build system is now the default).

Run `make` to create an executable in the `dist` folder. The first time you run this, it could take a while, as `cabal` will need to download and build all dependencies.

Run
---

Run the resulting executable to start the server (you can pass the `-h` flag to see all options). To access from an external device (eg. a phone, so that the controls actually work properly), you'll need to pass your IP address as the default is `localhost`.

In a web browser, navigate to the address indicated in the terminal (probably `http://localhost:8000`).

<!-- TODO readd 'advanced' section when development workflow has ossified -->
