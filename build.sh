#!/bin/sh

if ghc -fno-code Build.hs 2> /dev/null ; then
    ghc --run Build.hs
else
    cabal run Build.hs --project-file haskell/cabal.project --builddir .build/hs --write-ghc-environment-files=always
fi
