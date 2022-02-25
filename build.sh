#!/bin/sh

if ghc -fno-code Build.hs ; then
    ghc --run Build.hs
else
    cabal run Build.hs --project-file haskell/cabal.project --builddir .build/hs --write-ghc-environment-files=always
fi
