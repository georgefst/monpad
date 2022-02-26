#!/bin/sh

for i in 'ghc 9.2' 'cabal 3.6' 'elm 0.19' ; do
    s=( $i )
    PROG=${s[0]}
    REQUIRED=${s[1]}
    INSTALLED=$($PROG --version | head -n 1 | rev | cut -d ' ' -f 1 | rev)
    if printf '%s\n%s' $REQUIRED $INSTALLED | sort -V -C ; then
        :
    else
        printf '%s version %s required - %s found\n' $PROG $REQUIRED $INSTALLED
        exit 1
    fi
done

if ghc -fno-code Build.hs 2> /dev/null ; then
    ghc --run Build.hs
else
    cabal run Build.hs --project-file haskell/cabal.project --builddir .build/hs --write-ghc-environment-files=always
fi
