#!/bin/sh

for i in 'ghc 9.2' 'cabal 3.6' 'elm 0.19' ; do
    s=( $i )
    PROG=${s[0]}
    REQUIRED=${s[1]}
    INSTALLED=$($PROG --version | head -n 1 | rev | cut -d ' ' -f 1 | rev)
    if printf '%s\n%s' $REQUIRED $INSTALLED | sort -V -C ; then
        :
    else
        printf '%s version >= %s required - %s found\n' $PROG $REQUIRED $INSTALLED
        exit 1
    fi
done

if ghc -fno-code Build.hs 2> /dev/null ; then
    ghc -ignore-dot-ghci --run Build.hs
else
    # TODO environment files no longer written - https://github.com/haskell/cabal/issues/6999#issuecomment-1052953251
    cabal run -w ghc Build.hs --project-file cabal.project --builddir .build/hs --write-ghc-environment-files=always
fi
