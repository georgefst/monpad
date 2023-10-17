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

cabal run -v1 Build.hs
