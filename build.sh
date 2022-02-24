# this won't be necessary once all dependencies have updated for 9.2
# and it wouldn't be necessary if we could use `source-repository-package`, `allow-newer` etc. in cabal scripts...
cat haskell/cabal.project | tail -n +2 > /tmp/cabal.project.build
cabal run Build.hs --project-file /tmp/cabal.project.build --builddir .build --write-ghc-environment-files=always
