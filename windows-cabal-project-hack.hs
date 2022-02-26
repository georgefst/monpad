import Data.Text qualified as T
import Data.Text.IO qualified as T

-- TODO this is a hack due to https://github.com/haskell/cabal/issues/7083
main = do
    let badPkgs = map T.pack ["rawfilepath"]
        bad = any $ \t0 ->
            let t = T.strip t0
             in T.pack "location" `T.isPrefixOf` t && any (`T.isSuffixOf` t) badPkgs
    T.writeFile "haskell\\cabal.project.patched"
        . T.unlines
        . map T.unlines
        . filter (not . bad)
        . split T.null
        . T.lines
        =<< T.readFile "haskell\\cabal.project"
  where
    split _ [] = [[]]
    split f (x : xs) | f x = [] : split f xs
    split f (x : xs) | y : ys <- split f xs = (x : y) : ys
