#!/usr/bin/env cabal

{- cabal:
build-depends:
    base,
    dhall,
    shake,
    shake-dhall,
    text,
-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Text.IO qualified as T
import Development.Shake
import Development.Shake.Dhall
import Development.Shake.FilePath
import Dhall.Core (pretty, throws)
import Dhall.Import (SemanticCacheMode (UseSemanticCache), loadRelativeTo)
import Dhall.Parser (exprFromText)

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = build} $ do
    want [wg]

    "dhall" ~> need [dhall]
    "elm" ~> need [elm]
    "clean" ~> do
        putInfo "Cleaning build artefacts"
        removeFilesAfter build ["//*"]
        putInfo "Cleaning generated assets"
        removeFilesAfter rscDistDir ["//*"]

    wg %> \_ -> do
        need [dhall, elm, hsDir </> "web-gamepad.cabal", hsDir </> "cabal.project"]
        need =<< getDirectoryFiles "" [rscDir </> "*"]
        need =<< getDirectoryFiles "" [hsDir <//> "*.hs"]
        cmd (Cwd "haskell") "cabal install --install-method copy --flags=release --installdir" (".." </> dist)

    elm %> \_ -> do
        need [elmDir </> "elm.json"]
        need =<< getDirectoryFiles "" [elmDir <//> "*.elm"]
        cmd (Cwd "elm") "elm make src/Main.elm --optimize --output" (".." </> elm)

    dhallRule dhall (("dhall" </>) . takeFileName)

{- Constants -}

wg :: FilePath
wg = dist </> "web-gamepad" <.> exe

build :: FilePath
build = ".build"

dist :: FilePath
dist = "dist"

rscDir :: FilePath
rscDir = hsDir </> "rsc"

rscDistDir :: FilePath
rscDistDir = rscDir </> "dist"

hsDir :: FilePath
hsDir = "haskell"

elmDir :: FilePath
elmDir = "elm"

dhall :: FilePath
dhall = rscDistDir </> "default" <.> "dhall"

elm :: FilePath
elm = rscDistDir </> "elm" <.> "js"

{- Util -}

-- | Resolve imports in given files
dhallRule ::
    FilePattern ->
    -- | Compute output file name from input
    (FilePath -> FilePath) ->
    Rules ()
dhallRule p f = p %> \out -> do
    let in' = f out
    needDhall [in']
    putInfo $ "Resolving imports in: " <> out
    liftIO $ dhallResolve in' out
    where
        dhallResolve :: FilePath -> FilePath -> IO ()
        dhallResolve in' out = do
            expr <- throws . exprFromText in' =<< T.readFile in'
            resolvedExpression <- loadRelativeTo (takeDirectory in') UseSemanticCache expr
            T.writeFile out $ pretty resolvedExpression
