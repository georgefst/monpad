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

import Data.Function (on)
import Data.List (isPrefixOf)
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
        removeFilesAfter distDir ["//*"]

    wg %> \_ -> do
        need [dhall, elm]
        needDirExcept hsBuildDir hsDir
        cmd (Cwd "haskell") "cabal install --install-method copy --flags=release --installdir" (".." </> distDir)

    elm %> \_ -> do
        needDirExcept elmBuildDir elmDir
        cmd (Cwd "elm") "elm make src/Main.elm --optimize --output" (".." </> elm)

    dhallRule dhall (("dhall" </>) . takeFileName)

{- Constants -}

wg :: FilePath
wg = distDir </> "web-gamepad" <.> exe

build :: FilePath
build = ".build"

distDir :: FilePath
distDir = "dist"

rscDir :: FilePath
rscDir = hsDir </> "rsc"

rscDistDir :: FilePath
rscDistDir = rscDir </> "dist"

hsDir :: FilePath
hsDir = "haskell"

hsBuildDir :: FilePath
hsBuildDir = hsDir </> "dist-newstyle"

elmDir :: FilePath
elmDir = "elm"

elmBuildDir :: FilePath
elmBuildDir = elmDir </> "elm-stuff"

dhall :: FilePath
dhall = rscDistDir </> "default" <.> "dhall"

elm :: FilePath
elm = rscDistDir </> "elm" <.> "js"

{- Util -}

-- | Need all files in 'dir' except those in 'except'
needDirExcept :: FilePath -> FilePath -> Action ()
needDirExcept except dir =
    need =<< filter (not . (isPrefixOf `on` splitDirectories) except) <$> getDirectoryFiles "" [dir <//> "*"]

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
