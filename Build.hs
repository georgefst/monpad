#!/usr/bin/env cabal

{- cabal:
build-depends:
    base >= 4.14,
    dhall ^>= 1.36,
    extra ^>= 1.7.4,
    language-javascript ^>= 0.7.1.0,
    pretty-simple ^>= 4.0,
    shake ^>= 0.19.1,
    shake-dhall ^>= 0.1.0.0,
    text ^>= 1.2.3.2,
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Exception.Extra (Partial)
import Data.Function (on)
import Data.List (isPrefixOf)
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Development.Shake
import Development.Shake.Dhall
import Development.Shake.FilePath
import Dhall.Core (pretty, throws)
import Dhall.Import (SemanticCacheMode (UseSemanticCache), loadRelativeTo)
import Dhall.Parser (exprFromText)
import Language.JavaScript.Parser (parse, renderToText)
import Language.JavaScript.Process.Minify (minifyJS)

main :: IO ()
main = shakeArgs shakeOptions{shakeColor = True, shakeThreads = 0} $ do
    want [monpad]

    "dhall" ~> need [dhall]
    "elm" ~> need [elm]
    let clean = do
            putInfo "Cleaning Shake build artefacts"
            removeFilesAfter shakeDir ["//*"]
            putInfo "Cleaning generated assets"
            removeFilesAfter rscDistDir ["//*"]
            removeFilesAfter distDir ["//*"]
    "clean" ~> clean
    "deep-clean" ~> do
        putInfo "Cleaning Haskell build artefacts"
        removeFilesAfter hsBuildDir ["//*"]
        putInfo "Cleaning Elm artefacts"
        removeFilesAfter elmBuildDir ["//*"]
        clean

    monpad %> \_ -> do
        need [dhall, elm]
        needDirExcept hsBuildDir hsDir
            (Cwd hsDir)

    elm %> \_ -> do
        needDirExcept elmBuildDir elmDir
        cmd_ (Cwd "elm") "elm make src/Main.elm --optimize --output" (".." </> elm)
        liftIO $ minifyFileJS elm

    dhallRule dhall (("dhall" </>) . takeFileName)

{- Constants -}

monpad :: FilePath
monpad = distDir </> "monpad" <.> exe

shakeDir :: FilePath
shakeDir = ".shake"

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
    need . filter (not . (isPrefixOf `on` splitDirectories) except) =<< getDirectoryFiles "" [dir <//> "*"]

-- | Resolve imports in given files
dhallRule ::
    FilePattern ->
    -- | Compute output file name from input
    (FilePath -> FilePath) ->
    Rules ()
dhallRule p f =
    p %> \out -> do
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

-- | Minify in place. Fails if file doesn't contain valid JS.
minifyFileJS :: Partial => FilePath -> IO ()
minifyFileJS file =
    (flip parse file <$> readFile file) >>= \case
        Left s -> error $ "Failed to parse " <> file <> " as JavaScript:\n" <> s
        Right ast -> TL.writeFile file $ renderToText $ minifyJS ast
