#!/usr/bin/env cabal

{- cabal:
build-depends:
    base >= 4.13,
    dhall ^>= 1.36,
    extra ^>= 1.7.4,
    language-javascript ^>= 0.7.1.0,
    pretty-simple ^>= 4.0,
    shake ^>= 0.19.1,
    shake-dhall ^>= 0.1.0.0,
    text ^>= 1.2.3.2,
-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Exception.Extra
import Data.Function
import Data.List

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Language.JavaScript.Parser as JS
import qualified Language.JavaScript.Process.Minify as JS

import Development.Shake
import Development.Shake.Dhall
import Development.Shake.FilePath

main :: IO ()
main = do
    setLocaleEncoding utf8
    shakeArgs shakeOptions{shakeColor = True, shakeThreads = 0} rules

rules :: Rules ()
rules = do
    want [monpad]

    "elm" ~> need [elm]

    let rmr dir = liftIO $ removeFiles dir ["//*"]
        clean = do
            putInfo "Cleaning Shake build artefacts"
            rmr shakeDir
            putInfo "Cleaning generated assets"
            rmr rscDistDir
            rmr distDir
    "clean" ~> clean
    "deep-clean" ~> do
        clean
        putInfo "Cleaning Haskell build artefacts"
        rmr hsBuildDir
        putInfo "Cleaning Elm artefacts"
        rmr elmBuildDir

    (distDir </> "*") %> \f -> do
        need [dhall, elm] --TODO if only these have changed, cabal still does nothing as it thinks it's up to date
        needDirExcept hsBuildDir hsDir
        cmd_
            (Cwd hsDir)
            ("cabal build --flags=release exe:" <> takeBaseName f)
        getDirectoryFiles "" [hsBuildDir <//> takeFileName f] >>= \case
            [] -> error "No matches"
            [f'] -> copyFileChanged f' f
            fs -> error $ "Multiple matches: " <> intercalate ", " fs

    elm %> \_ -> do
        needDirExcept elmBuildDir elmDir
        cmd_ (Cwd "elm") "elm make src/Main.elm --optimize --output" (".." </> elm)
        liftIO $ minifyFileJS elm

    dhallRule (rscDistDir <//> "*" <.> "dhall") (("dhall" </>) . takeFileName)

{- Constants -}

monpadExe, monpad, shakeDir, distDir, rscDir, rscDistDir, hsDir, hsBuildDir, elmDir, elmBuildDir, dhall, dhallMapLayout, elm :: FilePath
monpadExe = "monpad" <.> exe
monpad = distDir </> monpadExe
shakeDir = ".shake"
distDir = "dist"
rscDir = hsDir </> "rsc"
rscDistDir = rscDir </> "dist"
hsDir = "haskell"
hsBuildDir = hsDir </> "dist-newstyle"
elmDir = "elm"
elmBuildDir = elmDir </> "elm-stuff"
dhall = rscDistDir </> "default" <.> "dhall"
dhallMapLayout = rscDistDir </> "map-layout" <.> "dhall"
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
        expr <- Dhall.throws . Dhall.exprFromText in' =<< T.readFile in'
        resolvedExpression <- Dhall.loadRelativeTo (takeDirectory in') Dhall.UseSemanticCache expr
        T.writeFile out $ Dhall.pretty resolvedExpression

-- | Minify in place. Fails if file doesn't contain valid JS.
minifyFileJS :: Partial => FilePath -> IO ()
minifyFileJS file =
    (flip JS.parse file <$> readFile file) >>= \case
        Left s -> error $ "Failed to parse " <> file <> " as JavaScript:\n" <> s
        Right ast -> TL.writeFile file $ JS.renderToText $ JS.minifyJS ast
