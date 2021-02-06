{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

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

module Main (main) where

import Control.Exception.Extra
import Control.Monad
import Control.Monad.Extra
import Data.Function
import Data.List

import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Language.JavaScript.Parser as JS
import qualified Language.JavaScript.Process.Minify as JS
import qualified System.Directory as Dir

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
    "dhall" ~> do
        need . map ((distDir </> "dhall") </>) =<< getDirectoryFiles "dhall" ["*"]
    "assets" ~> need assets

    let rmr dir = liftIO $ removeFiles dir ["//*"]
        clean = do
            putInfo "Cleaning Shake build artefacts"
            rmr shakeDir
            putInfo "Cleaning generated assets"
            rmr rscDir
            rmr distDir
    "clean" ~> clean
    "deep-clean" ~> do
        clean
        putInfo "Cleaning Haskell build artefacts"
        rmr hsBuildDir
        putInfo "Cleaning Elm artefacts"
        rmr elmBuildDir

    forM_ linkedAssets \(file, link) ->
        link %> \_ -> do
            need [file]
            liftIO $ unlessM (Dir.doesFileExist link) $ Dir.createFileLink (".." </> ".." </> file) link

    -- executables e.g. 'dist/monpad-ext-ws'
    (distDir </> "*") %> \f -> do
        need assets
        needDirExcept hsBuildDir hsDir
        cmd_
            (Cwd hsDir)
            ("cabal build --flags=release exe:" <> takeBaseName f)
            ("--builddir=" <> (".." </> hsBuildDir))
        getDirectoryFiles "" [hsBuildDir <//> takeFileName f] >>= \case
            [] -> error "No matches"
            [f'] -> copyFileChanged f' f
            fs -> error $ "Multiple matches: " <> intercalate ", " fs

    elm %> \_ -> do
        needDirExcept elmBuildDir elmDir
        cmd_ (Cwd "elm") "elm make src/Main.elm --optimize --output" (".." </> elm)
        liftIO $ minifyFileJS elm

    distDir </> "dhall" </> "*" %> \out -> do
        let in' = "dhall" </> takeFileName out
        needDhall [in']
        putInfo $ "Resolving imports in: " <> out
        liftIO $ do
            c <- T.readFile in'
            expr <-
                Dhall.throws . Dhall.exprFromText in' $
                    pack "(./lib/map-layout.dhall).to" <> osName <> pack " " <> bracketed c
            --TODO huge due to https://github.com/dhall-lang/dhall-haskell/issues/2116
            resolvedExpression <- Dhall.loadRelativeTo (takeDirectory in') Dhall.UseSemanticCache expr
            _ <- Dhall.throws $ Dhall.typeOf resolvedExpression
            T.writeFile out $ Dhall.pretty resolvedExpression

{- Constants -}

monpadExe, monpad, shakeDir, distDir, rscDir, hsDir, hsBuildDir, elmDir, elmBuildDir, elm :: FilePath
monpadExe = "monpad" <.> exe
monpad = distDir </> monpadExe
shakeDir = ".shake"
distDir = "dist"
rscDir = hsDir </> "rsc"
hsDir = "haskell"
hsBuildDir = ".build" </> "hs"
elmDir = "elm"
elmBuildDir = elmDir </> "elm-stuff"
elm = rscDir </> "elm" <.> "js"

linkedAssets :: [(FilePath, FilePath)]
linkedAssets =
    [ (file, rscDir </> takeFileName file)
    | file <- ["dist" </> "dhall" </> "default.dhall", "js" </> "main.js", "css" </> "main.css"]
    ]

assets :: [FilePath]
assets = elm : map snd linkedAssets

osName :: Text
osName = pack

#if linux_HOST_OS
    "Linux"
#elif mingw32_HOST_OS
    "Windows"
#elif darwin_HOST_OS
    "Mac"
#else
    error "unknown OS"
#endif

{- Util -}

-- | Need all files in 'dir' except those in 'except'
needDirExcept :: FilePath -> FilePath -> Action ()
needDirExcept except dir =
    need . filter (not . (isPrefixOf `on` splitDirectories) except) =<< getDirectoryFiles "" [dir <//> "*"]

minifyFileJS :: Partial => FilePath -> IO ()
minifyFileJS file =
    (flip JS.parse file <$> readFile file) >>= \case
        Left s -> error $ "Failed to parse " <> file <> " as JavaScript:\n" <> s
        Right ast -> TL.writeFile file $ JS.renderToText $ JS.minifyJS ast

bracketed :: Text -> Text
bracketed t = pack "(" <> t <> pack ")"
