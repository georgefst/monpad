#!/usr/bin/env cabal
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{- cabal:
build-depends:
    base >= 4.16,
    dhall ^>= {1.41, 1.42},
    directory ^>= 1.3.7.0,
    extra ^>= 1.7.4,
    language-javascript ^>= 0.7.1.0,
    pretty-simple ^>= 4.1,
    shake ^>= 0.19.1,
    shake-dhall ^>= 0.1.0.0,
    text ^>= {1.2.3, 1.3, 1.4, 1.5, 2.0},
-}

module Main (main) where

import Control.Exception.Extra
import Control.Monad
import Control.Monad.Extra
import Data.Bool
import Data.Either.Extra
import Data.Foldable
import Data.Function
import Data.List
import Data.Monoid.Extra
import System.Console.GetOpt
import System.IO.Error
import System.Info.Extra

import Data.Text (Text, pack)
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Dhall.Core qualified as Dhall
import Dhall.Import qualified as Dhall
import Dhall.Parser qualified as Dhall
import Dhall.TypeCheck qualified as Dhall
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Language.JavaScript.Parser qualified as JS
import Language.JavaScript.Process.Minify qualified as JS
import System.Directory qualified as Dir

import Development.Shake
import Development.Shake.Dhall
import Development.Shake.FilePath
import Text.Pretty.Simple

data Args
    = Target String
    | Compiler String
    deriving (Show)
optDescrs :: [OptDescr (Either String Args)]
optDescrs =
    [ Option
        []
        ["target"]
        (OptArg (maybeToEither "no arg" . fmap Target) "triple")
        "Cross compile. Expects a suitably-prefixed `cabal` to be available."
    , Option
        ['w']
        []
        (OptArg (maybeToEither "no arg" . fmap Compiler) "path")
        "Path to GHC binary to use."
    ]

main :: IO ()
main = do
    setLocaleEncoding utf8
    shakeArgsWith shakeOptions{shakeColor = True, shakeThreads = 0} optDescrs \args wanted ->
        pure $ pure $ uncurry (rules wanted) case args of
            [Target s] -> ("ghc", Just s)
            [Compiler s] -> (s, Nothing)
            [] -> ("ghc", Nothing)
            _ -> error "incompatible args"

rules :: [String] -> String -> Maybe String -> Rules ()
rules wanted ghc maybeTarget = do
    let cabal = maybe "cabal" (<> "-cabal") maybeTarget
        qualify = maybe id (flip (</>)) maybeTarget
        buildDirBase = ".build"
        buildDir = qualify buildDirBase
        hsBuildDir = buildDir </> "hs"
        distDir = qualify "dist"
        monpad = distDir </> "monpad" <.> exe
        monpadDebug = distDir </> "monpad-debug" <.> exe

    want case wanted of
        [] -> [monpad]
        _ -> wanted

    forM_ copiedAssets \(file, copy) ->
        copy %> \_ -> do
            putInfo $ "Copying " <> file <> " to " <> copy
            bool copyFileChanged minifyFileJS (takeExtension file == ".js") file copy

    let haskell path flags = do
            need assets
            -- TODO shouldn't need to exclude `dist-newstyle` but it can still end up being used for `cabal repl` etc.
            -- due to https://github.com/haskell/cabal/issues/5271
            needDirExcept (hsDir </> "dist-newstyle") hsDir
            let args =
                    [ "exe:monpad"
                    , "--flags=" <> flags
                    , "--builddir=" <> (".." </> hsBuildDir)
                    , "-w" <> ghc
                    ]
            cmd_
                (Cwd hsDir)
                cabal
                "build"
                args
            bins <-
                lines . fromStdout
                    <$> cmd
                        (Cwd hsDir)
                        cabal
                        "list-bin"
                        args
            case bins of
                [] -> error "No matches"
                [path'] -> copyFileChanged path' path
                fs -> error $ "Multiple matches: " <> intercalate ", " fs

    monpad %> \_ -> do
        haskell monpad "release"

    let elm optimise = do
            needDirExcept elmBuildDir elmDir
            out <- liftIO $ (</> "monpad-elm.js") <$> Dir.getTemporaryDirectory
            cmd_ (Cwd "elm") "elm make src/Main.elm --output" out (mwhen optimise "--optimize")
            bool (liftIO .: Dir.copyFile) minifyFileJS optimise out elmJS
    elmJS %> \_ -> elm True

    distDir </> "dhall" </> "*" %> \out -> do
        let in' = "dhall" </> takeFileName out
        needDhall [in']
        putInfo $ "Resolving imports in: " <> out
        liftIO $ do
            c <- T.readFile in'
            expr <-
                Dhall.throws . Dhall.exprFromText in' $
                    pack "(./lib/map-layout.dhall).to" <> osName <> pack " " <> bracketed c
            -- TODO huge due to https://github.com/dhall-lang/dhall-haskell/issues/2116
            resolvedExpression <- Dhall.loadRelativeTo (takeDirectory in') Dhall.UseSemanticCache expr
            _ <- Dhall.throws $ Dhall.typeOf resolvedExpression
            T.writeFile out $ Dhall.pretty resolvedExpression

    -- unoptimised, and needs to be run from a directory containing `rsc`, with all the JS/CSS etc. assets
    "debug" ~> do
        haskell monpadDebug ""
        rscs <- liftIO $ Dir.listDirectory rscDir
        for_ rscs $ \r -> copyFileChanged (rscDir </> r) (distDir </> rsc </> r)

    "elm" ~> need [elmJS]
    "elm-debug" ~> elm False
    "dhall" ~> do
        need . map ((distDir </> "dhall") </>) =<< getDirectoryFiles "dhall" ["*"]
    "assets" ~> need assets

    "clean" ~> do
        rmr shakeDir
        rmr rscDir
        rmr distDir
        rmr buildDir
        rmr elmBuildDir

{- Constants -}

shakeDir, rsc, rscDir, hsDir, elmDir, elmBuildDir, elmJS :: FilePath
shakeDir = ".shake"
rsc = "rsc"
rscDir = hsDir </> rsc
hsDir = "haskell"
elmDir = "elm"
elmBuildDir = elmDir </> "elm-stuff"
elmJS = rscDir </> "elm" <.> "js"

copiedAssets :: [(FilePath, FilePath)]
copiedAssets =
    [ (file, rscDir </> takeFileName file)
    | file <-
        [ "dist" </> "dhall" </> "default.dhall"
        , "js" </> "main.js"
        , "css" </> "common.css"
        , "css" </> "login.css"
        , "css" </> "app.css"
        ]
    ]

assets :: [FilePath]
assets = elmJS : map snd copiedAssets

osName :: Text
osName = pack case os of
    "mingw32" -> "Windows"
    "darwin" -> "Mac"
    "linux" -> "Linux"
    _ -> "unknown"

{- Util -}

-- | `rm -r`
rmr :: FilePath -> Action ()
rmr dir = liftIO $ removeFiles dir ["//*"]

-- | Need all files in 'dir' except those in 'except'
needDirExcept :: FilePath -> FilePath -> Action ()
needDirExcept except dir =
    need . filter (not . (isPrefixOf `on` splitDirectories) except) =<< getDirectoryFiles "" [dir <//> "*"]

minifyFileJS :: FilePath -> FilePath -> Action ()
minifyFileJS in_ out = do
    need [in_]
    contents <- liftIO $ readFile in_
    cmd_ "closure-compiler" in_ "--js_output_file" out `actionCatch` \(e :: IOError) -> do
        putInfo $ "Failed to run external minifier, trying Haskell version: " <> show e
        case JS.parse contents in_ of
            Left s -> error $ "Failed to parse " <> in_ <> " as JavaScript:\n" <> s
            Right ast -> liftIO $ TL.writeFile out $ JS.renderToText $ JS.minifyJS ast

bracketed :: Text -> Text
bracketed t = pack "(" <> t <> pack ")"

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
