{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

{- cabal:
build-depends:
    base >= 4.16,
    dhall ^>= 1.41,
    directory ^>= 1.3.7.0,
    extra ^>= 1.7.4,
    language-javascript ^>= 0.7.1.0,
    pretty-simple ^>= 4.0,
    shake ^>= 0.19.1,
    shake-dhall ^>= 0.1.0.0,
    text ^>= {1.2.3, 1.3, 1.4, 1.5, 2.0},
-}

module Main (main) where

import Control.Exception.Extra
import Control.Monad
import Control.Monad.Extra
import Data.Either.Extra
import Data.Function
import Data.List
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

newtype Args
    = Target String
    deriving (Show)
optDescrs :: [OptDescr (Either String Args)]
optDescrs =
    [ Option
        []
        ["target"]
        (OptArg (maybeToEither "no arg" . fmap Target) "triple")
        "Cross compile. Expects a suitably-prefixed `cabal` to be available."
    ]

main :: IO ()
main = do
    setLocaleEncoding utf8
    shakeArgsWith shakeOptions{shakeColor = True, shakeThreads = 0} optDescrs \args wanted ->
        pure $ pure $ rules wanted case args of
            Target s : _ -> Just s
            [] -> Nothing

rules :: [String] -> Maybe String -> Rules ()
rules wanted maybeTarget = do
    let cabal = maybe "cabal" (<> "-cabal") maybeTarget
        qualify = maybe id (flip (</>)) maybeTarget
        hsBuildDir = qualify hsBuildDirBase
        distDir = qualify distDirBase
        monpad = distDir </> "monpad" <.> exe
        monpadDebug = distDir </> "monpad-debug" <.> exe

    want case wanted of
        [] -> [monpad <.> exe]
        _ -> wanted

    forM_ linkedAssets \(file, link) ->
        link %> \_ -> do
            need [file]
            trySymlink file link

    let haskell path flags = do
            need assets
            needDirExcept (hsDir </> "dist-newstyle") hsDir
            let args =
                    [ "exe:monpad"
                    , "--flags=" <> flags
                    , "--builddir=" <> (".." </> hsBuildDir)
                    , "--project-file=cabal.project.patched"
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

    let elm opts = do
            needDirExcept elmBuildDir elmDir
            cmd_ (Cwd "elm") "elm make src/Main.elm --output" (".." </> elmJS) opts
            liftIO $ minifyFileJS elmJS
    elmJS %> \_ -> elm "--optimize"

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
        trySymlink rscDir $ distDir </> rsc

    "elm" ~> need [elmJS]
    "elm-debug" ~> elm ""
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
        rmr hsBuildDirBase
        putInfo "Cleaning Elm artefacts"
        rmr elmBuildDir

{- Constants -}

shakeDir, distDirBase, rsc, rscDir, hsDir, hsBuildDirBase, elmDir, elmBuildDir, elmJS :: FilePath
shakeDir = ".shake"
distDirBase = "dist"
rsc = "rsc"
rscDir = hsDir </> rsc
hsDir = "haskell"
hsBuildDirBase = ".build" </> "hs"
elmDir = "elm"
elmBuildDir = elmDir </> "elm-stuff"
elmJS = rscDir </> "elm" <.> "js"

linkedAssets :: [(FilePath, FilePath)]
linkedAssets =
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
assets = elmJS : map snd linkedAssets

osName :: Text
osName
    | isWindows = pack "Windows"
    | isMac = pack "Mac"
    | otherwise = pack "Linux"

{- Util -}

-- | Need all files in 'dir' except those in 'except'
needDirExcept :: FilePath -> FilePath -> Action ()
needDirExcept except dir =
    need . filter (not . (isPrefixOf `on` splitDirectories) except) =<< getDirectoryFiles "" [dir <//> "*"]

minifyFileJS :: Partial => FilePath -> IO ()
minifyFileJS file =
    readFile file >>= \contents -> case JS.parse contents file of
        Left s -> error $ "Failed to parse " <> file <> " as JavaScript:\n" <> s
        Right ast -> TL.writeFile file $ JS.renderToText $ JS.minifyJS ast

bracketed :: Text -> Text
bracketed t = pack "(" <> t <> pack ")"

catchPermissionError :: IO a -> (IOError -> IO a) -> IO a
catchPermissionError = catchBool isPermissionError

-- | If we can't symlink due to permissions (e.g. Windows non-admin shell), fall back to copying.
trySymlink :: FilePath -> FilePath -> Action ()
trySymlink file link = do
    let copy = liftIO $ Dir.copyFile file link
    file' <- liftIO $ Dir.makeAbsolute file
    liftIO (Dir.doesPathExist link) >>= \case
        True -> unlessM (liftIO $ Dir.pathIsSymbolicLink link) copy
        False -> do
            noLinkPermission <- liftIO do
                (Dir.createFileLink file' link >> pure False)
                    `catchPermissionError` \_ -> pure True
            when noLinkPermission $ putWarn "No permission to create symbolic links - copying instead" >> copy
