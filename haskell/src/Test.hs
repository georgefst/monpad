-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.IO.Encoding (setLocaleEncoding)
import Monpad.Plugins.Logger qualified as Logger
import System.IO (utf8)

import Monpad
import Monpad.Plugins

defaultSimple :: Text
defaultSimple = "(../dhall/lib/map-layout.dhall).void ../dhall/default.dhall"

test :: [Plugin () ()] -> [Text] -> IO ()
test ps ls = do
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall $ defaultSimple :| ls
    withPlugin
        (plugins $ Logger.plugin T.putStrLn Logger.Loud : ps)
        $ server
            30
            8000
            Nothing
            (Just "../dist/assets")
            layouts

testExt :: IO ()
testExt = do
    Just layouts <- layoutsFromDhall @() @() (pure defaultSimple)
    serverExtWs mempty 8000 8001 Nothing (Just "../dist/assets") layouts
