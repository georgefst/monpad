-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.IO.Encoding (setLocaleEncoding)
import Monpad.Plugins.LayoutSwitcher qualified as LayoutSwitcher
import Monpad.Plugins.Logger qualified as Logger
import Monpad.Plugins.PingIndicator qualified as PingIndicator
import Monpad.Plugins.QR qualified as QR
import Monpad.Plugins.WatchLayout qualified as WatchLayout
import System.IO (utf8)

import Monpad
import Monpad.Plugins

defaultSimple :: Text
defaultSimple = "(../dhall/lib/map-layout.dhall).void ../dhall/textinput.dhall"

-- NB: this covers all plugins except 'Logger', which is always in use here.
data P
    = WL
    | QR FilePath
    | LS
    | PI
plugin :: P -> Plugin () ()
plugin = \case
    WL -> WatchLayout.plugin
    QR path -> QR.plugin path
    LS -> LayoutSwitcher.plugin ()
    PI -> PingIndicator.plugin

test :: [P] -> [Text] -> IO ()
test ps ls = do
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall $ defaultSimple :| ls
    withPlugin
        (plugins $ Logger.plugin T.putStrLn Logger.Loud : map plugin ps)
        $ server
            30
            8000
            Nothing
            (Just "../dist/assets")
            layouts

testExt :: IO ()
testExt = do
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall $ defaultSimple :| []
    serverExtWs
        mempty
        8000
        8001
        Nothing
        (Just "../dist/assets")
        layouts
