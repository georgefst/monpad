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
defaultSimple = "(../dhall/lib/map-layout.dhall).void ../dhall/default.dhall"

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

-- NB: this cover all plugins except 'Logger', which is always in use here.
data P
    = WatchLayout
    | QR FilePath
    | LayoutSwitcher
    | PingIndicator
plugin :: P -> Plugin () ()
plugin = \case
    WatchLayout -> WatchLayout.plugin
    QR path -> QR.plugin path
    LayoutSwitcher -> LayoutSwitcher.plugin ()
    PingIndicator -> PingIndicator.plugin

testExt :: IO ()
testExt = do
    Just layouts <- layoutsFromDhall @() @() (pure defaultSimple)
    serverExtWs mempty 8000 8001 Nothing (Just "../dist/assets") layouts
