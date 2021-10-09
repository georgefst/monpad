{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import System.Directory.Extra
import System.FilePath

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty.Extra qualified as NE
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.IO.Encoding (setLocaleEncoding)
import Monpad.Plugins.LayoutSwitcher qualified as LayoutSwitcher
import Monpad.Plugins.Logger qualified as Logger
import Monpad.Plugins.PingIndicator qualified as PingIndicator
import Monpad.Plugins.QR qualified as QR
import Monpad.Plugins.WatchLayout qualified as WatchLayout
import System.IO (utf8)
import System.IO.Unsafe (unsafeInterleaveIO)

import Monpad
import Monpad.Plugins

import Control.Concurrent
import Control.Exception
import Control.Monad
import Streamly.FSNotify (EventPredicate (EventPredicate), watchDirectoryWith)
import Streamly.Prelude qualified as SP
import System.FSNotify
import System.INotify
import Text.Pretty.Simple

dhallLayoutDefault :: Text
dhallLayoutDefault = "../dhall/textinput.dhall"

dhallLayoutVoid :: Text -> Text
dhallLayoutVoid = ("(../dhall/lib/map-layout.dhall).void " <>)

-- NB: this covers all plugins except 'Logger', which is always in use here.
data P
    = WL
    | QR
    | LS
    | PI
    deriving (Eq, Enum, Bounded)
plugin :: FilePath -> P -> Plugin () ()
plugin home = \case
    WL -> WatchLayout.plugin
    QR -> QR.plugin $ home </> "Desktop"
    LS -> LayoutSwitcher.plugin 1 ()
    PI -> PingIndicator.plugin 1

test :: [P] -> [Text] -> [Text] -> IO ()
test ps ls lsVoid = do
    home <- unsafeInterleaveIO getHomeDirectory
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall $ NE.appendr lsVoid (dhallLayoutVoid <$> dhallLayoutDefault :| ls)
    withPlugin
        (plugins $ Logger.plugin T.putStrLn Logger.Normal : map (plugin home) ps)
        $ server
            1
            8000
            Nothing
            (Just "../dist/assets")
            layouts

testExt :: IO ()
testExt = do
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall $ dhallLayoutVoid dhallLayoutDefault :| []
    serverExtWs
        mempty
        8000
        8001
        Nothing
        (Just "../dist/assets")
        layouts

-- conf = defaultConfig
-- conf = defaultConfig{confDebounce = Debounce 0.1}
conf = defaultConfig{confDebounce = NoDebounce}

dir = "../dhall"

p mv x = takeMVar mv >> pPrint x >> putMVar mv ()

fs, fss, hi :: IO ()
fs = withManagerConf conf \mgr -> do
    mv <- newMVar ()
    _ <- watchDir mgr dir (const True) (p mv)
    forever $ threadDelay maxBound
fss = do
    mv <- newMVar ()
    SP.mapM_ (p mv) . snd =<< watchDirectoryWith conf dir (EventPredicate $ const True)
hi = do
    mv <- newMVar ()
    inot <- initINotify
    wd <- addWatch inot [AllEvents] dir (p mv)
    threadDelay maxBound
        `catch` \(e :: SomeException) -> print e >> removeWatch wd
