{- TODO this should be in a separate component really (maybe even an actual test)
but we'll need to wait for GHC's new "multiple home units" support to be used by cabal
otherwise we'd have very slow turnarounds for testing from making an edit in the core library
-}

-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import Control.Concurrent.Extra
import Control.Monad
import Data.Functor
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

import Monpad
import Monpad.Plugins

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
plugin :: Logger -> FilePath -> P -> Plugin () ()
plugin write home = \case
    WL -> WatchLayout.plugin write
    QR -> QR.plugin write $ home </> "Desktop"
    LS -> LayoutSwitcher.plugin 1 ()
    PI -> PingIndicator.plugin 1

test :: [P] -> [Text] -> [Text] -> IO ()
test ps ls lsVoid = do
    home <- getHomeDirectory
    setLocaleEncoding utf8
    write <- mkLogger
    Just layouts <- layoutsFromDhall write $ NE.appendr lsVoid (dhallLayoutVoid <$> dhallLayoutDefault :| ls)
    withPlugin
        (plugins $ Logger.plugin write Logger.Normal : map (plugin write home) ps)
        $ server
            write
            1
            JSONEncoding
            Nothing
            8000
            "dropped"
            "monpad test"
            defaultLoginPageOpts
            nColours
            (Just "../dist/assets")
            layouts

testExt :: IO ()
testExt = do
    setLocaleEncoding utf8
    write <- mkLogger
    Just layouts <- layoutsFromDhall write $ dhallLayoutVoid dhallLayoutDefault :| []
    serverExtWs
        mempty
        JSONEncoding
        8000
        8001
        "dropped"
        "monpad test ext"
        defaultLoginPageOpts
        nColours
        (Just "../dist/assets")
        layouts

testDump :: IO ()
testDump = do
    tmp <- getTemporaryDirectory
    setLocaleEncoding utf8
    write <- mkLogger
    Just layouts <- layoutsFromDhall write $ dhallLayoutVoid dhallLayoutDefault :| []
    dumpHTML
        JSONEncoding
        (Just $ tmp </> "monpad-login.html")
        (tmp </> "monpad.html")
        Nothing
        8001
        "dropped"
        "monpad test dump"
        defaultLoginPageOpts
        nColours
        layouts

mkLogger :: IO Logger
mkLogger = newLock <&> \lock -> join Logger (withLock lock . T.putStrLn) True

nColours :: Int
nColours = 3
