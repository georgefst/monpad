-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import Control.Monad
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

import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Text.Pretty.Simple
import Data.Hash.Murmur (murmur3)
import Data.Word (Word8)
import Numeric (showHex)

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
    WL -> WatchLayout.plugin write
    QR -> QR.plugin write $ home </> "Desktop"
    LS -> LayoutSwitcher.plugin 1 ()
    PI -> PingIndicator.plugin 1

test :: [P] -> [Text] -> [Text] -> IO ()
test ps ls lsVoid = do
    home <- unsafeInterleaveIO getHomeDirectory
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall write $ NE.appendr lsVoid (dhallLayoutVoid <$> dhallLayoutDefault :| ls)
    withPlugin
        (plugins $ Logger.plugin write Logger.Normal : map (plugin home) ps)
        $ server
            write
            1
            8000
            Nothing
            (Just "../dist/assets")
            layouts

testExt :: IO ()
testExt = do
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall write $ dhallLayoutVoid dhallLayoutDefault :| []
    serverExtWs
        mempty
        8000
        8001
        Nothing
        (Just "../dist/assets")
        layouts

write :: Logger
write = join Logger T.putStrLn

main :: IO ()
main = do
    -- let x = encode $ ButtonUp (ElementID "e1")
    let x = encode $ StickMove (ElementID "somelongishname") $ V2 (1 / 7) (1 / 9)
    pPrintForceColor $ fromIntegral @_ @Double (BS.length x) * 30 * 10 / 1000
    pPrintForceColor x

    let y = encode $ StickMove (ElementID "somelongishname") $ V2 (1 / 7) (1 / 9)
    pPrintForceColor $ fromIntegral @_ @Double (BS.length y) * 30 * 10 / 1000
    pPrintForceColor y
    -- pPrintForceColor (fromIntegral $ murmur3 0 "Stick" :: Word8)

    -- collision
    -- pPrintForceColor (fromIntegral $ murmur3 0 "Stick" :: Word8)
    -- pPrintForceColor (fromIntegral $ murmur3 0 "Menu" :: Word8)
    -- pPrintForceColor (murmur3 0 "Stick")
    -- pPrintForceColor (murmur3 0 "Menu")
    -- pPrintForceColor (fromIntegral $ murmur3 0 "Blue" :: Word8)

    -- pPrintForceColor $ flip showHex "" $ murmur3 0 "Blue"
    -- pPrintForceColor $ map (flip showHex "")
    --     [ 0
    --     , 186
    --     , 26
    --     , 145
    --     , 226
    --     ]

    pure ()
