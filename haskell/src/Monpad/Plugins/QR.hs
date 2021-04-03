module Monpad.Plugins.QR (plugin) where

import Codec.Picture
import Data.Functor
import System.Directory
import System.FilePath

import Codec.QRCode
import Codec.QRCode.JuicyPixels
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Monpad
import Monpad.Plugins

plugin :: FilePath -> Plugin a b
plugin path = Plugin $ writeQR @() @() path

writeQR :: (Monoid e, Monoid s) => FilePath -> ServerConfig e s a b
writeQR path0 = const mempty
    { onStart = \url -> case encodeText (defaultQRCodeOptions M) Iso8859_1OrUtf8WithoutECI url of
        Nothing -> T.putStrLn "Failed to encode URL as QR code"
        Just qr -> do
            path <- doesDirectoryExist path0 <&> \case
                True -> path0 </> "monpad-address-qr.png"
                False -> path0
            savePngImage path . ImageY8 $ toImage 4 100 qr
            T.putStrLn $ "Server address encoded as: " <> T.pack path
    }
