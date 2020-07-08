module Main (main) where

import Data.Int (Int32)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text (Text)
import Text.Read (readMaybe)

import Evdev

import WebGamepad

--TODO print messages for new connection and dropped
    -- work out how to reuse from 'defaultConfig'
    -- include ID, physical path and devnode

main :: IO ()
main = server $ defaultConfig
    { getArgs = getCommandLineArgs
    , onNewConnection = \(ClientID i) -> do
        fmap (,()) $ newUDevice $ encodeUtf8 i
    , onMessage = \update dev () -> case update of
        ButtonDown b -> case readT b of
            Nothing -> warn b "Key"
            Just k -> writeBatch dev [KeyEvent k Pressed]
        ButtonUp b -> case readT b of
            Nothing -> warn b "Key"
            Just k -> writeBatch dev [KeyEvent k Released]
        StickMove s (V2 x y) -> case readT s of
            Nothing -> warn s "AbsoluteAxis pair"
            Just (ax,ay) -> writeBatch dev
                [ AbsoluteEvent ax . EventValue $ translate x
                , AbsoluteEvent ay . EventValue $ translate y
                ]
    , onDroppedConnection = mempty
    --TODO manually delete the device rather than waiting for it to be GCed
    }

-- input is in [-1,1]
translate :: Double -> Int32
translate x = round $ (x + 1) * 255 / 2

warn :: Text -> Text -> IO ()
warn txt typ = T.putStrLn $ "Warning: \"" <> txt <> "\" is not a valid evdev " <> typ <> ": ignoring"

readT :: Read a => Text -> Maybe a
readT = readMaybe . T.unpack
