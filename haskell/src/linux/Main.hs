module Main where

import Evdev
import Evdev.Codes
import Data.Text.Encoding

import WebGamepad

main :: IO ()
main = server $ defaultConfig
    { getArgs = getCommandLineArgs
    , onNewConnection = \(ClientID i) -> do
        fmap (,()) $ newUDevice $ encodeUtf8 i
    , onMessage = \update dev () -> case update of
        ButtonDown b -> writeBatch dev [KeyEvent (toKey b) Pressed]
        ButtonUp b -> writeBatch dev [KeyEvent (toKey b) Released]
        Stick v -> mempty
    , onDroppedConnection = mempty
    }

-- TODO let this be configurable?
toKey :: Button -> Key
toKey = \case
    Blue -> KeyX
    Yellow -> KeyY
    Red -> KeyB
    Green -> KeyA
