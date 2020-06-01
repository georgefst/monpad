module Main where

import Data.Int
import Evdev
import Evdev.Codes
import Data.Text.Encoding

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
        ButtonDown b -> writeBatch dev [KeyEvent (toKey b) Pressed]
        ButtonUp b -> writeBatch dev [KeyEvent (toKey b) Released]
        Stick (V2 x y) -> writeBatch dev
            [ AbsoluteEvent AbsX (EventValue $ translate x)
            , AbsoluteEvent AbsY (EventValue $ translate y)
            ]
    , onDroppedConnection = mempty
    }

-- input is in [-1,1]
translate :: Double -> Int32
translate x = round $ (x + 1) * 255 / 2

-- TODO let this be configurable?
-- TODO use BtnNorth etc. ?
toKey :: Button -> Key
toKey = \case
    Blue -> KeyX
    Yellow -> KeyY
    Red -> KeyB
    Green -> KeyA
