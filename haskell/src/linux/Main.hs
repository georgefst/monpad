{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Data.Bool (bool)
import Data.Int (Int32)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text (Text)
import Dhall (Generic, FromDhall)
import Text.Read (readMaybe)

import Evdev
import Evdev.Codes

import WebGamepad

main :: IO ()
main = server defaultConfig
    { getArgs = getCommandLineArgs defaultArgs
    , onNewConnection = \(ClientID i) ->
        fmap (,()) $ newUDevice $ encodeUtf8 i
    , onMessage = \update _ () -> onMessage update () ()
    , onAxis = \a x dev () -> writeBatch dev
        [AbsoluteEvent a $ EventValue $ translate x]
    , onButton = \key up dev () -> writeBatch dev
        [KeyEvent key $ bool Released Pressed up]
    , onDroppedConnection = \cid _ -> onDroppedConnection cid ()
    --TODO manually delete the device rather than waiting for it to be GCed
    }
  where ServerConfig{..} = defaultConfig

-- input is in [-1,1], output in [0,255]
translate :: Double -> Int32
translate x = round $ (x + 1) * 255 / 2

deriving instance Generic Key
deriving instance FromDhall Key
deriving instance Generic AbsoluteAxis
deriving instance FromDhall AbsoluteAxis
