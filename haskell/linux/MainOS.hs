{-# OPTIONS_GHC -Wno-orphans #-}
module MainOS (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Int (Int32)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Dhall (FromDhall, Generic)
import Text.Pretty.Simple (pPrint)

import Evdev
import Evdev.Codes

import Monpad

main :: IO ()
main = do
    args <- getCommandLineArgs defaultArgs
    server args ServerConfig
        { onStart = T.putStrLn "Monpad server started"
        , onNewConnection = \(ClientID i) -> do
            T.putStrLn $ "New client: " <> i
            fmap (,()) $ newUDevice $ encodeUtf8 i
        , onMessage = \m -> do
            c <- asks snd
            pPrint (c,m)
        , onAxis = \a x -> do
            dev <- asks fst
            liftIO $ writeBatch dev [AbsoluteEvent a $ EventValue $ translate x]
        , onButton = \key up -> do
            dev <- asks fst
            liftIO $ writeBatch dev [KeyEvent key $ bool Released Pressed up]
        , onDroppedConnection = \_ -> do
            (ClientID i) <- asks snd
            liftIO $ T.putStrLn $ "Client disconnected: " <> i
        }

-- input is in [-1,1], output in [0,255]
translate :: Double -> Int32
translate x = round $ (x + 1) * 255 / 2

deriving instance Generic Key
deriving instance FromDhall Key
deriving instance Generic AbsoluteAxis
deriving instance FromDhall AbsoluteAxis
