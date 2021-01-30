module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Int (Int32)
import qualified Data.Text.IO as T
import Numeric (readHex)
import Options.Applicative (execParser, fullDesc, header, helper, info, short, switch)
import Text.Pretty.Simple (pPrint)

import Evdev
import Evdev.Codes

import Monpad
import Orphans.Evdev ()

main :: IO ()
main = do
    let parser = (,) <$> switch (short 'q' <> long "quiet") <*> argParser (defaultDhall ())
    (quiet, (port, imageDir, dhallLayout)) <- execParser $ info (helper <*> parser) (fullDesc <> header "monpad")
    layout <- layoutFromDhall dhallLayout
    server port imageDir layout ServerConfig
        { onStart = T.putStrLn "Monpad server started"
        , onNewConnection = \(ClientID i) -> do
            T.putStrLn $ "New client: " <> i
            let (as, bs) = allAxesAndButs layout
            dev <- newUDevice (defaultNewUDevice "Monpad")
                { keys = bs
                , absAxes = zip as $ repeat AbsInfo
                    { absValue = 127
                    , absMinimum = 0
                    , absMaximum = 255
                    , absFuzz = 0
                    , absFlat = 0
                    , absResolution = 0
                    }
                , miscs = [MscScan]
                , idVendor = Just monpadId
                , idProduct = Just monpadId
                }
            return (dev, ())
        , onMessage = \m -> do
            c <- asks snd
            unless quiet $ pPrint (c, m)
            dev <- asks fst
            liftIO $ writeBatch dev [AbsoluteEvent a $ EventValue $ translate x]
        , onButton = \key up -> do
            dev <- asks fst
            liftIO $ writeBatch dev [KeyEvent key $ bool Released Pressed up]
        , onDroppedConnection = \_ -> do
            ClientID i <- asks snd
            liftIO $ T.putStrLn $ "Client disconnected: " <> i
        , updates = mempty
        }

-- >>> monpadId == sum (zipWith (*) (iterate (* 256) 1) $ map (fromEnum @Char) "MP")
monpadId :: Int
monpadId = fst $ head $ readHex "504d"

-- input is in [-1,1], output in [0,255]
translate :: Double -> Int32
translate x = round $ (x + 1) * 255 / 2
