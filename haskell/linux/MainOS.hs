module MainOS (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Int (Int32)
import Data.Text.IO qualified as T
import Numeric (readHex)
import Options.Applicative (execParser, fullDesc, header, helper, info)
import Text.Pretty.Simple (pPrint)

import Evdev
import Evdev.Codes

import Monpad
import Orphans.Evdev ()

main :: IO ()
main = do
    (port, dhallLayout) <- execParser $ info (helper <*> argParser) (fullDesc <> header "monpad")
    server port ServerConfig
        { onStart = T.putStrLn "Monpad server started"
        , onNewConnection = \(ClientID i) -> do
            T.putStrLn $ "New client: " <> i
            layout <- layoutFromDhall dhallLayout
            let (as, bs) = allAxesAndButs layout
            dev <- newUDevice $ (defaultNewUDevice "Monpad")
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
            return (layout, dev, ())
        , onMessage = \m -> do
            c <- asks snd
            pPrint (c, m)
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

-- >>> monpadId == sum (zipWith (*) (iterate (* 256) 1) $ map (fromEnum @Char) "MP")
monpadId :: Int
monpadId = fst $ head $ readHex "504d"

-- input is in [-1,1], output in [0,255]
translate :: Double -> Int32
translate x = round $ (x + 1) * 255 / 2
