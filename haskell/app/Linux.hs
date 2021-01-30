module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import qualified Data.Text.IO as T
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric (readHex)
import Options.Applicative (execParser, fullDesc, header, helper, info, long, short, switch)
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
                (aas, ras) = partitionEithers $ as <&> \case
                   AxisInfo{axis = Abs a, ..} -> Left
                        ( a
                        , AbsInfo
                            { absValue = floor $ multiplier / 2
                            , absMinimum = 0
                            , absMaximum = floor multiplier
                            , absFuzz = 0
                            , absFlat = 0
                            , absResolution = 0
                            }
                        )
                   AxisInfo{axis = Rel a} -> Right a
            dev <- newUDevice (defaultNewUDevice "Monpad")
                { keys = bs
                , absAxes = aas
                , relAxes = ras
                , miscs = [MscScan]
                , idVendor = Just monpadId
                , idProduct = Just monpadId
                }
            return (dev, ())
        , onMessage = \m -> do
            c <- asks snd
            unless quiet $ pPrint (c, m)
        , onAxis = \AxisInfo{..} x -> do --note that x is always between -1 and 1
            dev <- asks fst
            liftIO $ writeBatch dev $ pure @[] case axis of
                Abs a -> AbsoluteEvent a $ EventValue $ round $ (x + 1) * multiplier / 2
                Rel a -> RelativeEvent a $ EventValue $ round $ x * multiplier
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

data AxisInfo = AxisInfo
    { axis :: Axis
    , multiplier :: Double
    }
    deriving (Generic, FromDhall)

data Axis
    = Abs AbsoluteAxis
    | Rel RelativeAxis
    deriving (Generic, FromDhall)
