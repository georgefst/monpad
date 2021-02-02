module OS where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Text.Encoding (encodeUtf8)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric (readHex)

import Evdev
import Evdev.Codes

import Monpad
import Orphans.Evdev ()

conf :: Layout AxisInfo Key -> ServerConfig UDevice () AxisInfo Key
conf layout = ServerConfig
    { onStart = mempty
    , onNewConnection = \(ClientID clientId) -> do
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
        dev <- newUDevice (defaultNewUDevice $ "Monpad (" <> encodeUtf8 clientId <> ")")
            { keys = bs
            , absAxes = aas
            , relAxes = ras
            , miscs = [MscScan]
            , idVendor = Just monpadId
            , idProduct = Just monpadId
            }
        return (dev, ())
    , onMessage = mempty
    , onAxis = \AxisInfo{..} x -> do --note that x is always between -1 and 1
        dev <- asks fst
        liftIO $ writeBatch dev $ pure @[] case axis of
            Abs a -> AbsoluteEvent a $ EventValue $ round $ (x + 1) * multiplier / 2
            Rel a -> RelativeEvent a $ EventValue $ round $ x * multiplier
    , onButton = \key up -> do
        dev <- asks fst
        liftIO $ writeBatch dev [KeyEvent key $ bool Released Pressed up]
    , onDroppedConnection = mempty
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
