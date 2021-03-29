module OS where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Tuple.Extra (snd3)
import Data.Text.Encoding (encodeUtf8)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric (readHex)

import Evdev hiding (Device, newDevice)
import Evdev.Uinput
import Evdev.Codes

import Monpad
import Orphans.Evdev ()

conf :: Layout AxisInfo Key -> ServerConfig [Device] () AxisInfo Key
conf layout = mempty
    { onNewConnection = \(ClientID clientId) -> do
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
        dev <- newDevice ("Monpad (" <> encodeUtf8 clientId <> ")") defaultDeviceOpts
            { keys = bs
            , absAxes = aas
            , relAxes = ras
            , miscs = [MscScan]
            , idVendor = Just monpadId
            , idProduct = Just monpadId
            }
        return (pure dev, ())
    , onAxis = \AxisInfo{..} x -> do
        devs <- asks snd3
        liftIO $ for_ devs $ \d -> writeBatch d $ pure @[] case axis of
            Abs a -> AbsoluteEvent a $ EventValue $ round $ (x + 1) * multiplier / 2
            Rel a -> RelativeEvent a $ EventValue $ round $ x * multiplier
    , onButton = \key up -> do
        devs <- asks snd3
        liftIO $ for_ devs $ \d -> writeBatch d [KeyEvent key $ bool Released Pressed up]
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

keyUnknown :: Key
keyUnknown = KeyUnknown
