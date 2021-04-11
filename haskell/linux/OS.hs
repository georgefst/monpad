module OS where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Text.Encoding (encodeUtf8)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric (readHex)
import Optics (view)

import Evdev hiding (Device, newDevice)
import Evdev.Uinput
import Evdev.Codes

import Monpad
import Orphans.Evdev ()

conf :: ServerConfig [Device] () AxisInfo Key
conf = mempty
    { onNewConnection = \layouts (ClientID clientId) -> do
        let (as, bs) = foldMap allAxesAndButs layouts
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
        return (pure dev, (), [])
    , onAxis = \AxisInfo{..} x -> do
        devs <- asks $ view #extra
        liftIO $ for_ devs $ \d -> writeBatch d $ pure @[] case axis of
            Abs a -> AbsoluteEvent a $ EventValue $ round $ (x + 1) * multiplier / 2
            Rel a -> RelativeEvent a $ EventValue $ round $ x * multiplier
        mempty
    , onButton = \key up -> do
        devs <- asks $ view #extra
        liftIO $ for_ devs $ \d -> writeBatch d [KeyEvent key $ bool Released Pressed up]
        mempty
    }

-- >>> monpadId == sum (zipWith (*) (iterate (* 256) 1) $ map (fromEnum @Char) "MP")
monpadId :: Int
monpadId = fst $ head $ readHex "504d"

data AxisInfo = AxisInfo
    { axis :: Axis
    , multiplier :: Double
    }
    deriving (Show, Generic, FromDhall)

data Axis
    = Abs AbsoluteAxis
    | Rel RelativeAxis
    deriving (Show, Generic, FromDhall)

keyUnknown :: Key
keyUnknown = KeyUnknown
