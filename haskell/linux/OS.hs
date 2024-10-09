module OS (conf, keyUnknown) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Traversable
import Optics hiding (Empty)

import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric (readHex)
import Optics.State.Operators ((.=))
import Streamly.Data.Stream.Prelude qualified as S

import Evdev hiding (Device, newDevice)
import Evdev.Uinput
import Evdev.Codes

import Monpad
import Orphans.Evdev ()

type E = Map LayoutID (Device, LayoutMeta)
type S = (Device, LayoutMeta)
type A = AxisInfo
type B = Key

conf :: ServerConfig E S A B
conf = ServerConfig
    { onNewConnection = \(fmap fst -> layouts) client -> do
        layoutInfos <- for layouts \layout -> do
            let meta = mkLayoutMeta $ layout ^. #elements
                (axes, keys) = allAxesAndButs meta
                (absAxes, relAxes) = partitionEithers $ axes <&> \case
                    AxisInfo{axis = Abs a, ..} -> Left
                        ( a
                        , AbsInfo
                            { absValue = abs multiplier `div` 2
                            , absMinimum = 0
                            , absMaximum = abs multiplier
                            , absFuzz = 0
                            , absFlat = 0
                            , absResolution = 0
                            }
                        )
                    AxisInfo{axis = Rel a} -> Right a
                devName = BS.unwords
                    [ "Monpad:"
                    , encodeUtf8 $ layout ^. #name % coerced
                    , "(" <> encodeUtf8 client.id.unwrap <> ")"
                    ]
            device <- newDevice devName defaultDeviceOpts
                { keys
                , absAxes
                , relAxes
                , miscs = [MscScan]
                , idVendor = Just monpadId
                , idProduct = Just monpadId
                }
            pure
                ( layout ^. #name
                , (device, meta)
                )
        pure
            ( Map.fromList $ toList layoutInfos
            , snd $ NE.head layoutInfos
            , []
            )
    , onUpdate = \case
        ServerUpdate (SwitchLayout l) -> do
            ls <- asks (^. #extra)
            case ls !? l of
                Just x -> #extra .= x
                Nothing -> warn $ "no evdev device found for layout: " <> coerce l
            mempty
        ClientUpdate (ButtonUp i) ->
            lookup' #buttonMap i $ onButton Released
        ClientUpdate (ButtonDown i) ->
            lookup' #buttonMap i $ onButton Pressed
        ClientUpdate (StickMove i (V2 x y)) ->
            lookup' #stickMap i \(x', y') -> onAxis x x' <> onAxis y y'
        ClientUpdate (SliderMove i x) ->
            lookup' #sliderMap i . onAxis $ x * 2 - 1
        _ -> mempty
    , onStart = mempty
    , onDroppedConnection = mempty
    , onPong = mempty
    , updates = const S.nil
    }
  where
    lookup' ::
        Monoid m =>
        Lens' LayoutMeta (Map ElementID a) ->
        ElementID ->
        (a -> Device -> Monpad E S A B ()) ->
        Monpad E S A B m
    lookup' l i f = do
        (device, info) <- gets (^. #extra)
        case (info ^. l) !? i of
            Just a -> f a device
            Nothing -> unless (internalElementTag `T.isPrefixOf` coerce i)
                $ warn $ "element id not found: " <> coerce i
        mempty

-- | Expects input in range [-1, 1].
onAxis :: MonadIO m => Double -> AxisInfo -> Device -> m ()
onAxis x AxisInfo{..} dev = liftIO . writeBatch dev $ pure @[] case axis of
    Abs a -> AbsoluteEvent a $ EventValue $ round $ (x * m + abs m) / 2
    Rel a -> RelativeEvent a $ EventValue $ round $ x * m
  where
    m = fromIntegral multiplier

onButton :: MonadIO m => KeyEvent -> Key -> Device -> m ()
onButton ev key dev = liftIO $ writeBatch dev [KeyEvent key ev]

data LayoutMeta = LayoutMeta
    { stickMap :: Map ElementID (A, A)
    , sliderMap :: Map ElementID A
    , buttonMap :: Map ElementID B
    }
    deriving (Generic)

mkLayoutMeta :: [FullElement A B] -> LayoutMeta
mkLayoutMeta = foldl' (flip insert) $ LayoutMeta mempty mempty mempty
  where
    insert e = case e ^. #element of
        Stick x -> over #stickMap $ Map.insert (e ^. #name) (x ^. #stickDataX, x ^. #stickDataY)
        Slider x -> over #sliderMap $ Map.insert (e ^. #name) (x ^. #sliderData)
        Button x -> over #buttonMap $ Map.insert (e ^. #name) (x ^. #buttonData)
        Indicator _ -> id
        Input _ -> id
        Empty _ -> id

allAxesAndButs :: LayoutMeta -> ([A], [B])
allAxesAndButs LayoutMeta{..} =
    ( concatMap (\(x, y) -> [x, y]) (Map.elems stickMap) <> Map.elems sliderMap
    , Map.elems buttonMap
    )

-- >>> monpadId == sum (zipWith (*) (iterate (* 256) 1) $ map (fromEnum @Char) "MP")
monpadId :: Int
monpadId = maybe (error "static hex parse failed") fst $ listToMaybe $ readHex "504d"

data AxisInfo = AxisInfo
    { axis :: Axis
    , multiplier :: Int32
    }
    deriving (Show, Generic, FromDhall)

data Axis
    = Abs AbsoluteAxis
    | Rel RelativeAxis
    deriving (Show, Generic, FromDhall)

keyUnknown :: Key
keyUnknown = KeyUnknown
