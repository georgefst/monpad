module OS (conf, keyUnknown) where

import Control.Monad.Reader
import Data.Either
import Data.Foldable
import Data.Int
import Data.List.Extra hiding (insert, (!?))
import Data.Maybe
import Data.Traversable
import Optics hiding (Empty)

import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric (readHex)
import Optics.State.Operators ((.=))

import Evdev hiding (Device, newDevice)
import Evdev.Uinput
import Evdev.Codes

import Monpad
import Orphans.Evdev ()

type E = Map LayoutID (Device, LayoutMeta)
type S = ((Device, LayoutMeta), Text)
type A = AxisInfo
type B = Key

conf :: ServerConfig E S A B
conf = ServerConfig
    { onNewConnection = \(fmap fst -> layouts) (ClientID clientId) -> do
        layoutInfos <- for layouts \layout -> do
            let meta = mkLayoutMeta $ layout ^. #elements
                (axes, keys0) = allAxesAndButs meta
                isTextInput = \case
                    FullElement{element = Input Input'{inputType = Text _}} -> True
                    _ -> False
                keys = if any isTextInput $ layout ^. #elements
                    then keys0 <> filter (isJust . keyToChar) enumerate
                    else keys0
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
                    , "(" <> encodeUtf8 clientId <> ")"
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
            , (snd $ NE.head layoutInfos, "")
            , []
            )
    , onUpdate = \case
        ServerUpdate (SwitchLayout l) -> do
            ls <- asks (^. #extra)
            case ls !? l of
                Just x -> #extra % _1 .= x
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
        ClientUpdate (InputText _ t) -> do
            (#extra % _2) .= t
            mempty
        ClientUpdate (SubmitInput _) -> do
            dev <- use $ #extra % _1 % _1
            liftIO . writeBatch dev
                . concatMap (\k -> [KeyEvent k Pressed, KeyEvent k Released])
                . (++ [KeyEnter])
                . mapMaybe charToKey
                . T.unpack
                =<< use (#extra % _2)
            mempty
        _ -> mempty
    , onStart = mempty
    , onDroppedConnection = mempty
    , onPong = mempty
    , updates = mempty
    }
  where
    lookup' ::
        Monoid m =>
        Lens' LayoutMeta (Map ElementID a) ->
        ElementID ->
        (a -> Device -> Monpad E S A B ()) ->
        Monpad E S A B m
    lookup' l i f = do
        (device, info) <- use $ #extra % _1
        case (info ^. l) !? i of
            Just a -> f a device
            Nothing -> warn $ "element id not found: " <> coerce i
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
        Empty -> id

allAxesAndButs :: LayoutMeta -> ([A], [B])
allAxesAndButs LayoutMeta{..} =
    ( concatMap (\(x, y) -> [x, y]) (Map.elems stickMap) <> Map.elems sliderMap
    , Map.elems buttonMap
    )

-- >>> monpadId == sum (zipWith (*) (iterate (* 256) 1) $ map (fromEnum @Char) "MP")
monpadId :: Int
monpadId = fst $ head $ readHex "504d"

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

charToKey :: Char -> Maybe Key
charToKey = \case
    '0' -> Just Key0
    '1' -> Just Key1
    '2' -> Just Key2
    '3' -> Just Key3
    '4' -> Just Key4
    '5' -> Just Key5
    '6' -> Just Key6
    '7' -> Just Key7
    '8' -> Just Key8
    '9' -> Just Key9
    'a' -> Just KeyA
    'b' -> Just KeyB
    'c' -> Just KeyC
    'd' -> Just KeyD
    'e' -> Just KeyE
    'f' -> Just KeyF
    'g' -> Just KeyG
    'h' -> Just KeyH
    'i' -> Just KeyI
    'j' -> Just KeyJ
    'k' -> Just KeyK
    'l' -> Just KeyL
    'm' -> Just KeyM
    'n' -> Just KeyN
    'o' -> Just KeyO
    'p' -> Just KeyP
    'q' -> Just KeyQ
    'r' -> Just KeyR
    's' -> Just KeyS
    't' -> Just KeyT
    'u' -> Just KeyU
    'v' -> Just KeyV
    'w' -> Just KeyW
    'x' -> Just KeyX
    'y' -> Just KeyY
    'z' -> Just KeyZ
    '\'' -> Just KeyApostrophe
    '\\' -> Just KeyBackslash
    ',' -> Just KeyComma
    '.' -> Just KeyDot
    '=' -> Just KeyEqual
    '`' -> Just KeyGrave
    '{' -> Just KeyLeftbrace
    '-' -> Just KeyMinus
    '}' -> Just KeyRightbrace
    ';' -> Just KeySemicolon
    '/' -> Just KeySlash
    ' ' -> Just KeySpace
    _ -> Nothing

keyToChar :: Key -> Maybe Char
keyToChar = \case
    Key0 ->  Just '0'
    Key1 ->  Just '1'
    Key2 ->  Just '2'
    Key3 ->  Just '3'
    Key4 ->  Just '4'
    Key5 ->  Just '5'
    Key6 ->  Just '6'
    Key7 ->  Just '7'
    Key8 ->  Just '8'
    Key9 ->  Just '9'
    KeyA ->  Just 'a'
    KeyB ->  Just 'b'
    KeyC ->  Just 'c'
    KeyD ->  Just 'd'
    KeyE ->  Just 'e'
    KeyF ->  Just 'f'
    KeyG ->  Just 'g'
    KeyH ->  Just 'h'
    KeyI ->  Just 'i'
    KeyJ ->  Just 'j'
    KeyK ->  Just 'k'
    KeyL ->  Just 'l'
    KeyM ->  Just 'm'
    KeyN ->  Just 'n'
    KeyO ->  Just 'o'
    KeyP ->  Just 'p'
    KeyQ ->  Just 'q'
    KeyR ->  Just 'r'
    KeyS ->  Just 's'
    KeyT ->  Just 't'
    KeyU ->  Just 'u'
    KeyV ->  Just 'v'
    KeyW ->  Just 'w'
    KeyX ->  Just 'x'
    KeyY ->  Just 'y'
    KeyZ ->  Just 'z'
    KeyApostrophe ->  Just '\''
    KeyBackslash ->  Just '\\'
    KeyComma ->  Just ','
    KeyDot ->  Just '.'
    KeyEqual ->  Just '='
    KeyGrave ->  Just '`'
    KeyLeftbrace ->  Just '{'
    KeyMinus ->  Just '-'
    KeyRightbrace ->  Just '}'
    KeySemicolon ->  Just ';'
    KeySlash ->  Just '/'
    KeySpace ->  Just ' '
    _ -> Nothing
