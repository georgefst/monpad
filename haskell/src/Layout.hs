{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Layout where

import Data.Aeson qualified as J
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Aeson.Types qualified as JSON
import Data.Bifunctor (Bifunctor)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Dhall (FromDhall, auto, input)
import GHC.Generics (Generic)
import Generic.Functor (GenericBifunctor (GenericBifunctor))
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Linear.V2 (V2)
import Orphans.V2 ()
import Util.Elm (Unit, Via (..))
import Util.Elm qualified as Elm
import Util.ShowNewtype (ShowNewtypeWithoutRecord (ShowNewtypeWithoutRecord))

allAxesAndButs :: Layout a b -> ([a], [b])
allAxesAndButs layout = partitionEithers $ map element layout.elements >>= \case
    Stick s -> map Left [s.stickDataX, s.stickDataY]
    Button b -> [Right b.buttonData]
    Slider s -> [Left s.sliderData]
    Indicator _ -> []
    Empty -> []

-- | A (non-empty) list of 'Layout's.
type Layouts a b = NonEmpty (Layout a b)
layoutsFromDhall :: (FromDhall a, FromDhall b) => NonEmpty Text -> IO (Layouts a b)
layoutsFromDhall = traverse layoutFromDhall

layoutFromDhall :: (FromDhall a, FromDhall b) => Text -> IO (Layout a b)
layoutFromDhall = input auto

newtype LayoutID = LayoutID {unwrap :: Text}
    deriving newtype (Eq, Ord, Semigroup, Monoid, ToJSON, FromDhall, HasElmType, HasElmDecoder JSON.Value)
    deriving Show via (ShowNewtypeWithoutRecord "LayoutID" Text)

data Layout a b = Layout
    { elements :: [FullElement a b]
    , viewBox :: ViewBox
    , backgroundColour :: Colour
    , name :: LayoutID
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 Layout Unit Unit
    deriving (ToJSON) via Elm.Via2 Layout a b
    deriving (Bifunctor) via GenericBifunctor Layout

data FullElement a b = FullElement
    { element :: Element a b
    , location :: V2 Int
    , name :: ElementID
    , text :: Maybe TextBox
    , image :: Maybe Image
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 FullElement Unit Unit
    deriving (ToJSON) via Elm.Via2 FullElement a b
    deriving (Bifunctor) via GenericBifunctor FullElement

newtype ElementID = ElementID {unwrap :: Text}
    deriving stock (Eq, Ord)
    deriving newtype (FromDhall, ToJSON, FromJSON, HasElmType, HasElmEncoder J.Value, HasElmDecoder J.Value)
    deriving Show via (ShowNewtypeWithoutRecord "ElementID" Text)

data Element a b
    = Stick (Stick a)
    | Button (Button b)
    | Slider (Slider a)
    | Indicator Indicator
    | Empty
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 Element Unit Unit
    deriving (ToJSON) via Elm.Via2 Element a b
    deriving (Bifunctor) via GenericBifunctor Element

data Stick a = Stick'
    { radius :: Word
    , range :: Word
    , stickColour :: Colour
    , backgroundColour :: Colour
    , stickDataX :: a
    , stickDataY :: a
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via1 Stick Unit
    deriving (ToJSON) via Elm.Via1 Stick a

data Button b = Button'
    { shape :: Shape
    , colour :: Colour
    , buttonData :: b
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via1 Button Unit
    deriving (ToJSON) via Elm.Via1 Button b

data Slider a = Slider'
    { radius :: Word
    , offset :: V2 Int
    -- ^ where the slider ends (it starts at the element's location)
    , width :: Word
    , initialPosition :: Double
    -- ^ 0 (start) to 1 (end)
    , resetOnRelease :: Bool
    , sliderColour :: Colour
    , backgroundColour :: Colour
    , sliderData :: a
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via1 Slider Unit
    deriving (ToJSON) via Elm.Via1 Slider a

data Image = Image
    { width :: Word
    , height :: Word
    , url :: Text
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via Image

data TextBox = TextBox
    { text :: Text
    , style :: TextStyle
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via TextBox

data Indicator = Indicator'
    { hollowness :: Double
    -- ^ [0, 1]
    , arcStart :: Double
    -- ^ [0, 1)
    , arcEnd :: Double
    -- ^ [0, arcStart + 1)
    , centre :: V2 Double
    -- ^ x and y in [-1, 1]
    , colour :: Colour
    , shape :: Shape
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via Indicator

data Shape
    = Circle Word
    | Rectangle (V2 Word)
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via Shape

-- field names chosen to match 'elm-color's 'fromRgba'
data Colour = Colour
    { red :: Double
    , green :: Double
    , blue :: Double
    , alpha :: Double
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via Colour

data ViewBox = ViewBox
    { x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via ViewBox

data TextStyle = TextStyle
    { size :: Word
    , colour :: Colour
    , bold :: Bool
    , italic :: Bool
    , underline :: Bool
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via TextStyle
