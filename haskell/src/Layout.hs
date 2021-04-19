{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Layout where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson qualified as J
import Data.Aeson.Types (FromJSON, ToJSON, ToJSONKey)
import Data.Aeson.Types qualified as JSON
import Data.Bifunctor (Bifunctor)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Tuple.Extra ((&&&))
import Deriving.Aeson (CustomJSON (CustomJSON))
import Dhall (FromDhall (autoWith))
import GHC.Generics (Generic)
import Generic.Functor (GenericBifunctor (GenericBifunctor))
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Linear.V2 (V2)
import Orphans.V2 ()
import Util.ShowNewtype (ShowNewtypeWithoutRecord (ShowNewtypeWithoutRecord))

import Opts qualified
import Util

allAxesAndButs :: Layout a b -> ([a], [b])
allAxesAndButs layout = partitionEithers $ map element (Map.elems layout.elements.unwrap) >>= \case
    Stick s -> map Left [s.stickDataX, s.stickDataY]
    Button b -> [Right b.buttonData]
    Slider s -> [Left s.sliderData]
    Indicator _ -> []
    Empty -> []

-- | A (non-empty) list of 'Layout's.
type Layouts a b = NonEmpty (Layout a b, Maybe DhallExpr)
layoutsFromDhall :: (FromDhall a, FromDhall b) => NonEmpty Text -> IO (Maybe (Layouts a b))
layoutsFromDhall = runMaybeT . traverse \t -> do
    e <- dhallExprFromText t
    (l, _) <- dhallToHs e
    pure (l, Just e)

newtype LayoutID = LayoutID {unwrap :: Text}
    deriving newtype (Eq, Ord, Semigroup, Monoid, ToJSON, FromDhall, HasElmType, HasElmDecoder JSON.Value)
    deriving Show via (ShowNewtypeWithoutRecord "LayoutID" Text)

data Layout a b = Layout
    { elements :: ElementMap a b
    , viewBox :: ViewBox
    , backgroundColour :: Colour
    , name :: LayoutID
    }
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Layout a b)
    deriving (Bifunctor) via GenericBifunctor Layout

-- | A newtype wrapper so we can have Haskell and Elm use a Map/Dict, but Dhall expect a list.
newtype ElementMap a b = ElementMap {unwrap :: Map ElementID (FullElement a b)}
    deriving stock (Generic, Functor)
    deriving newtype (ToJSON)
    deriving (Bifunctor) via GenericBifunctor ElementMap
    deriving (Show) via ShowNewtypeWithoutRecord "ElementMap" (Map ElementID (FullElement a b))
instance (FromDhall a, FromDhall b) => FromDhall (ElementMap a b) where
    autoWith = fmap (ElementMap . Map.fromList . map ((.name) &&& id)) . autoWith

data FullElement a b = FullElement
    { element :: Element a b
    , location :: V2 Int
    , name :: ElementID
    , text :: Maybe TextBox
    , image :: Maybe Image
    , hidden :: Bool
    }
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (FullElement a b)
    deriving (Bifunctor) via GenericBifunctor FullElement

newtype ElementID = ElementID {unwrap :: Text}
    deriving stock (Eq, Ord)
    deriving newtype (FromDhall, ToJSON, ToJSONKey, FromJSON, HasElmType, HasElmEncoder J.Value, HasElmDecoder J.Value)
    deriving Show via (ShowNewtypeWithoutRecord "ElementID" Text)

data Element a b
    = Stick (Stick a)
    | Button (Button b)
    | Slider (Slider a)
    | Indicator Indicator
    | Empty
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Element a b)
    deriving (Bifunctor) via GenericBifunctor Element

data Stick a = Stick'
    { radius :: Word
    , range :: Word
    , stickColour :: Colour
    , backgroundColour :: Colour
    , stickDataX :: a
    , stickDataY :: a
    }
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Stick a)

data Button b = Button'
    { shape :: Shape
    , colour :: Colour
    , buttonData :: b
    }
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Button b)

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
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Slider a)

data Image = Image
    { width :: Word
    , height :: Word
    , url :: Text
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON Image

data TextBox = TextBox
    { text :: Text
    , style :: TextStyle
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON TextBox

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
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON Indicator

data Shape
    = Circle Word
    | Rectangle (V2 Word)
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON Shape

-- field names chosen to match 'elm-color's 'fromRgba'
data Colour = Colour
    { red :: Double
    , green :: Double
    , blue :: Double
    , alpha :: Double
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON Colour

data ViewBox = ViewBox
    { x :: Int
    , y :: Int
    , w :: Word
    , h :: Word
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON ViewBox

data TextStyle = TextStyle
    { size :: Word
    , colour :: Colour
    , bold :: Bool
    , italic :: Bool
    , underline :: Bool
    , shadow :: [TextShadow]
    , font :: Text
    -- ^ this is used directly as the value of the HTML `font-family` attribute
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON TextStyle

data TextShadow = TextShadow
    { offset :: V2 Int
    , blur :: Word
    , colour :: Colour
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON TextShadow
