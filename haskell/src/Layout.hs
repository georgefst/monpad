module Layout where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson qualified as J
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Aeson.Types qualified as JSON
import Data.Bifunctor (Bifunctor)
import Data.Colour (AlphaColour)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (CustomJSON))
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Generic.Functor (GenericBifunctor (GenericBifunctor))
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Linear.V2 (V2)
import Orphans.V2 ()
import Util.ShowNewtype (ShowNewtypeWithoutRecord (ShowNewtypeWithoutRecord))

import Orphans.Colour ()
import Opts qualified
import Util

-- | A (non-empty) list of 'Layout's.
type Layouts a b = NonEmpty (Layout a b, Maybe DhallExpr)
layoutsFromDhall :: (FromDhall a, FromDhall b) => Logger -> NonEmpty Text -> IO (Maybe (Layouts a b))
layoutsFromDhall write = runMaybeT . traverse \t -> do
    e <- dhallExprFromText write t
    (l, _) <- dhallToHs write e
    pure (l, Just e)

newtype LayoutID = LayoutID {unwrap :: Text}
    deriving newtype (Eq, Ord, Semigroup, Monoid, ToJSON, FromDhall, HasElmType, HasElmDecoder JSON.Value)
    deriving Show via (ShowNewtypeWithoutRecord "LayoutID" Text)

data Layout a b = Layout
    { elements :: [FullElement a b]
    , viewBox :: ViewBox
    , backgroundColour :: AlphaColour Double
    , name :: LayoutID
    }
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Layout a b)
    deriving (Bifunctor) via GenericBifunctor Layout

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
    deriving newtype (FromDhall, ToJSON, FromJSON, HasElmType, HasElmEncoder J.Value, HasElmDecoder J.Value)
    deriving Show via (ShowNewtypeWithoutRecord "ElementID" Text)

data Element a b
    = Stick (Stick a)
    | Button (Button b)
    | Slider (Slider a)
    | Indicator Indicator
    | Input Input
    | Empty (V2 Word) -- ^ dimensions are needed so that we can calculate an extent for text elements
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Element a b)
    deriving (Bifunctor) via GenericBifunctor Element

data Stick a = Stick'
    { radius :: Word
    , range :: Word
    , stickColour :: AlphaColour Double
    , backgroundColour :: AlphaColour Double
    , stickDataX :: a
    , stickDataY :: a
    }
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Stick a)

data Button b = Button'
    { shape :: Shape
    , colour :: AlphaColour Double
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
    , sliderColour :: AlphaColour Double
    , backgroundColour :: AlphaColour Double
    , sliderData :: a
    }
    deriving (Show, Functor, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON (Slider a)

data Input = Input'
    { width :: Word
    , height :: Word
    , inputType :: InputType
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON Input
data InputType
    = CheckBox () --TODO this dummy field works around a bug in my PR: https://github.com/folq/haskell-to-elm/pull/18
    | Number NumberInput
    | Text TextInput
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON InputType
data NumberInput = NumberInput'
    { textStyle :: TextStyle
    , min :: Maybe Double
    , max :: Maybe Double
    , step :: Maybe Double
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON NumberInput
data TextInput = TextInput'
    { textStyle :: TextStyle
    , minLength :: Maybe Word
    , maxLength :: Maybe Word
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON TextInput

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
    , colour :: AlphaColour Double
    , shape :: Shape
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON Indicator

data Shape
    = Circle Word
    | Rectangle (V2 Word)
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON Shape

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
    , colour :: AlphaColour Double
    , bold :: Bool
    , italic :: Bool
    , underline :: Bool
    , shadow :: [TextShadow]
    , rotation :: Double
    , font :: Text
    -- ^ this is used directly as the value of the HTML `font-family` attribute
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON TextStyle

data TextShadow = TextShadow
    { offset :: V2 Int
    , blur :: Word
    , colour :: AlphaColour Double
    }
    deriving (Show, Generic, FromDhall)
    deriving (ToJSON) via CustomJSON Opts.JSON TextShadow
