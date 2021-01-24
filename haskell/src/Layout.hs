{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Layout where

import Data.Aeson.Types (ToJSON)
import qualified Data.Aeson.Types as JSON
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Dhall (FromDhall, auto, input)
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmType)
import Linear.V2 (V2)
import Orphans.V2 ()
import Util.Elm (Unit, Via (..))
import qualified Util.Elm as Elm
import Prelude hiding (length) --TODO perhaps 'bifunctors' could just qualify?

allAxesAndButs :: Layout a b -> ([a], [b])
allAxesAndButs layout = partitionEithers $ map element layout.elements >>= \case
    Stick s -> map Left [s.stickDataX, s.stickDataY]
    Button b -> [Right b.buttonData]
    Slider s -> [Left s.sliderData]
    Image _ -> []

layoutFromDhall :: (FromDhall a, FromDhall b) => Text -> IO (Layout a b)
layoutFromDhall = input auto

data Layout a b = Layout
    { elements :: [FullElement a b]
    , grid :: V2 Int
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 Layout
deriving via (Elm.Via2 Layout) instance ToJSON (Layout Unit Unit)

data FullElement a b = FullElement
    { element :: Element a b
    , location :: V2 Int
    , name :: Text
    , showName :: Bool
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 FullElement
deriving via (Elm.Via2 FullElement) instance ToJSON (FullElement Unit Unit)

data Element a b
    = Stick (Stick a)
    | Button (Button b)
    | Slider (Slider a)
    | Image Image
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 Element
deriving via (Elm.Via2 Element) instance ToJSON (Element Unit Unit)

data Stick a = Stick'
    { radius :: Int
    , range :: Int
    , stickColour :: Colour
    , backgroundColour :: Colour
    , stickDataX :: a
    , stickDataY :: a
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via1 Stick
deriving via (Elm.Via1 Stick) instance ToJSON (Stick Unit)

data Button b = Button'
    { shape :: Shape
    , colour :: Colour
    , buttonData :: b
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via1 Button
deriving via (Elm.Via1 Button) instance ToJSON (Button Unit)

data Slider a = Slider'
    { radius :: Int
    , length :: Int
    , width :: Int
    , sliderColour :: Colour
    , backgroundColour :: Colour
    , vertical :: Bool
    , sliderData :: a
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via1 Slider
deriving via (Elm.Via1 Slider) instance ToJSON (Slider Unit)

data Image = Image'
    { width :: Int
    , height :: Int
    , url :: Text
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via Image
deriving via (Elm.Via Image) instance ToJSON Image

data Shape
    = Circle Int
    | Rectangle (V2 Int)
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via Shape
deriving via (Elm.Via Shape) instance ToJSON Shape

-- field names chosen to match 'elm-color's 'fromRgba'
data Colour = Colour
    { red :: Double
    , green :: Double
    , blue :: Double
    , alpha :: Double
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via Colour
deriving via (Elm.Via Colour) instance ToJSON Colour

$(deriveBifunctor ''Layout)
$(deriveBifunctor ''FullElement)
$(deriveBifunctor ''Element)
