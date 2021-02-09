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
    Indicator _ -> []

layoutFromDhall :: (FromDhall a, FromDhall b) => Text -> IO (Layout a b)
layoutFromDhall = input auto

data Layout a b = Layout
    { elements :: [FullElement a b]
    , viewBox :: ViewBox
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
    | Indicator Indicator
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 Element
deriving via (Elm.Via2 Element) instance ToJSON (Element Unit Unit)

data Stick a = Stick'
    { radius :: Word
    , range :: Word
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
    { radius :: Word
    , length :: Word
    , width :: Word
    , sliderColour :: Colour
    , backgroundColour :: Colour
    , vertical :: Bool
    , sliderData :: a
    }
    deriving (Show, Functor, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via1 Slider
deriving via (Elm.Via1 Slider) instance ToJSON (Slider Unit)

data Image = Image'
    { width :: Word
    , height :: Word
    , url :: Text
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder JSON.Value) via Elm.Via Image

data Indicator = Indicator'
    { hollowness :: Double --TODO ignored by frontend
    -- ^ 0 to 1
    , arcStart :: Double
    -- ^ [0, 2π)
    , arcEnd :: Double
    -- ^ [0, arcStart + 2π)
    , colour :: Colour
    , radius :: Word
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

$(deriveBifunctor ''Layout)
$(deriveBifunctor ''FullElement)
$(deriveBifunctor ''Element)
