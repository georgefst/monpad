{-# LANGUAGE TemplateHaskell #-}

module Layout where

import Data.Aeson.Types qualified as JSON
import Data.Aeson.Types (ToJSON)
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Dhall (FromDhall, auto, input)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmType)
import Linear.V2 (V2)
import Orphans.Tuple ()
import Orphans.V2 ()
import Util.Elm qualified as Elm

allAxesAndButs :: Layout a b -> ([a], [b])
allAxesAndButs Layout {elements} = partitionEithers $
    map element elements >>= \case
        Stick {stickDataX, stickDataY} -> map Left [stickDataX, stickDataY]
        Button {buttonData} -> [Right buttonData]
        Slider {sliderData} -> [Left sliderData]

layoutFromDhall :: (FromDhall a, FromDhall b) => Text -> IO (Layout a b)
layoutFromDhall = input auto

data Layout a b = Layout
    { elements :: [FullElement a b],
      grid :: V2 Int
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 Layout

data FullElement a b = FullElement
    { element :: Element a b,
      location :: V2 Int,
      name :: Text,
      showName :: Bool
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 FullElement

data Element a b
    = Stick
          { radius :: Int,
            range :: Int,
            stickColour :: Colour,
            backgroundColour :: Colour,
            stickDataX :: a,
            stickDataY :: a
          }
    | Button
          { shape :: Shape,
            colour :: Colour,
            buttonData :: b
          }
    | Slider
          { radius :: Int,
            length :: Int,
            width :: Int,
            sliderColour :: Colour,
            backgroundColour :: Colour,
            vertical :: Bool,
            sliderData :: a
          }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via2 Element

data Shape
    = Circle Int
    | Rectangle (V2 Int)
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via Shape

-- field names chosen to match 'elm-color's 'fromRgba'
data Colour = Colour
    { red :: Double,
      green :: Double,
      blue :: Double,
      alpha :: Double
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder JSON.Value) via Elm.Via Colour

$(deriveBifunctor ''Layout)
$(deriveBifunctor ''FullElement)
$(deriveBifunctor ''Element)
