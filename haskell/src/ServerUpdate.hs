module ServerUpdate where

import Data.Aeson (ToJSON)
import Data.Bifunctor (Bifunctor)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..))
import GHC.Generics (Generic)
import Generic.Functor (GenericBifunctor (..))
import Linear.V2 (V2)

import Layout
import Opts qualified

-- | A message generated by the server.
data ServerUpdate a b
    = PlayAudioURL Text
    | Vibrate [Int]
    -- ^ millisecond intervals: https://developer.mozilla.org/en-US/docs/Web/API/Vibration_API#vibration_patterns
    | SetImageURL ElementID Text
    | AddImage ElementID Image
    | DeleteImage ElementID
    | SetText ElementID Text
    | AddText ElementID TextBox
    | DeleteText ElementID
    | SetLayout (Layout a b)
    | SwitchLayout LayoutID
    | HideElement ElementID
    | ShowElement ElementID
    -- ^ i.e. 'unhide'
    | AddElement (FullElement a b)
    | RemoveElement ElementID
    | SetBackgroundColour Colour
    | SetIndicatorHollowness ElementID Double
    | SetIndicatorArcStart ElementID Double
    | SetIndicatorArcEnd ElementID Double
    | SetIndicatorShape ElementID Shape
    | SetIndicatorCentre ElementID (V2 Double)
    | SetIndicatorColour ElementID Colour
    | SetSliderPosition ElementID Double
    | SetButtonColour ElementID Colour
    | SetButtonPressed ElementID Bool
    | ResetLayout ResetLayout
    deriving (Show, Generic, Functor)
    deriving (ToJSON) via CustomJSON Opts.JSON (ServerUpdate a b)
    deriving (Bifunctor) via GenericBifunctor ServerUpdate

data ResetLayout
    = StateReset
    -- ^ just stick positions, buttons pressed, etc.
    | FullReset
    -- ^ return to the layout the program was initialised with (undo add/remove elements etc.)
    deriving (Show, Generic)
    deriving (ToJSON) via CustomJSON Opts.JSON ResetLayout