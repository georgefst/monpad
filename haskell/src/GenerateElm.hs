{-# OPTIONS_GHC -Wno-orphans #-}

module GenerateElm where

import Data.Aeson qualified as J
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)

import Monpad
import Orphans.Elm ()
import Util.Elm qualified as Elm

{- | Auto generate Elm datatypes, encoders/decoders etc.
It's best to run this via GHCI or HLS.
We could make it externally executable and fully integrate with the build process, but there wouldn't be much point
since the kinds of changes we're likely to make which would require re-running this,
are likely to require manual changes to Elm code anyway.
e.g. if we added an extra case to 'Update', it would need to be handled in various Elm functions.
-}

-- >>> elm "elm"
elm :: FilePath -> IO ()
elm pathToElm = Elm.writeDefs pathToElm $ mconcat
    [ Elm.decodedTypes @ClientUpdate
    , Elm.decodedTypes @(V2 Double)
    , Elm.encodedTypes @(ServerUpdate () ())
    , Elm.encodedTypes @(Layout () ())
    , Elm.encodedTypes @(FullElement () ())
    , Elm.encodedTypes @(Element () ())
    , Elm.encodedTypes @(Stick ())
    , Elm.encodedTypes @(Slider ())
    , Elm.encodedTypes @(Button ())
    , Elm.encodedTypes @Image
    , Elm.encodedTypes @TextBox
    , Elm.encodedTypes @TextStyle
    , Elm.encodedTypes @TextShadow
    , Elm.encodedTypes @ResetLayout
    , Elm.encodedTypes @ElmFlags
    , Elm.encodedTypes @ViewBox
    , Elm.encodedTypes @Colour
    , Elm.encodedTypes @Indicator
    , Elm.encodedTypes @Shape
    , Elm.encodedTypes @(V2 Int)
    , Elm.encodedTypes @Unit
    ]

deriving instance SOP.Generic ClientUpdate
deriving instance SOP.HasDatatypeInfo ClientUpdate
deriving via Elm.Via ClientUpdate instance HasElmType ClientUpdate
deriving via Elm.Via ClientUpdate instance HasElmEncoder J.Value ClientUpdate

deriving instance SOP.Generic (ServerUpdate a b)
deriving instance SOP.HasDatatypeInfo (ServerUpdate a b)
deriving via Elm.Via2 ServerUpdate Unit Unit instance HasElmType (ServerUpdate a b)
deriving via Elm.Via2 ServerUpdate Unit Unit instance HasElmDecoder J.Value (ServerUpdate a b)

deriving instance SOP.Generic (Layout a b)
deriving instance SOP.HasDatatypeInfo (Layout a b)
deriving via Elm.Via2 Layout Unit Unit instance HasElmType (Layout a b)
deriving via Elm.Via2 Layout Unit Unit instance HasElmDecoder J.Value (Layout a b)

deriving instance SOP.Generic (FullElement a b)
deriving instance SOP.HasDatatypeInfo (FullElement a b)
deriving via Elm.Via2 FullElement Unit Unit instance HasElmType (FullElement a b)
deriving via Elm.Via2 FullElement Unit Unit instance HasElmDecoder J.Value (FullElement a b)

deriving instance SOP.Generic (Element a b)
deriving instance SOP.HasDatatypeInfo (Element a b)
deriving via Elm.Via2 Element Unit Unit instance HasElmType (Element a b)
deriving via Elm.Via2 Element Unit Unit instance HasElmDecoder J.Value (Element a b)

deriving instance SOP.Generic (Stick a)
deriving instance SOP.HasDatatypeInfo (Stick a)
deriving via Elm.Via1 Stick Unit instance HasElmType (Stick a)
deriving via Elm.Via1 Stick Unit instance HasElmDecoder J.Value (Stick a)

deriving instance SOP.Generic (Slider a)
deriving instance SOP.HasDatatypeInfo (Slider a)
deriving via Elm.Via1 Slider Unit instance HasElmType (Slider a)
deriving via Elm.Via1 Slider Unit instance HasElmDecoder J.Value (Slider a)

deriving instance SOP.Generic (Button a)
deriving instance SOP.HasDatatypeInfo (Button a)
deriving via Elm.Via1 Button Unit instance HasElmType (Button a)
deriving via Elm.Via1 Button Unit instance HasElmDecoder J.Value (Button a)

deriving instance SOP.Generic Image
deriving instance SOP.HasDatatypeInfo Image
deriving via Elm.Via Image instance HasElmType Image
deriving via Elm.Via Image instance HasElmDecoder J.Value Image

deriving instance SOP.Generic TextBox
deriving instance SOP.HasDatatypeInfo TextBox
deriving via Elm.Via TextBox instance HasElmType TextBox
deriving via Elm.Via TextBox instance HasElmDecoder J.Value TextBox

deriving instance SOP.Generic TextStyle
deriving instance SOP.HasDatatypeInfo TextStyle
deriving via Elm.Via TextStyle instance HasElmType TextStyle
deriving via Elm.Via TextStyle instance HasElmDecoder J.Value TextStyle

deriving instance SOP.Generic TextShadow
deriving instance SOP.HasDatatypeInfo TextShadow
deriving via Elm.Via TextShadow instance HasElmType TextShadow
deriving via Elm.Via TextShadow instance HasElmDecoder J.Value TextShadow

deriving instance SOP.Generic ResetLayout
deriving instance SOP.HasDatatypeInfo ResetLayout
deriving via Elm.Via ResetLayout instance HasElmType ResetLayout
deriving via Elm.Via ResetLayout instance HasElmDecoder J.Value ResetLayout

deriving instance SOP.Generic ElmFlags
deriving instance SOP.HasDatatypeInfo ElmFlags
deriving via Elm.Via ElmFlags instance HasElmType ElmFlags
deriving via Elm.Via ElmFlags instance HasElmDecoder J.Value ElmFlags

deriving instance SOP.Generic ViewBox
deriving instance SOP.HasDatatypeInfo ViewBox
deriving via Elm.Via ViewBox instance HasElmType ViewBox
deriving via Elm.Via ViewBox instance HasElmDecoder J.Value ViewBox

deriving instance SOP.Generic Colour
deriving instance SOP.HasDatatypeInfo Colour
deriving via Elm.Via Colour instance HasElmType Colour
deriving via Elm.Via Colour instance HasElmDecoder J.Value Colour

deriving instance SOP.Generic Indicator
deriving instance SOP.HasDatatypeInfo Indicator
deriving via Elm.Via Indicator instance HasElmType Indicator
deriving via Elm.Via Indicator instance HasElmDecoder J.Value Indicator

deriving instance SOP.Generic Shape
deriving instance SOP.HasDatatypeInfo Shape
deriving via Elm.Via Shape instance HasElmType Shape
deriving via Elm.Via Shape instance HasElmDecoder J.Value Shape
