{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Auto-generate all required Elm code - types plus JSON encoders and decoders.
module GenerateElm (elm) where

import Data.Aeson qualified as J
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Deriving.Aeson (aesonOptions)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Language.Elm.Definition qualified as Def
import Language.Elm.Expression qualified as Expr
import Language.Elm.Name qualified as Name
import Language.Elm.Pretty qualified as Pretty
import Language.Elm.Simplification qualified as Simplify
import Language.Elm.Type qualified as Type
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (joinPath, (<.>), (</>))
import Type.Reflection (Typeable)
import Util.Util (listDirectory', typeRepT)

import Language.Haskell.To.Elm

import Monpad
import Opts qualified

{- | Auto generate Elm datatypes, encoders/decoders etc.
It's best to run this via GHCI or HLS.
We could make it externally executable and fully integrate with the build process, but there wouldn't be much point
since the kinds of changes we're likely to make which would require re-running this,
are likely to require manual changes to Elm code anyway.
e.g. if we added an extra case to 'Update', it would need to be handled in various Elm functions.
-}

-- >>> elm "elm"
elm :: FilePath -> IO ()
elm pathToElm = writeDefs pathToElm $ mconcat
    [ defAndEncoder @ClientUpdate
    , defAndEncoder @(V2 Double)
    , defAndDecoder @(ServerUpdate () ())
    , defAndDecoder @(Layout () ())
    , defAndDecoder @(FullElement () ())
    , defAndDecoder @(Element () ())
    , defAndDecoder @(Stick ())
    , defAndDecoder @(Slider ())
    , defAndDecoder @(Button ())
    , defAndDecoder @Input
    , defAndDecoder @InputType
    , defAndDecoder @NumberInput
    , defAndDecoder @TextInput
    , defAndDecoder @Image
    , defAndDecoder @TextBox
    , defAndDecoder @TextStyle
    , defAndDecoder @TextShadow
    , defAndDecoder @ResetLayout
    , defAndDecoder @Encoding
    , defAndDecoder @ElmFlags
    , defAndDecoder @ViewBox
    , defAndDecoder @Colour
    , defAndDecoder @Indicator
    , defAndDecoder @Shape
    , defAndDecoder @(V2 Int)
    ]

writeDefs :: FilePath -> [Def.Definition] -> IO ()
writeDefs elmPath defs = do
    createDirectoryIfMissing False autoFull
    mapM_ removeFile =<< listDirectory' autoFull
    for_ (HashMap.toList modules) \(moduleName, contents) ->
        T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
            renderStrict $ layoutPretty defaultLayoutOptions contents
  where
    src = elmPath </> "src"
    modules = Pretty.modules $ map Simplify.simplifyDefinition defs
    autoFull = src </> T.unpack autoDir

autoDir :: Text
autoDir = "Auto"

-- | Like 'jsonDefinitions', but for types without decoders.
defAndEncoder :: forall t. HasElmEncoder J.Value t => [Def.Definition]
defAndEncoder = catMaybes
    [ elmDefinition @t
    , elmEncoderDefinition @J.Value @t
    ]

-- | Like 'jsonDefinitions', but for types without encoders.
defAndDecoder :: forall t. HasElmDecoder J.Value t => [Def.Definition]
defAndDecoder = catMaybes
    [ elmDefinition @t
    , elmDecoderDefinition @J.Value @t
    ]

{- Instances for Monpad types -}

deriving instance SOP.Generic ClientUpdate
deriving instance SOP.HasDatatypeInfo ClientUpdate
deriving via Via ClientUpdate instance HasElmType ClientUpdate
deriving via Via ClientUpdate instance HasElmEncoder J.Value ClientUpdate

deriving instance SOP.Generic (ServerUpdate a b)
deriving instance SOP.HasDatatypeInfo (ServerUpdate a b)
deriving via Via2 ServerUpdate () () instance HasElmType (ServerUpdate a b)
deriving via Via2 ServerUpdate () () instance HasElmDecoder J.Value (ServerUpdate a b)

deriving instance SOP.Generic (Layout a b)
deriving instance SOP.HasDatatypeInfo (Layout a b)
deriving via Via2 Layout () () instance HasElmType (Layout a b)
deriving via Via2 Layout () () instance HasElmDecoder J.Value (Layout a b)

deriving instance SOP.Generic (FullElement a b)
deriving instance SOP.HasDatatypeInfo (FullElement a b)
deriving via Via2 FullElement () () instance HasElmType (FullElement a b)
deriving via Via2 FullElement () () instance HasElmDecoder J.Value (FullElement a b)

deriving instance SOP.Generic (Element a b)
deriving instance SOP.HasDatatypeInfo (Element a b)
deriving via Via2 Element () () instance HasElmType (Element a b)
deriving via Via2 Element () () instance HasElmDecoder J.Value (Element a b)

deriving instance SOP.Generic (Stick a)
deriving instance SOP.HasDatatypeInfo (Stick a)
deriving via Via1 Stick () instance HasElmType (Stick a)
deriving via Via1 Stick () instance HasElmDecoder J.Value (Stick a)

deriving instance SOP.Generic (Slider a)
deriving instance SOP.HasDatatypeInfo (Slider a)
deriving via Via1 Slider () instance HasElmType (Slider a)
deriving via Via1 Slider () instance HasElmDecoder J.Value (Slider a)

deriving instance SOP.Generic (Button a)
deriving instance SOP.HasDatatypeInfo (Button a)
deriving via Via1 Button () instance HasElmType (Button a)
deriving via Via1 Button () instance HasElmDecoder J.Value (Button a)

deriving instance SOP.Generic Input
deriving instance SOP.HasDatatypeInfo Input
deriving via Via Input instance HasElmType Input
deriving via Via Input instance HasElmDecoder J.Value Input

deriving instance SOP.Generic InputType
deriving instance SOP.HasDatatypeInfo InputType
deriving via Via InputType instance HasElmType InputType
deriving via Via InputType instance HasElmDecoder J.Value InputType

deriving instance SOP.Generic NumberInput
deriving instance SOP.HasDatatypeInfo NumberInput
deriving via Via NumberInput instance HasElmType NumberInput
deriving via Via NumberInput instance HasElmDecoder J.Value NumberInput

deriving instance SOP.Generic TextInput
deriving instance SOP.HasDatatypeInfo TextInput
deriving via Via TextInput instance HasElmType TextInput
deriving via Via TextInput instance HasElmDecoder J.Value TextInput

deriving instance SOP.Generic Image
deriving instance SOP.HasDatatypeInfo Image
deriving via Via Image instance HasElmType Image
deriving via Via Image instance HasElmDecoder J.Value Image

deriving instance SOP.Generic TextBox
deriving instance SOP.HasDatatypeInfo TextBox
deriving via Via TextBox instance HasElmType TextBox
deriving via Via TextBox instance HasElmDecoder J.Value TextBox

deriving instance SOP.Generic TextStyle
deriving instance SOP.HasDatatypeInfo TextStyle
deriving via Via TextStyle instance HasElmType TextStyle
deriving via Via TextStyle instance HasElmDecoder J.Value TextStyle

deriving instance SOP.Generic TextShadow
deriving instance SOP.HasDatatypeInfo TextShadow
deriving via Via TextShadow instance HasElmType TextShadow
deriving via Via TextShadow instance HasElmDecoder J.Value TextShadow

deriving instance SOP.Generic ResetLayout
deriving instance SOP.HasDatatypeInfo ResetLayout
deriving via Via ResetLayout instance HasElmType ResetLayout
deriving via Via ResetLayout instance HasElmDecoder J.Value ResetLayout

deriving instance SOP.Generic Encoding
deriving instance SOP.HasDatatypeInfo Encoding
deriving via Via Encoding instance HasElmType Encoding
deriving via Via Encoding instance HasElmDecoder J.Value Encoding

deriving instance SOP.Generic ElmFlags
deriving instance SOP.HasDatatypeInfo ElmFlags
deriving via Via ElmFlags instance HasElmType ElmFlags
deriving via Via ElmFlags instance HasElmDecoder J.Value ElmFlags

deriving instance SOP.Generic ViewBox
deriving instance SOP.HasDatatypeInfo ViewBox
deriving via Via ViewBox instance HasElmType ViewBox
deriving via Via ViewBox instance HasElmDecoder J.Value ViewBox

deriving instance SOP.Generic Colour
deriving instance SOP.HasDatatypeInfo Colour
deriving via Via Colour instance HasElmType Colour
deriving via Via Colour instance HasElmDecoder J.Value Colour

deriving instance SOP.Generic Indicator
deriving instance SOP.HasDatatypeInfo Indicator
deriving via Via Indicator instance HasElmType Indicator
deriving via Via Indicator instance HasElmDecoder J.Value Indicator

deriving instance SOP.Generic Shape
deriving instance SOP.HasDatatypeInfo Shape
deriving via Via Shape instance HasElmType Shape
deriving via Via Shape instance HasElmDecoder J.Value Shape

{- Instances for third-party types -}

elmUnit :: Name.Qualified
elmUnit = Name.Qualified ["Basics"] "()"
instance HasElmDecoder J.Value () where
    elmDecoder = Expr.App (Expr.Global $ Name.Qualified ["Json", "Decode"] "succeed") (Expr.Global elmUnit)
instance HasElmType () where elmType = Type.Global elmUnit

-- NB we can decode but encoding would be unsafe
instance HasElmType Word where
    elmType = "Basics.Int"
instance HasElmDecoder J.Value Word where
    elmDecoder = "Json.Decode.int"

data IntVec2 = IntVec2
    { x :: Int
    , y :: Int
    }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via Via IntVec2
data WordVec2 = WordVec2
    { x :: Word
    , y :: Word
    }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via Via IntVec2
data DoubleVec2 = DoubleVec2
    { x :: Double
    , y :: Double
    }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)

deriving instance SOP.Generic (V2 Int)
deriving instance SOP.HasDatatypeInfo (V2 Int)
instance HasElmDecoder J.Value (V2 Int) where
    elmDecoderDefinition = elmDecoderDefinition @J.Value @IntVec2
instance HasElmType (V2 Int) where
    elmDefinition = elmDefinition @IntVec2

deriving instance SOP.Generic (V2 Word)
deriving instance SOP.HasDatatypeInfo (V2 Word)
instance HasElmDecoder J.Value (V2 Word) where
    elmDecoderDefinition = elmDecoderDefinition @J.Value @WordVec2
instance HasElmType (V2 Word) where
    elmDefinition = elmDefinition @WordVec2

deriving instance SOP.Generic (V2 Double)
deriving instance SOP.HasDatatypeInfo (V2 Double)
instance HasElmDecoder J.Value (V2 Double) where
    elmDecoder = Expr.Global $ Name.Qualified ["Util"] "decodeVec2"
instance HasElmEncoder J.Value (V2 Double) where
    elmEncoder = Expr.Global $ Name.Qualified ["Util"] "encodeVec2"
instance HasElmType (V2 Double) where
    elmType = Type.Global $ Name.Qualified ["Math", "Vector2"] "Vec2"

{- All the ugly details -}

-- | A type to derive via.
newtype Via a = Via a
instance ED a a => HasElmType (Via a) where
    elmDefinition = ed @a @a
instance EED a a => HasElmEncoder J.Value (Via a) where
    elmEncoderDefinition = eed @a @a
instance EDD a a => HasElmDecoder J.Value (Via a) where
    elmDecoderDefinition = edd @a @a

newtype Via1 t a = Via1 (t a)
instance ED t (t ()) => HasElmType (Via1 t a) where
    elmDefinition = ed @t @(t ())
instance EED t (t ()) => HasElmEncoder J.Value (Via1 t ()) where
    elmEncoderDefinition = eed @t @(t ())
instance EDD t (t ()) => HasElmDecoder J.Value (Via1 t ()) where
    elmDecoderDefinition = edd @t @(t ())

newtype Via2 t a b = Via2 (t a b)
instance ED t (t () ()) => HasElmType (Via2 t a b) where
    elmDefinition = ed @t @(t () ())
instance EED t (t () ()) => HasElmEncoder J.Value (Via2 t () ()) where
    elmEncoderDefinition = eed @t @(t () ())
instance EDD t (t () ()) => HasElmDecoder J.Value (Via2 t () ()) where
    elmDecoderDefinition = edd @t @(t () ())

qual :: forall t. Typeable t => Text -> Name.Qualified
qual = Name.Qualified [autoDir, typeRepT @t]
ed :: forall t a. (DeriveParameterisedElmTypeDefinition 0 a, Typeable t) => Maybe Def.Definition
ed = Just $ deriveElmTypeDefinition @a defaultOptions $ qual @t (typeRepT @t)
eed :: forall t a. (DeriveParameterisedElmEncoderDefinition 0 J.Value a, Typeable t) => Maybe Def.Definition
eed = Just $ deriveElmJSONEncoder @a defaultOptions (aesonOptions @Opts.JSON) $ qual @t "encode"
edd :: forall t a. (DeriveParameterisedElmDecoderDefinition 0 J.Value a, Typeable t) => Maybe Def.Definition
edd = Just $ deriveElmJSONDecoder @a defaultOptions (aesonOptions @Opts.JSON) $ qual @t "decode"

type A f a = SOP.All2 f (SOP.Code a)
type ED t a = (SOP.HasDatatypeInfo a, A HasElmType a, Typeable t)
type EED t a = (ED t a, HasElmType a, A (HasElmEncoder J.Value) a)
type EDD t a = (ED t a, HasElmType a, A (HasElmDecoder J.Value) a)

instance HasElmType a => HasElmType (NonEmpty a) where
    elmType = elmType @[a]
instance HasElmDecoder J.Value a => HasElmDecoder J.Value (NonEmpty a) where
    elmDecoder = elmDecoder @J.Value @[a]
