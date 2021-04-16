{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GenerateElm where

import Control.Monad (forM_)
import Data.Aeson as JSON
import Data.Aeson qualified as J
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import GHC.Generics (Generic, Rep)
import Generics.SOP qualified as SOP
import Language.Elm.Definition (Definition)
import Language.Elm.Expression qualified as Expr
import Language.Elm.Name (Qualified (Qualified))
import Language.Elm.Name qualified as Name
import Language.Elm.Pretty qualified as Pretty
import Language.Elm.Simplification (simplifyDefinition)
import Language.Elm.Type qualified as Type
import Language.Haskell.To.Elm as Elm
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (joinPath, (<.>), (</>))
import Type.Reflection (Typeable)
import Util.Util (listDirectory', typeRepT)

import Monpad

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
    [ decodedTypes @ClientUpdate
    , decodedTypes @(V2 Double)
    , encodedTypes @(ServerUpdate () ())
    , encodedTypes @(Layout () ())
    , encodedTypes @(FullElement () ())
    , encodedTypes @(Element () ())
    , encodedTypes @(Stick ())
    , encodedTypes @(Slider ())
    , encodedTypes @(Button ())
    , encodedTypes @Image
    , encodedTypes @TextBox
    , encodedTypes @TextStyle
    , encodedTypes @TextShadow
    , encodedTypes @ResetLayout
    , encodedTypes @ElmFlags
    , encodedTypes @ViewBox
    , encodedTypes @Colour
    , encodedTypes @Indicator
    , encodedTypes @Shape
    , encodedTypes @(V2 Int)
    ]

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

{- Vectors -}

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

-- NB we can decode but encoding would be unsafe
instance HasElmType Word where
    elmType = "Basics.Int"
instance HasElmDecoder J.Value Word where
    elmDecoder = "Json.Decode.int"

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

elmUnit :: Name.Qualified
elmUnit = Name.Qualified ["Basics"] "()"
instance HasElmDecoder JSON.Value () where
    elmDecoder = Expr.App (Expr.Global $ Name.Qualified ["Json", "Decode"] "succeed") (Expr.Global elmUnit)
instance HasElmType () where elmType = Type.Global elmUnit

writeDefs :: FilePath -> [Definition] -> IO ()
writeDefs elmPath defs = do
    createDirectoryIfMissing False autoFull
    mapM_ removeFile =<< listDirectory' autoFull
    forM_ (HashMap.toList modules) \(moduleName, contents) ->
        T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
            renderStrict $ layoutPretty defaultLayoutOptions contents
  where
    src = elmPath </> "src"
    modules = Pretty.modules $ map simplifyDefinition defs
    autoFull = src </> T.unpack autoDir

jsonOpts :: JSON.Options
jsonOpts = JSON.defaultOptions{sumEncoding = ObjectWithSingleField}

elmOpts :: Elm.Options
elmOpts = Elm.defaultOptions

-- | A type to derive via.
newtype Via a = Via a
instance ED a a => HasElmType (Via a) where
    elmDefinition = ed @a @a
instance EED a a => HasElmEncoder Value (Via a) where
    elmEncoderDefinition = eed @a @a
instance EDD a a => HasElmDecoder Value (Via a) where
    elmDecoderDefinition = edd @a @a

newtype Via1 t a = Via1 (t a)
instance ED t (t ()) => HasElmType (Via1 t a) where
    elmDefinition = ed @t @(t ())
instance EED t (t ()) => HasElmEncoder Value (Via1 t ()) where
    elmEncoderDefinition = eed @t @(t ())
instance EDD t (t ()) => HasElmDecoder Value (Via1 t ()) where
    elmDecoderDefinition = edd @t @(t ())

newtype Via2 t a b = Via2 (t a b)
instance ED t (t () ()) => HasElmType (Via2 t a b) where
    elmDefinition = ed @t @(t () ())
instance EED t (t () ()) => HasElmEncoder Value (Via2 t () ()) where
    elmEncoderDefinition = eed @t @(t () ())
instance EDD t (t () ()) => HasElmDecoder Value (Via2 t () ()) where
    elmDecoderDefinition = edd @t @(t () ())

autoDir :: Text
autoDir = "Auto"

-- | Like 'jsonDefinitions', but for types without decoders.
decodedTypes :: forall t. HasElmEncoder Value t => [Definition]
decodedTypes = catMaybes
    [ elmDefinition @t
    , elmEncoderDefinition @Value @t
    ]

-- | Like 'jsonDefinitions', but for types without encoders.
encodedTypes :: forall t. HasElmDecoder Value t => [Definition]
encodedTypes = catMaybes
    [ elmDefinition @t
    , elmDecoderDefinition @Value @t
    ]

qual :: forall t. Typeable t => Text -> Qualified
qual = Qualified [autoDir, typeRepT @t]
tj :: (Generic a, GToJSON' Value Zero (Rep a)) => (b -> a) -> b -> Value
tj f = genericToJSON jsonOpts . f
pj :: (Generic a, GFromJSON Zero (Rep a)) => (a -> b) -> Value -> Parser b
pj v = fmap v . genericParseJSON jsonOpts
ed :: forall t a. (DeriveParameterisedElmTypeDefinition 0 a, Typeable t) => Maybe Definition
ed = Just $ deriveElmTypeDefinition @a elmOpts $ qual @t (typeRepT @t)
eed :: forall t a. (DeriveParameterisedElmEncoderDefinition 0 Value a, Typeable t) => Maybe Definition
eed = Just $ deriveElmJSONEncoder @a elmOpts jsonOpts $ qual @t "encode"
edd :: forall t a. (DeriveParameterisedElmDecoderDefinition 0 Value a, Typeable t) => Maybe Definition
edd = Just $ Elm.deriveElmJSONDecoder @a elmOpts jsonOpts $ qual @t "decode"

type A f a = SOP.All2 f (SOP.Code a)
type ED t a = (SOP.HasDatatypeInfo a, A HasElmType a, Typeable t)
type EED t a = (ED t a, HasElmType a, A (HasElmEncoder Value) a)
type EDD t a = (ED t a, HasElmType a, A (HasElmDecoder Value) a)

instance HasElmType a => HasElmType (NonEmpty a) where
    elmType = elmType @[a]
instance HasElmDecoder J.Value a => HasElmDecoder J.Value (NonEmpty a) where
    elmDecoder = elmDecoder @J.Value @[a]
