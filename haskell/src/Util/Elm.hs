{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.Elm (
    Via (..),
    Via1 (..),
    Via2 (..),
    writeDefs,
    encodedTypes,
    decodedTypes,
) where

import Control.Monad (forM_)
import Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HashMap
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

elmUnit :: Name.Qualified
elmUnit = Name.Qualified ["Basics"] "()"
instance HasElmDecoder JSON.Value () where
    elmDecoder = Expr.App (Expr.Global $ Name.Qualified ["Json", "Decode"] "succeed") (Expr.Global elmUnit)
instance HasElmType () where elmType = Type.Global elmUnit

writeDefs :: FilePath -> [Definition] -> IO ()
writeDefs elm defs = do
    createDirectoryIfMissing False autoFull
    mapM_ removeFile =<< listDirectory' autoFull
    forM_ (HashMap.toList modules) \(moduleName, contents) ->
        T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
            renderStrict $ layoutPretty defaultLayoutOptions contents
  where
    src = elm </> "src"
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
