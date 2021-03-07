{-# LANGUAGE UndecidableInstances #-}

module Util.Elm (
    writeDefs,
    Via (..),
    Via1 (..),
    Via2 (..),
    encodedTypes,
    decodedTypes,
    jsonDefinitions,
    autoDir,
    Unit (..),
) where

import Control.Monad (forM_)
import Data.Aeson as JSON
import Data.Char (toLower)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Dhall (FromDhall)
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

import Util

{- | Isomorphic to '()', but we want to decode to empty record, rather than list, in order to match 'dhall-to-json'.
Also avoids orphans for 'HasElmType' etc.
-}
data Unit = Unit
    deriving (Show, Generic, FromDhall)
instance Semigroup Unit where Unit <> Unit = Unit
instance Monoid Unit where mempty = Unit
elmUnit :: Name.Qualified
elmUnit = Name.Qualified ["Basics"] "()"
instance HasElmDecoder JSON.Value Unit where
    elmDecoder = Expr.App (Expr.Global $ Name.Qualified ["Json", "Decode"] "succeed") (Expr.Global elmUnit)
instance HasElmType Unit where elmType = Type.Global elmUnit
instance ToJSON Unit where toJSON Unit = Object HashMap.empty

writeDefs :: FilePath -> [Definition] -> IO ()
writeDefs src defs = do
    createDirectoryIfMissing False autoFull
    mapM_ removeFile =<< listDirectory' autoFull
    forM_ (HashMap.toList modules) \(moduleName, contents) ->
        T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
            renderStrict $ layoutPretty defaultLayoutOptions contents
  where
    modules = Pretty.modules $ map simplifyDefinition defs
    autoFull = src </> T.unpack autoDir

jsonOpts :: JSON.Options
jsonOpts =
    JSON.defaultOptions
        { sumEncoding = ObjectWithSingleField
        , JSON.constructorTagModifier = \case
            c : cs -> toLower c : cs
            "" -> ""
        }

elmOpts :: Elm.Options
elmOpts = Elm.defaultOptions

-- | A type to derive via.
newtype Via a = Via a
instance (Generic a, GToJSON Zero (Rep a), Typeable a) => ToJSON (Via a) where
    toJSON (Via a) = genericToJSON jsonOpts a
instance (Generic a, GFromJSON Zero (Rep a), Typeable a) => FromJSON (Via a) where
    parseJSON = fmap Via . genericParseJSON jsonOpts
instance
    (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a), Typeable a) =>
    HasElmType (Via a)
    where
    elmDefinition =
        Just $ deriveElmTypeDefinition @a elmOpts $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance
    (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmEncoder Value) (SOP.Code a), HasElmType (Via a), Typeable a) =>
    HasElmEncoder Value (Via a)
    where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @a elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "encode"
instance
    (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmDecoder Value) (SOP.Code a), HasElmType (Via a), Typeable a) =>
    HasElmDecoder Value (Via a)
    where
    elmDecoderDefinition =
        Just $ Elm.deriveElmJSONDecoder @a elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "decode"

newtype Via1 a = Via1 (a Unit)
instance (Generic (a Unit), GToJSON Zero (Rep (a Unit))) => ToJSON (Via1 a) where
    toJSON (Via1 a) = genericToJSON jsonOpts a
instance (Generic (a Unit), GFromJSON Zero (Rep (a Unit))) => FromJSON (Via1 a) where
    parseJSON = fmap Via1 . genericParseJSON jsonOpts
instance
    (SOP.HasDatatypeInfo (a Unit), SOP.All2 HasElmType (SOP.Code (a Unit)), Typeable a) =>
    HasElmType (Via1 a)
    where
    elmDefinition =
        Just $ deriveElmTypeDefinition @(a Unit) elmOpts $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance
    (SOP.HasDatatypeInfo (a Unit), HasElmType (a Unit), SOP.All2 (HasElmEncoder Value) (SOP.Code (a Unit)), HasElmType (Via1 a), Typeable a) =>
    HasElmEncoder Value (Via1 a)
    where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @(a Unit) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "encode"
instance
    (SOP.HasDatatypeInfo (a Unit), HasElmType (a Unit), SOP.All2 (HasElmDecoder Value) (SOP.Code (a Unit)), HasElmType (Via1 a), Typeable a) =>
    HasElmDecoder Value (Via1 a)
    where
    elmDecoderDefinition =
        Just $ Elm.deriveElmJSONDecoder @(a Unit) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "decode"

newtype Via2 a = Via2 (a Unit Unit)
instance (Generic (a Unit Unit), GToJSON Zero (Rep (a Unit Unit))) => ToJSON (Via2 a) where
    toJSON (Via2 a) = genericToJSON jsonOpts a
instance (Generic (a Unit Unit), GFromJSON Zero (Rep (a Unit Unit))) => FromJSON (Via2 a) where
    parseJSON = fmap Via2 . genericParseJSON jsonOpts
instance
    (SOP.HasDatatypeInfo (a Unit Unit), SOP.All2 HasElmType (SOP.Code (a Unit Unit)), Typeable a) =>
    HasElmType (Via2 a)
    where
    elmDefinition =
        Just $ deriveElmTypeDefinition @(a Unit Unit) elmOpts $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance
    (SOP.HasDatatypeInfo (a Unit Unit), HasElmType (a Unit Unit), SOP.All2 (HasElmEncoder Value) (SOP.Code (a Unit Unit)), HasElmType (Via2 a), Typeable a) =>
    HasElmEncoder Value (Via2 a)
    where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @(a Unit Unit) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "encode"
instance
    (SOP.HasDatatypeInfo (a Unit Unit), HasElmType (a Unit Unit), SOP.All2 (HasElmDecoder Value) (SOP.Code (a Unit Unit)), HasElmType (Via2 a), Typeable a) =>
    HasElmDecoder Value (Via2 a)
    where
    elmDecoderDefinition =
        Just $ Elm.deriveElmJSONDecoder @(a Unit Unit) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "decode"

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
