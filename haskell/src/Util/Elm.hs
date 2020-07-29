{-# LANGUAGE UndecidableInstances #-}

module Util.Elm (
    writeDefs,
    Via,
    Via1,
    Via2,
    encodedTypes,
    decodedTypes,
    jsonDefinitions,
    autoDir,
) where

import Data.Aeson as JSON
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Generics.SOP qualified as SOP
import GHC.Generics (Generic, Rep)
import Language.Elm.Definition
import Language.Elm.Name
import Language.Haskell.To.Elm as Elm
import Type.Reflection (Typeable)

import Control.Monad (forM_)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Language.Elm.Pretty qualified as Pretty
import Language.Elm.Simplification (simplifyDefinition)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (joinPath, (<.>), (</>))

import Util

writeDefs :: FilePath -> [Definition] -> IO ()
writeDefs src defs =
    let modules = Pretty.modules $ map simplifyDefinition defs
        autoFull = src </> T.unpack autoDir
    in do
        createDirectoryIfMissing False autoFull
        mapM_ removeFile =<< listDirectory' autoFull
        forM_ (HashMap.toList modules) \(moduleName, contents) ->
            T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
                renderStrict $ layoutPretty defaultLayoutOptions contents

jsonOpts :: JSON.Options
jsonOpts = JSON.defaultOptions --TODO {sumEncoding = ObjectWithSingleField} -- not yet in haskell-to-elm

elmOpts :: Elm.Options
elmOpts = Elm.defaultOptions

-- | A type to derive via.
newtype Via a = Via a
instance (Generic a, GToJSON Zero (Rep a), Typeable a) => ToJSON (Via a) where
    toJSON (Via a) = genericToJSON jsonOpts a
instance (Generic a, GFromJSON Zero (Rep a), Typeable a) => FromJSON (Via a) where
    parseJSON = fmap Via . genericParseJSON jsonOpts
instance (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a), Typeable a) =>
    HasElmType (Via a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @a elmOpts $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmEncoder Value) (SOP.Code a), HasElmType (Via a), Typeable a) =>
    HasElmEncoder Value (Via a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @a elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmDecoder Value) (SOP.Code a), HasElmType (Via a), Typeable a) =>
    HasElmDecoder Value (Via a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @a elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "decode"

newtype Via1 a = Via1 (a ())
instance (Generic (a ()), GToJSON Zero (Rep (a ())), Typeable (a ())) => ToJSON (Via1 a) where
    toJSON (Via1 a) = genericToJSON jsonOpts a
instance (Generic (a ()), GFromJSON Zero (Rep (a ())), Typeable (a ())) => FromJSON (Via1 a) where
    parseJSON = fmap Via1 . genericParseJSON jsonOpts
instance (SOP.HasDatatypeInfo (a ()), SOP.All2 HasElmType (SOP.Code (a ())), Typeable a) =>
    HasElmType (Via1 a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @(a ()) elmOpts $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo (a ()), HasElmType (a ()), SOP.All2 (HasElmEncoder Value) (SOP.Code (a ())), HasElmType (Via1 a), Typeable a) =>
    HasElmEncoder Value (Via1 a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @(a ()) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo (a ()), HasElmType (a ()), SOP.All2 (HasElmDecoder Value) (SOP.Code (a ())), HasElmType (Via1 a), Typeable a) =>
    HasElmDecoder Value (Via1 a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @(a ()) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "decode"

newtype Via2 a = Via2 (a () ())
instance (Generic (a () ()), GToJSON Zero (Rep (a () ())), Typeable (a () ())) => ToJSON (Via2 a) where
    toJSON (Via2 a) = genericToJSON jsonOpts a
instance (Generic (a () ()), GFromJSON Zero (Rep (a () ())), Typeable (a () ())) => FromJSON (Via2 a) where
    parseJSON = fmap Via2 . genericParseJSON jsonOpts
instance (SOP.HasDatatypeInfo (a () ()), SOP.All2 HasElmType (SOP.Code (a () ())), Typeable a) =>
    HasElmType (Via2 a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @(a () ()) elmOpts $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo (a () ()), HasElmType (a () ()), SOP.All2 (HasElmEncoder Value) (SOP.Code (a () ())), HasElmType (Via2 a), Typeable a) =>
    HasElmEncoder Value (Via2 a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @(a () ()) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo (a () ()), HasElmType (a () ()), SOP.All2 (HasElmDecoder Value) (SOP.Code (a () ())), HasElmType (Via2 a), Typeable a) =>
    HasElmDecoder Value (Via2 a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @(a () ()) elmOpts jsonOpts $ Qualified [autoDir, typeRepT @a] "decode"

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
