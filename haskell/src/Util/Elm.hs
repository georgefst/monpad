{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Elm (
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
import qualified Language.Elm.Expression as Expr
import qualified Language.Elm.Name as Name
import qualified Language.Elm.Type as Type
import Type.Reflection (Typeable)

import Util

-- | A type to derive via.
newtype Via a = Via a
instance (Generic a, GToJSON Zero (Rep a), Typeable a) => ToJSON (Via a) where
    toJSON (Via a) = genericToJSON JSON.defaultOptions a
instance (Generic a, GFromJSON Zero (Rep a), Typeable a) => FromJSON (Via a) where
    parseJSON = fmap Via . genericParseJSON JSON.defaultOptions
instance (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a), Typeable a) =>
    HasElmType (Via a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @a Elm.defaultOptions $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmEncoder Value) (SOP.Code a), HasElmType (Via a), Typeable a) =>
    HasElmEncoder Value (Via a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @a Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmDecoder Value) (SOP.Code a), HasElmType (Via a), Typeable a) =>
    HasElmDecoder Value (Via a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @a Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "decode"

newtype Via1 a = Via1 (a ())
instance (Generic (a ()), GToJSON Zero (Rep (a ())), Typeable (a ())) => ToJSON (Via1 a) where
    toJSON (Via1 a) = genericToJSON JSON.defaultOptions a
instance (Generic (a ()), GFromJSON Zero (Rep (a ())), Typeable (a ())) => FromJSON (Via1 a) where
    parseJSON = fmap Via1 . genericParseJSON JSON.defaultOptions
instance (SOP.HasDatatypeInfo (a ()), SOP.All2 HasElmType (SOP.Code (a ())), Typeable a) =>
    HasElmType (Via1 a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @(a ()) Elm.defaultOptions $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo (a ()), HasElmType (a ()), SOP.All2 (HasElmEncoder Value) (SOP.Code (a ())), HasElmType (Via1 a), Typeable a) =>
    HasElmEncoder Value (Via1 a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @(a ()) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo (a ()), HasElmType (a ()), SOP.All2 (HasElmDecoder Value) (SOP.Code (a ())), HasElmType (Via1 a), Typeable a) =>
    HasElmDecoder Value (Via1 a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @(a ()) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "decode"

newtype Via2 a = Via2 (a () ())
instance (Generic (a () ()), GToJSON Zero (Rep (a () ())), Typeable (a () ())) => ToJSON (Via2 a) where
    toJSON (Via2 a) = genericToJSON JSON.defaultOptions a
instance (Generic (a () ()), GFromJSON Zero (Rep (a () ())), Typeable (a () ())) => FromJSON (Via2 a) where
    parseJSON = fmap Via2 . genericParseJSON JSON.defaultOptions
instance (SOP.HasDatatypeInfo (a () ()), SOP.All2 HasElmType (SOP.Code (a () ())), Typeable a) =>
    HasElmType (Via2 a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @(a () ()) Elm.defaultOptions $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo (a () ()), HasElmType (a () ()), SOP.All2 (HasElmEncoder Value) (SOP.Code (a () ())), HasElmType (Via2 a), Typeable a) =>
    HasElmEncoder Value (Via2 a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @(a () ()) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo (a () ()), HasElmType (a () ()), SOP.All2 (HasElmDecoder Value) (SOP.Code (a () ())), HasElmType (Via2 a), Typeable a) =>
    HasElmDecoder Value (Via2 a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @(a () ()) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "decode"

elmUnit :: Qualified
elmUnit = Name.Qualified ["Basics"] "()"
instance HasElmDecoder JSON.Value () where
    elmDecoder = Expr.App (Expr.Global $ Name.Qualified ["Json", "Decode"] "succeed") (Expr.Global elmUnit)
instance HasElmType () where
    elmType = Type.Global elmUnit

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
