{-# LANGUAGE UndecidableInstances #-}

module Util.Elm (
    Via,
    Via1,
    Via2,
    Unit(Unit),
    encodedTypes,
    decodedTypes,
    jsonDefinitions,
    autoDir,
) where

import Data.Aeson as JSON
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Generics.SOP qualified as SOP
import Language.Elm.Definition
import Language.Elm.Name
import Language.Haskell.To.Elm as Elm
import Type.Reflection (Typeable, typeRep)
import Dhall (FromDhall)

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

newtype Via1 a = Via1 (a Unit)
instance (Generic (a Unit), GToJSON Zero (Rep (a Unit)), Typeable (a Unit)) => ToJSON (Via1 a) where
    toJSON (Via1 a) = genericToJSON JSON.defaultOptions a
instance (Generic (a Unit), GFromJSON Zero (Rep (a Unit)), Typeable (a Unit)) => FromJSON (Via1 a) where
    parseJSON = fmap Via1 . genericParseJSON JSON.defaultOptions
instance (SOP.HasDatatypeInfo (a Unit), SOP.All2 HasElmType (SOP.Code (a Unit)), Typeable a) =>
    HasElmType (Via1 a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @(a Unit) Elm.defaultOptions $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo (a Unit), HasElmType (a Unit), SOP.All2 (HasElmEncoder Value) (SOP.Code (a Unit)), HasElmType (Via1 a), Typeable a) =>
    HasElmEncoder Value (Via1 a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @(a Unit) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo (a Unit), HasElmType (a Unit), SOP.All2 (HasElmDecoder Value) (SOP.Code (a Unit)), HasElmType (Via1 a), Typeable a) =>
    HasElmDecoder Value (Via1 a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @(a Unit) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "decode"

newtype Via2 a = Via2 (a Unit Unit)
instance (Generic (a Unit Unit), GToJSON Zero (Rep (a Unit Unit)), Typeable (a Unit Unit)) => ToJSON (Via2 a) where
    toJSON (Via2 a) = genericToJSON JSON.defaultOptions a
instance (Generic (a Unit Unit), GFromJSON Zero (Rep (a Unit Unit)), Typeable (a Unit Unit)) => FromJSON (Via2 a) where
    parseJSON = fmap Via2 . genericParseJSON JSON.defaultOptions
instance (SOP.HasDatatypeInfo (a Unit Unit), SOP.All2 HasElmType (SOP.Code (a Unit Unit)), Typeable a) =>
    HasElmType (Via2 a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @(a Unit Unit) Elm.defaultOptions $ Qualified [autoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo (a Unit Unit), HasElmType (a Unit Unit), SOP.All2 (HasElmEncoder Value) (SOP.Code (a Unit Unit)), HasElmType (Via2 a), Typeable a) =>
    HasElmEncoder Value (Via2 a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @(a Unit Unit) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo (a Unit Unit), HasElmType (a Unit Unit), SOP.All2 (HasElmDecoder Value) (SOP.Code (a Unit Unit)), HasElmType (Via2 a), Typeable a) =>
    HasElmDecoder Value (Via2 a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @(a Unit Unit) Elm.defaultOptions JSON.defaultOptions $ Qualified [autoDir, typeRepT @a] "decode"

--TODO just use '()' ?
    -- syntactically awkward to link to Elm
    -- also the idea was to have instances where Unit-valued fields are omitted
        -- still unsure how best to achieve this
data Unit = Unit
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON, FromDhall)
    deriving (HasElmType, HasElmEncoder Value, HasElmDecoder Value) via Via Unit

autoDir :: Text
autoDir = "Auto"

-- | Like 'jsonDefinitions', but for types without decoders.
decodedTypes :: forall t. (HasElmEncoder Value t) => [Definition]
decodedTypes = catMaybes
    [ elmDefinition @t
    , elmEncoderDefinition @Value @t
    ]

-- | Like 'jsonDefinitions', but for types without encoders.
encodedTypes :: forall t. (HasElmDecoder Value t) => [Definition]
encodedTypes = catMaybes
    [ elmDefinition @t
    , elmDecoderDefinition @Value @t
    ]
