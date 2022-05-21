{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | All the ugly details
module GenerateElm.Via (autoDir, Via (..), Via1 (..), Via2 (..)) where

import Data.Aeson qualified as J
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Deriving.Aeson (aesonOptions)
import Generics.SOP qualified as SOP
import Language.Elm.Definition qualified as Def
import Language.Elm.Name qualified as Name
import Type.Reflection (Typeable)
import Util.Util (typeRepT)

import Language.Haskell.To.Elm

import Opts qualified

autoDir :: Text
autoDir = "Auto"

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
