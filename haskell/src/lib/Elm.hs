{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for auto generating Elm datatypes, encoders/decoders etc.
-- It's best to open this file in GHCI and run 'elm'.
-- We could make it externally executable and fully integrate with the build process, but there wouldn't be much point
-- since the kinds of changes we're likely to make which would require re-running this,
-- are likely to require manual changes to Elm code anyway.
-- e.g. if we added an extra case to 'Update', it would need to be handled in various Elm functions.
module Elm (elm) where

import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Language.Haskell.To.Elm
import Lib
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Generics.SOP as SOP
import qualified Language.Elm.Definition as Elm
import qualified Language.Elm.Expression as ElmExpr
import qualified Language.Elm.Name as Elm
import qualified Language.Elm.Pretty as Elm
import qualified Language.Elm.Simplification as Elm
import qualified Language.Elm.Type as Elm
import System.Directory
import System.FilePath

elm :: FilePath -> IO ()
elm src =
    let definitions = Elm.simplifyDefinition <$>
            jsonDefinitions @Button <> jsonDefinitions @Update <> jsonDefinitions @(V2 Double)
        modules = Elm.modules definitions
        auto = src </> "Auto"
    in do
        createDirectoryIfMissing False auto
        mapM_ (removeFile . (auto </>)) =<< listDirectory auto
        forM_ (HashMap.toList modules) \(moduleName, contents) ->
            T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
                renderStrict $ layoutPretty defaultLayoutOptions contents

--TODO we don't actually need to decode from elm (only encode)
--TODO could these be derived more easily? this is all *very* generic
    -- 'https://github.com/folq/haskell-to-elm' for some explanations
    -- keep an eye on 'https://github.com/folq/haskell-to-elm/issues/6'
instance HasElmEncoder Value Button where
    elmEncoderDefinition = elmEnc @Button "Auto.Button.encode"
instance HasElmDecoder Value Button where
    elmDecoderDefinition = elmDec @Button "Auto.Button.decode"
instance HasElmType Button where
    elmDefinition = elmTyp @Button "Auto.Button.Button"
instance HasElmEncoder Value Update where
    elmEncoderDefinition = elmEnc @Update "Auto.Update.encode"
instance HasElmDecoder Value Update where
    elmDecoderDefinition = elmDec @Update "Auto.Update.decode"
instance HasElmType Update where
    elmDefinition = elmTyp @Update "Auto.Update.Update"

elmEnc :: forall a. DeriveParameterisedElmEncoderDefinition 0 Value a => Elm.Qualified -> Maybe Elm.Definition
elmEnc s = Just $ deriveElmJSONEncoder @a defaultOptions Aeson.defaultOptions s
elmDec :: forall a. DeriveParameterisedElmDecoderDefinition 0 Value a => Elm.Qualified -> Maybe Elm.Definition
elmDec s = Just $ deriveElmJSONDecoder @a defaultOptions Aeson.defaultOptions s
elmTyp :: forall a. DeriveParameterisedElmTypeDefinition 0 a => Elm.Qualified -> Maybe Elm.Definition
elmTyp s = Just $ deriveElmTypeDefinition @a defaultOptions s


{- Link 'V2 Double' with Elm's Vec2 -}

deriving instance SOP.Generic (V2 Double)
deriving instance SOP.HasDatatypeInfo (V2 Double)
instance HasElmEncoder Value (V2 Double) where
    elmEncoder = ElmExpr.Global $ Elm.Qualified ["Util"] "encodeVec2"
instance HasElmDecoder Value (V2 Double) where
    elmDecoder = ElmExpr.Global $ Elm.Qualified ["Util"] "decodeVec2"
instance HasElmType (V2 Double) where
    elmType = Elm.Global $ Elm.Qualified ["Math","Vector2"] "Vec2"
