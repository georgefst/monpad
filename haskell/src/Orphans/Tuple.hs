{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Tuple where

import qualified Data.Aeson.Types as JSON
import qualified Language.Elm.Expression as ElmExpr
import qualified Language.Elm.Name as ElmName
import qualified Language.Elm.Type as ElmType
import Language.Haskell.To.Elm (HasElmDecoder (elmDecoder), HasElmType (elmType))

elmUnit :: ElmName.Qualified
elmUnit = ElmName.Qualified ["Basics"] "()"

instance HasElmDecoder JSON.Value () where
    elmDecoder = ElmExpr.App (ElmExpr.Global $ ElmName.Qualified ["Json", "Decode"] "succeed") (ElmExpr.Global elmUnit)

instance HasElmType () where
    elmType = ElmType.Global elmUnit
