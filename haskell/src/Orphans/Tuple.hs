{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Tuple where

import Data.Aeson.Types qualified as JSON
import Language.Elm.Expression qualified as ElmExpr
import Language.Elm.Name qualified as ElmName
import Language.Elm.Type qualified as ElmType
import Language.Haskell.To.Elm (HasElmDecoder (elmDecoder), HasElmType (elmType))

elmUnit :: ElmName.Qualified
elmUnit = ElmName.Qualified ["Basics"] "()"

instance HasElmDecoder JSON.Value () where
    elmDecoder = ElmExpr.App (ElmExpr.Global $ ElmName.Qualified ["Json", "Decode"] "succeed") (ElmExpr.Global elmUnit)

instance HasElmType () where
    elmType = ElmType.Global elmUnit
