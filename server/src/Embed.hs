{-# LANGUAGE TemplateHaskell #-}
module Embed where

import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

mainCSS, elmJS, jsJS :: Text
mainCSS = decodeUtf8 $(embedFile "../client/manual/main.css")
elmJS = decodeUtf8 $(embedFile "../client/dist/elm.js")
jsJS = decodeUtf8 $(embedFile "../client/manual/js.js")
