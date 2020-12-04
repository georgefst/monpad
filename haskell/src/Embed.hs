{-# LANGUAGE CPP #-}

#if defined(RELEASE)

{-# LANGUAGE TemplateHaskell #-}
module Embed where

import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath

mainCSS, elmJS, jsJS :: () -> Text
mainCSS () = decodeUtf8 $(embedFile $ "rsc" </> "main.css")
elmJS () = decodeUtf8 $(embedFile $ "rsc" </> "dist" </> "elm.js")
jsJS () = decodeUtf8 $(embedFile $ "rsc" </> "main.js")
defaultDhall :: Text
defaultDhall = bracketed $ decodeUtf8 $(embedFile $ "rsc" </> "dist" </> "default.dhall")
mapLayoutDhall :: Text
mapLayoutDhall = bracketed $ decodeUtf8 $(embedFile $ "rsc" </> "dist" </> "map-layout.dhall")

#else

-- useful unsafe hack to allow assets to be loaded dynamically
-- the extra `()` is a tad awkward, but it works
-- we DO NOT want this in a release build

{-# OPTIONS_GHC -fno-full-laziness -fforce-recomp #-}

module Embed where

import Data.Text (Text)
import Data.Text.IO qualified as T
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

mainCSS, elmJS, jsJS :: () -> Text
{-# NOINLINE mainCSS #-}
mainCSS () = unsafePerformIO $ T.readFile $ "rsc" </> "main.css"
{-# NOINLINE elmJS #-}
elmJS () = unsafePerformIO $ T.readFile $ "rsc" </> "dist" </> "elm.js"
{-# NOINLINE jsJS #-}
jsJS () = unsafePerformIO $ T.readFile $ "rsc" </> "main.js"
defaultDhall :: Text
defaultDhall = bracketed "./../dhall/default.dhall"
mapLayoutDhall :: Text
mapLayoutDhall = bracketed "./../dhall/map-layout.dhall"

#endif

bracketed :: Text -> Text
bracketed t = "(" <> t <> ")"
