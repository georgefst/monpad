{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-full-laziness -fforce-recomp #-}

#if defined(RELEASE)

#define GET_FILE(FILE) decodeUtf8 $(embedFile $ FILE)
{-# LANGUAGE TemplateHaskell #-}
module Embed where
import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)

#else

-- useful hack to allow assets to be loaded dynamically - the extra `()` is a tad awkward, but it works
#define GET_FILE(FILE) unsafePerformIO . T.readFile $ FILE
module Embed where
import Data.Text.IO qualified as T
import System.IO.Unsafe (unsafePerformIO)

#endif

import Data.Text (Text)
import System.FilePath ((</>))

{-# NOINLINE commonCSS #-}
{-# NOINLINE appCSS #-}
{-# NOINLINE elmJS #-}
{-# NOINLINE jsJS #-}
{-# NOINLINE defaultDhall #-}
commonCSS, loginCSS, appCSS, elmJS, jsJS, defaultDhall :: () -> Text
commonCSS () = GET_FILE("rsc" </> "common.css")
loginCSS () = GET_FILE("rsc" </> "login.css")
appCSS () = GET_FILE("rsc" </> "app.css")
elmJS () = GET_FILE("rsc" </> "elm.js")
jsJS () = GET_FILE("rsc" </> "main.js")
defaultDhall () = bracketed $ GET_FILE("rsc" </> "default.dhall")

bracketed :: Text -> Text
bracketed t = "(" <> t <> ")"
