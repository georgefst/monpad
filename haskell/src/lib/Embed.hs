--TODO use backpack instead of CPP
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

#else

--TODO AFAICT, there's no way to force an 'unsafePerformIO' to be run every time it's evaluated
    -- this makes sure this file is always recompiled instead, which is kinda overkill
{-# OPTIONS_GHC -fforce-recomp #-}

module Embed where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

mainCSS, elmJS, jsJS :: () -> Text
{-# NOINLINE mainCSS #-}
mainCSS () = unsafePerformIO $ readAbs $ "rsc" </> "main.css"
{-# NOINLINE elmJS #-}
elmJS () = unsafePerformIO $ readAbs $ "rsc" </> "dist" </> "elm.js"
{-# NOINLINE jsJS #-}
jsJS () = unsafePerformIO $ readAbs $ "rsc" </> "main.js"

readAbs :: FilePath -> IO Text
readAbs path = do
    interactive <- (== "<interactive>") <$> getProgName --TODO filthy...
    path' <- if interactive then return $ path else (</> path) . takeDirectory <$> getExecutablePath
    T.readFile $ path'

#endif
