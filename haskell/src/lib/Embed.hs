--TODO use backpack instead of CPP
{-# LANGUAGE CPP #-}

#if defined(FULL)

{-# LANGUAGE TemplateHaskell #-}
module Embed where

import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath

--TODO how to convince cabal to look for changes to these when deciding whether to rebuild?
    -- addDependentFile?
    -- some cabal field?
    -- should file-embed "explicitly reports its files to GHC as dependent files"?
        -- https://github.com/commercialhaskell/stack/issues/4360
    -- if we work this out, we can remove 'clean' from makefile
mainCSS, elmJS, jsJS :: Text
mainCSS = decodeUtf8 $(embedFile $ ".." </> "src-other" </> "main.css")
elmJS = decodeUtf8 $(embedFile $ ".." </> "elm" </> "build" </> "elm.js")
jsJS = decodeUtf8 $(embedFile $ ".." </> "src-other" </> "main.js")

#else

module Embed where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

mainCSS, elmJS, jsJS :: Text
{-# NOINLINE mainCSS #-}
mainCSS = unsafePerformIO $ readAbs (".." </> "src-other", "main.css")
{-# NOINLINE elmJS #-}
elmJS = unsafePerformIO $ readAbs (".." </> "elm" </> "build", "elm.js")
{-# NOINLINE jsJS #-}
jsJS = unsafePerformIO $ readAbs (".." </> "src-other", "main.js")

readAbs :: (FilePath,FilePath) -> IO Text
readAbs (path,file) = do
    interactive <- (== "<interactive>") <$> getProgName --TODO filthy...
    path' <- if interactive then return path else takeDirectory <$> getExecutablePath
    T.readFile $ path' </> file

#endif
