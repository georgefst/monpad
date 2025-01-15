{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import Frontend
import GHC.Wasm.Prim
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run app

#else

module Main (main) where

import Frontend
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Javascript.JSaddle.Warp
import Network.Wai.Application.Static

main :: IO ()
main = debugOr
    (Just $ encodeUtf8 "<title>Monpad</title>\n<link rel='stylesheet' href='common.css'/>")
    8000
    app
    (staticApp (defaultWebAppSettings "../frontend-hs"))

#endif
