module Main where

import Text.Pretty.Simple

import Server

main :: IO ()
main = server ServerConfig
    { onMessage = \m () -> pPrint m
    , onStart = \_ -> return ()
    , port = 8001
    }
