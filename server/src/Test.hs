module Main where

import Text.Pretty.Simple

import Server

main :: IO ()
main = server ServerConfig { onMessage = pPrint, port = 8001 }
