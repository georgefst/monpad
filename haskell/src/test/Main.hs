module Main where

import WebGamepad

main :: IO ()
main = do
    args <- getCommandLineArgs defaultArgs
    server defaultConfig {args}
