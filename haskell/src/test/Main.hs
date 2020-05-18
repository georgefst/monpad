module Main where

import WebGamepad

main :: IO ()
main = server defaultConfig{getArgs = getCommandLineArgs}
