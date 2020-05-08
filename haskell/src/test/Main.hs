module Main where

import Lib

main :: IO ()
main = server defaultConfig{getArgs = getCommandLineArgs}
