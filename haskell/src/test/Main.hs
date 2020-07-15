module Main where

import Data.Generics.Labels ()
import Data.Text (Text)
import Lens.Micro (over)

import WebGamepad

main :: IO ()
main = server defaultConfig {getArgs = getCommandLineArgs def}
  where
    --TODO this is a workaround until we have something like https://github.com/dhall-lang/dhall-haskell/issues/1521
    def = over #dhallLayout voidLayout defaultArgs
    voidLayout = ("let E = ./../dhall/evdev.dhall in (./../dhall/WG.dhall E.AbsAxis E.Key).voidLayout " <>)
