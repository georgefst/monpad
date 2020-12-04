{-# LANGUAGE ApplicativeDo #-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Monpad
import Options.Applicative

data Args = Args
    { httpPort :: Int
    , wsPort :: Int
    }

parser :: Parser Args
parser = do
    httpPort <- option auto $ long "http" <> metavar "PORT"
    wsPort <- option auto $ long "ws" <> metavar "PORT"
    pure Args{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    Args{..} <- execParser $ info parser mempty
    serverExtWs httpPort wsPort =<< layoutFromDhall @() @() (mapLayoutDhall <> ".void " <> defaultDhall)
