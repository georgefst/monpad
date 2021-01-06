{-# LANGUAGE ApplicativeDo #-}

module Main where

import Data.Text (Text)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Monpad
import Options.Applicative

data Args = Args
    { httpPort :: Int
    , wsPort :: Int
    , imageDir :: Maybe FilePath
    , layoutDhall :: Maybe Text
    }

parser :: Parser Args
parser = do
    httpPort <- option auto $ long "http" <> metavar "PORT"
    wsPort <- option auto $ long "ws" <> metavar "PORT"
    imageDir <- optional . strOption $ long "images" <> metavar "DIR"
    layoutDhall <- optional . strOption $ long "layout" <> metavar "EXPR"
    pure Args{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    Args{..} <- execParser $ info parser mempty
    serverExtWs httpPort wsPort imageDir =<< maybe defaultSimple layoutFromDhall layoutDhall
