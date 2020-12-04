module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 qualified as BSL
import GHC.Generics
import Layout
import Monpad

{- TODO
use a more compact encoding - https://hackage.haskell.org/package/aeson-1.5.4.1/docs/Data-Aeson.html#t:SumEncoding
also, avoid decoding JSON from the frontend, only to re-encode it here
ideally don't use JSON at all - protobuf?
-}

main :: IO ()
main = do
    layout <- layoutFromDhall @() @() defaultDhall
    server 8000 $ conf layout --TODO take port as CLI arg

conf :: Layout () () -> ServerConfig () () () ()
conf layout =
    ServerConfig
        { onStart = mempty
        , onNewConnection = \c -> do
            jsonOut $ Joined c
            pure (layout, (), ())
        , onMessage = jsonOut . Update
        , onAxis = mempty
        , onButton = mempty
        , onDroppedConnection = const . jsonOut . Dropped
        }

data Event
    = Update Update
    | Joined ClientID
    | Dropped ClientID
    deriving (Generic, ToJSON)

jsonOut :: MonadIO m => Event -> m ()
jsonOut = liftIO . BSL.putStrLn . toLazyByteString . fromEncoding . toEncoding
