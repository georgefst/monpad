module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 qualified as BSL
import GHC.Generics
import Layout
import Monpad

main :: IO ()
main = do
    layout <- layoutFromDhall @() @() $ voidLayout <> defaultDhall
    server 8000 $ conf layout --TODO take port as CLI arg
  where
    --TODO copied from 'Monpad.test' - put this in a file
    voidLayout =
        "let E = ./../dhall/evdev.dhall \
        \let A = E.AbsAxis \
        \let B = E.Key \
        \in (./../dhall/monpad.dhall A B).mapLayout {} {} (λ(_ : A) → {=}) (λ(_ : B) → {=}) "

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
