module Monpad.Plugins.Logger (plugin) where

import Control.Monad.Reader
import Data.Tuple.Extra
import Text.Pretty.Simple
import Util.Util

import Data.Text (Text)

import Monpad
import Monpad.Plugins

plugin :: (Text -> IO ()) -> Bool -> Plugin a b
plugin f quiet = Plugin @() @() . applyWhen (not quiet) (logUpdates f <>) $ logImportantStuff f

logImportantStuff :: (Monoid e, Monoid s) => (Text -> IO ()) -> ServerConfig e s a b
logImportantStuff f = const mempty
    { onStart = \url -> f $ "Monpad server started at " <> url
    , onNewConnection = \(ClientID i) -> do
        f $ "New client: " <> i
        mempty
    , onDroppedConnection = const do
        ClientID i <- asks thd3
        liftIO $ f $ "Client disconnected: " <> i
        mempty
    }

logUpdates :: (Monoid e, Monoid s) => (Text -> IO ()) -> ServerConfig e s a b
logUpdates f = const mempty
    { onMessage = \m -> do
        ClientID c <- asks thd3
        liftIO $ f $ "Message received from client: " <> c
        pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} m
        mempty
    }
