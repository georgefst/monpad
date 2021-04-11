module Monpad.Plugins.Logger (plugin) where

import Control.Monad.Reader
import Data.Tuple.Extra
import Text.Pretty.Simple
import Util.Util

import Data.Text (Text)

import Monpad
import Monpad.Plugins

plugin :: (Show a, Show b) => (Text -> IO ()) -> Bool -> Plugin a b
plugin f quiet = Plugin @() @() . applyWhen (not quiet) (logUpdates f <>) $ logImportantStuff f

logImportantStuff :: (Monoid e, Monoid s) => (Text -> IO ()) -> ServerConfig e s a b
logImportantStuff f = mempty
    { onStart = \url -> f $ "Monpad server started at " <> url
    , onNewConnection = \_ (ClientID i) -> do
        f $ "New client: " <> i
        mempty
    , onDroppedConnection = \(ClientID i) _ _ ->
        f $ "Client disconnected: " <> i
    }

logUpdates :: (Monoid e, Monoid s, Show a, Show b) => (Text -> IO ()) -> ServerConfig e s a b
logUpdates f = mempty
    { onUpdate = \m -> do
        ClientID c <- asks thd3
        liftIO $ f $ "Message received from client: " <> c
        pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} m
        mempty
    }
