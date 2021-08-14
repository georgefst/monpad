module Monpad.Plugins.Logger (plugin, Settings (..)) where

import Control.Monad.Reader
import Optics
import Text.Pretty.Simple

import Data.Text (Text)
import qualified Data.Text.Lazy as TL

import Monpad
import Monpad.Plugins

data Settings
    = Normal
    | Quiet
    | Loud
    deriving (Show, Enum, Bounded)

plugin :: (Show a, Show b) => (Text -> IO ()) -> Settings -> Plugin a b
plugin f settings = Plugin @() @() $ logUpdates settings f <> logImportantStuff f <> logPong settings f

logImportantStuff :: (Monoid e, Monoid s) => (Text -> IO ()) -> ServerConfig e s a b
logImportantStuff f = mempty
    { onStart = \url -> f $ "Monpad server started at " <> url
    , onNewConnection = \_ (ClientID i) -> do
        f $ "New client: " <> i
        mempty
    , onDroppedConnection = \_ (ClientID i) _ ->
        f $ "Client disconnected: " <> i
    }

logUpdates :: (Monoid e, Monoid s, Show a, Show b) => Settings -> (Text -> IO ()) -> ServerConfig e s a b
logUpdates settings f = mempty
    { onUpdate = \u -> do
        ClientID c <- asks $ view #client
        let (pc, ps) = case settings of
                Normal -> (printClientUpdate c, mempty)
                Quiet -> mempty
                Loud -> (printClientUpdate c, printServerUpdate c)
        case u of
            ClientUpdate cu -> pc cu
            ServerUpdate su -> ps su
        mempty
    }
  where
    printClientUpdate c u = do
        liftIO $ f $ "Message received from client: " <> c
        pLogIndented f u
    printServerUpdate c u = do
        liftIO $ f $ "Message sent to client: " <> c
        pLogIndented f u

logPong :: Settings -> (Text -> IO ()) -> ServerConfig () () a b
logPong settings f = case settings of
    Normal -> mempty
    Quiet -> mempty
    Loud -> mempty
        { onPong = \t (ClientID c) () -> do
            liftIO $ f $ "Pong: " <> c
            pLogIndented f t
            mempty
        }

pLogIndented :: (Show x, MonadIO m) => (Text -> IO ()) -> x -> m ()
pLogIndented f = liftIO . f . TL.toStrict . pShowOpt defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}
