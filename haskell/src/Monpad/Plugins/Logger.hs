module Monpad.Plugins.Logger (plugin, Settings (..)) where

import Control.Monad.Reader
import Optics
import Text.Pretty.Simple

import Data.Text (Text)

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
    , onDroppedConnection = \(ClientID i) _ _ ->
        f $ "Client disconnected: " <> i
    }

logUpdates :: (Monoid e, Monoid s, Show a, Show b) => Settings -> (Text -> IO ()) -> ServerConfig e s a b
logUpdates settings f = mempty
    { onUpdate = \u -> do
        ClientID c <- asks $ view #client
        let (pc, ps) = case settings of
                Normal -> (printClientUpdate c, mempty)
                Quiet -> (mempty, mempty)
                Loud -> (printClientUpdate c, printServerUpdate c)
        case u of
            ClientUpdate cu -> pc cu
            ServerUpdate su -> ps su
        mempty
    }
  where
    printClientUpdate c u = do
        liftIO $ f $ "Message received from client: " <> c
        pPrintIndented u
    printServerUpdate c u = do
        liftIO $ f $ "Message sent to client: " <> c
        pPrintIndented u

logPong :: Settings -> (Text -> IO ()) -> ServerConfig () () a b
logPong settings f = case settings of
    Normal -> mempty
    Quiet -> mempty
    Loud -> mempty
        { onPong = \() t -> do
            liftIO $ f "Pong: "
            pPrintIndented t
            mempty
        }

pPrintIndented :: (Show x, MonadIO m) => x -> m ()
pPrintIndented = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}
