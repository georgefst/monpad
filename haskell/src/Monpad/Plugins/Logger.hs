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
plugin write settings = Plugin @() @() $ logUpdates settings write <> logImportantStuff write <> logPong settings write

logImportantStuff :: (Monoid e, Monoid s) => (Text -> IO ()) -> ServerConfig e s a b
logImportantStuff write = mempty
    { onStart = \url -> write $ "Monpad server started at " <> url
    , onNewConnection = \_ (ClientID i) -> do
        write $ "New client: " <> i
        mempty
    , onDroppedConnection = \_ (ClientID i) _ ->
        write $ "Client disconnected: " <> i
    }

logUpdates :: (Monoid e, Monoid s, Show a, Show b) => Settings -> (Text -> IO ()) -> ServerConfig e s a b
logUpdates settings write = mempty
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
        liftIO $ write $ "Message received from client: " <> c
        pLogIndented write u
    printServerUpdate c u = do
        liftIO $ write $ "Message sent to client: " <> c
        pLogIndented write u

logPong :: (Monoid e, Monoid s) => Settings -> (Text -> IO ()) -> ServerConfig e s a b
logPong settings write = case settings of
    Normal -> mempty
    Quiet -> mempty
    Loud -> mempty
        { onPong = \t (ClientID c) _ -> do
            liftIO $ write $ "Pong: " <> c
            pLogIndented write t
            mempty
        }

pLogIndented :: (Show x, MonadIO m) => (Text -> IO ()) -> x -> m ()
pLogIndented write = liftIO . write . TL.toStrict . pShowOpt defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4}
