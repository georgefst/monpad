{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
module Monpad.Plugins.Logger (plugin, Settings (..)) where

import Control.Monad.Reader
import Optics
import Text.Pretty.Simple

import qualified Data.Text.Lazy as TL

import Monpad
import Monpad.Plugins

data Settings
    = Normal
    | Quiet
    | Loud
    deriving (Show, Enum, Bounded)

plugin :: (Show a, Show b) => Logger -> Settings -> Plugin a b
plugin write settings = Plugin @() @() $ logUpdates settings write <> logImportantStuff write <> logPong settings write

logImportantStuff :: (Monoid e, Monoid s) => Logger -> ServerConfig e s a b
logImportantStuff write = mempty
    { onStart = \url -> write.log $ "Monpad server started at " <> url
    , onNewConnection = \_ (ClientID i) -> do
        write.log $ "New client: " <> i
        mempty
    , onDroppedConnection = \_ (ClientID i) _ ->
        write.log $ "Client disconnected: " <> i
    }

logUpdates :: (Monoid e, Monoid s, Show a, Show b) => Settings -> Logger -> ServerConfig e s a b
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
        liftIO $ write.log $ "Message received from client: " <> c
        pLogIndented write u
    printServerUpdate c u = do
        liftIO $ write.log $ "Message sent to client: " <> c
        pLogIndented write u

logPong :: (Monoid e, Monoid s) => Settings -> Logger -> ServerConfig e s a b
logPong settings write = case settings of
    Normal -> mempty
    Quiet -> mempty
    Loud -> mempty
        { onPong = \t (ClientID c) _ -> do
            liftIO $ write.log $ "Pong: " <> c
            pLogIndented write t
            mempty
        }

pLogIndented :: (Show x, MonadIO m) => Logger -> x -> m ()
pLogIndented write = liftIO . write.log . TL.toStrict
    . pShowOpt defaultOutputOptionsDarkBg {outputOptionsInitialIndent = 4}
