module Monpad.Plugins.Logger (plugin, Settings (..)) where

import Control.Monad.Reader
import Data.Time
import Optics
import System.Console.ANSI

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Text.Pretty.Simple (OutputOptions (outputOptionsInitialIndent), defaultOutputOptionsDarkBg, pShow, pShowOpt, pPrint)

import Monpad
import Monpad.Plugins

data Settings
    = Normal
    | Quiet
    | Loud
    deriving (Show, Enum, Bounded)

plugin :: (Show a, Show b) => Logger -> Settings -> Plugin a b
plugin write0 settings = Plugin @() @() $ logUpdates settings write <> logImportantStuff write <> logPong settings write
  where
    write = write0
        { log = write0.log <=< \s -> do
            t <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
            pure $ mconcat
                [ T.pack $ setSGRCode
                    [ SetColor Background Dull Green
                    , SetColor Foreground Dull Black
                    , SetConsoleIntensity BoldIntensity
                    ]
                , T.pack t
                , T.pack $ setSGRCode [Reset]
                , " "
                , s
                ]
        }

logImportantStuff :: (Monoid e, Monoid s) => Logger -> ServerConfig e s a b
logImportantStuff write = mempty
    { onStart = \url -> write.log $ "Monpad server started at " <> url
    , onNewConnection = \_ (ClientID i) -> do
        write.log $ "New client: " <> i
        mempty
    , onDroppedConnection = \e (ClientID i) _ -> do
        pPrint e
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
        liftIO $ write.log $ T.intercalate "\n"
            [ "Message received from client: " <> TL.toStrict (pShow c)
            , pShowIndented u
            ]
    printServerUpdate c u = do
        liftIO $ write.log $ T.intercalate "\n"
            [ "Message sent to client: " <> TL.toStrict (pShow c)
            , pShowIndented u
            ]

logPong :: (Monoid e, Monoid s) => Settings -> Logger -> ServerConfig e s a b
logPong settings write = case settings of
    Normal -> mempty
    Quiet -> mempty
    Loud -> mempty
        { onPong = \t (ClientID c) _ -> do
            liftIO $ write.log $ T.intercalate "\n"
                [ "Pong: " <> c
                , pShowIndented t
                ]
            mempty
        }

pShowIndented :: Show a => a -> Text
pShowIndented = TL.toStrict . pShowOpt defaultOutputOptionsDarkBg {outputOptionsInitialIndent = 4}
