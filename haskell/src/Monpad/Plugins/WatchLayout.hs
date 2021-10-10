module Monpad.Plugins.WatchLayout (plugin) where

import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Streamly.FSNotify
import System.FilePath

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Text (Text)
import Data.Text qualified as T
import Dhall (FromDhall)
import Dhall.Core qualified as D
import Dhall.Src qualified as D
import Optics (view)
import Streamly.Prelude qualified as SP
import System.FSNotify (Debounce (Debounce), WatchConfig (confDebounce), defaultConfig)

import Monpad
import Monpad.Plugins
import Util

plugin :: (FromDhall a, FromDhall b) => (Text -> IO ()) -> Plugin a b
plugin = Plugin . sendLayout @() @()

sendLayout :: (Monoid e, Monoid s, FromDhall a, FromDhall b) => (Text -> IO ()) -> ServerConfig e s a b
sendLayout write = mempty
    { updates = \env -> SP.fromAsync do
        let exprs = mapMaybe (sequence . first (view #name)) . toList $ view #initialLayouts env
        flip foldMap exprs \(name, expr) -> do
            imports <- dhallImports expr
            evss <- for imports \(dir, toList -> files) -> liftIO do
                let isImport = EventPredicate $ (`elem` files) . T.pack . takeFileName . eventPath
                write $ "Watching: " <> T.pack dir <> " (" <> T.intercalate ", " files <> ")"
                snd <$> watchDirectoryWith conf dir (isModification `conj` isImport)
            flip foldMap evss $ SP.fromSerial
                . traceStream (const $ write "Sending new layout to client")
                . SP.map (send name)
                . SP.mapMaybeM (const $ getLayout write expr)
    }
  where
    conf = defaultConfig{confDebounce = Debounce 0.1}

getLayout :: (FromDhall a, FromDhall b) => (Text -> IO ()) -> D.Expr D.Src D.Import -> IO (Maybe (Layout a b))
getLayout write e = runMaybeT do
    (l, h) <- dhallToHs write e
    liftIO $ write $ "Parsed Dhall expression: " <> h
    pure l

send :: LayoutID -> Layout a b -> [ServerUpdate a b]
send name layout = [SwitchLayout name, SetLayout layout]
