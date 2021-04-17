module Monpad.Plugins.WatchLayout (plugin) where

import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Streamly.FSNotify
import System.FilePath

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dhall (FromDhall)
import Dhall.Core qualified as D
import Dhall.Src qualified as D
import Optics (view)
import Streamly (serially)
import Streamly.Prelude qualified as SP

import Monpad
import Monpad.Plugins
import Util

plugin :: (FromDhall a, FromDhall b) => Plugin a b
plugin = Plugin $ sendLayout @() @()

{-TODO
find a way to remove 'lastOfGroup' workaround
the issue (seemingly on all three platforms) is that we get too many events, when all we care about is CLOSE_WRITE
    but because 'fsnotify' is cross-platform, there may be no good way to filter
-}
sendLayout :: (Monoid e, Monoid s, FromDhall a, FromDhall b) => ServerConfig e s a b
sendLayout = mempty
    { updates = \env -> do
        let exprs = mapMaybe (sequence . first (view #name)) . toList $ view #initialLayouts env
        flip foldMap exprs \(name, expr) -> do
            imports <- dhallImports expr
            evss <- for imports \(dir, toList -> files) -> liftIO do
                let isImport = EventPredicate $ (`elem` files) . T.pack . takeFileName . eventPath
                T.putStrLn $ "Watching: " <> T.pack dir <> " (" <> T.intercalate ", " files <> ")"
                snd <$> watchDirectory dir (isModification `conj` isImport)
            flip foldMap evss $ serially
                . traceStream (const $ T.putStrLn "Sending new layout to client")
                . SP.map (const . send name)
                . SP.mapMaybeM (const $ getLayout expr)
                . lastOfGroup 100_000
    }

getLayout :: (FromDhall a, FromDhall b) => D.Expr D.Src D.Import -> IO (Maybe (Layout a b))
getLayout e = runMaybeT do
    (l, h) <- dhallToHs e
    liftIO $ T.putStrLn $ "Parsed Dhall expression: " <> h
    pure l

send :: LayoutID -> Layout a b -> [ServerUpdate a b]
send name layout = [SwitchLayout name, SetLayout layout]
