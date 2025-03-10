module Monpad.Plugins.WatchLayout (plugin) where

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.Function
import Streamly.FSNotify
import System.FilePath
import System.Info

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Dhall (FromDhall)
import Dhall.Core qualified as D
import Dhall.Src qualified as D
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as Stream
import Util.Util ((<<$>>))

import Monpad
import Monpad.Plugins
import Util

plugin :: (FromDhall a, FromDhall b) => Logger -> Plugin a b
plugin = Plugin . sendLayout @() @()

sendLayout :: (Monoid e, Monoid s, FromDhall a, FromDhall b) => Logger -> ServerConfig e s a b
sendLayout write = mempty
    { updates = \env -> S.fromList (toList env.initialLayouts) & S.parConcatMap id \(layout, expr) ->
        Stream.withInit do
            imports <- second toList <<$>> dhallImports expr
            for_ imports \(dir, files) ->
                write.log $ "Watching: " <> T.pack dir <> " (" <> T.intercalate ", " files <> ")"
            pure imports
        $ traceStream (const $ write.log "Sending new layout to client")
            . fmap (send layout.name)
            . S.mapMaybeM (\() -> getLayout write expr)
            . S.parConcat id . S.fromList
            . map \(dir, files) -> case os of
                "linux" -> watchDir dir & S.mapMaybe \case
                    CloseWrite p _ _ -> guard $ T.pack (takeFileName p) `elem` files
                    _ -> Nothing
                -- if we're on an OS without special `CloseWrite` events,
                -- we make do with `Modified`, and debounce those which occur within 0.1s of each other
                _ -> fmap NE.head $ Stream.groupByTime 0.1 $ watchDir dir & S.mapMaybe \case
                    Modified p _ _ -> guard $ T.pack (takeFileName p) `elem` files
                    _ -> Nothing
    }

getLayout :: (FromDhall a, FromDhall b) => Logger -> D.Expr D.Src D.Import -> IO (Maybe (Layout a b))
getLayout write e = runMaybeT do
    (l, h) <- dhallToHs write e
    liftIO $ write.log $ "Parsed Dhall expression: " <> h
    pure l

send :: LayoutID -> Layout a b -> [ServerUpdate a b]
send name layout = [SwitchLayout name, SetLayout layout]
