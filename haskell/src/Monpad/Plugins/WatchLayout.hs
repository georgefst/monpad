module Monpad.Plugins.WatchLayout (plugin) where

import Data.Foldable
import Data.Traversable
import Streamly.FSNotify
import System.FilePath

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Text qualified as T
import Dhall (FromDhall)
import Dhall.Core qualified as D
import Dhall.Src qualified as D
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly qualified as Stream

import Monpad
import Monpad.Plugins
import Util

plugin :: (FromDhall a, FromDhall b) => Logger -> Plugin a b
plugin = Plugin . sendLayout @() @()

sendLayout :: (Monoid e, Monoid s, FromDhall a, FromDhall b) => Logger -> ServerConfig e s a b
sendLayout write = mempty
    { updates = \env -> flip (S.parConcatMap id) (S.fromList $ toList env.initialLayouts)
        \(layout, expr) -> Stream.withInit do
                imports <- dhallImports expr
                S.parConcat id . S.fromList <$> for imports \(dir, toList -> files) -> liftIO do
                    write.log $ "Watching: " <> T.pack dir <> " (" <> T.intercalate ", " files <> ")"
                    let isImport = \case
                            Modified p _ _ -> T.pack (takeFileName p) `elem` files
                            _ -> False
                    pure $ Stream.groupByTime 0.1 $ S.filter isImport $ watchDir dir
            $ traceStream (const $ write.log "Sending new layout to client")
                . fmap (send layout.name)
                . S.mapMaybeM (const $ getLayout write expr)
    }

getLayout :: (FromDhall a, FromDhall b) => Logger -> D.Expr D.Src D.Import -> IO (Maybe (Layout a b))
getLayout write e = runMaybeT do
    (l, h) <- dhallToHs write e
    liftIO $ write.log $ "Parsed Dhall expression: " <> h
    pure l

send :: LayoutID -> Layout a b -> [ServerUpdate a b]
send name layout = [SwitchLayout name, SetLayout layout]
