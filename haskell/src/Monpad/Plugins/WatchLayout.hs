module Monpad.Plugins.WatchLayout (plugin) where

import Control.Monad.Extra
import Data.Foldable
import Data.Traversable
import Streamly.FSNotify
import System.FilePath

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dhall (FromDhall, auto, extract, toMonadic)
import Dhall.Core qualified as Dhall
import Dhall.Import qualified as Dhall
import Dhall.Parser qualified as Dhall
import Streamly (serially)
import Streamly.Prelude qualified as SP

import Monpad
import Monpad.Plugins
import Util

plugin :: (FromDhall a, FromDhall b) => Text -> Plugin a b
plugin = Plugin . sendLayout @() @()

{-TODO
find a way to remove 'lastOfGroup' workaround
the issue (seemingly on all three platforms) is that we get too many events, when all we care about is CLOSE_WRITE
    but because 'fsnotify' is cross-platform, there may be no good way to filter
-}
sendLayout :: (Monoid e, Monoid s, FromDhall a, FromDhall b) => Text -> ServerConfig e s a b
sendLayout exprText = mempty
    { updates = const do
        imports <- dhallImports exprText
        evss <- for imports \(dir, toList -> files) -> liftIO do
            let isImport = EventPredicate $ (`elem` files) . T.pack . takeFileName . eventPath
            T.putStrLn $ "Watching: " <> T.pack dir <> " (" <> T.intercalate ", " files <> ")"
            snd <$> watchDirectory dir (isModification `conj` isImport)
        flip foldMap evss $ serially
            . traceStream (const $ T.putStrLn "Sending new layout to client")
            . SP.map (const . pure . SetLayout)
            . SP.mapMaybeM (const $ parseLayout exprText)
            . lastOfGroup 100_000
    }

parseLayout :: (FromDhall a, FromDhall b) => Text -> IO (Maybe (Layout a b))
parseLayout t = runMaybeT do
    x <- Dhall.normalize <$> (printError =<< liftIO . dhallLoadSafe =<< printError (Dhall.exprFromText "" t))
    liftIO $ T.putStrLn $ "Parsed Dhall expression: " <> Dhall.hashExpressionToCode (Dhall.normalize x)
    printError . toMonadic $ extract auto x
  where
    printError :: Show e => Either e a -> MaybeT IO a
    printError = either (\e -> liftIO (print e) >> mzero) pure
