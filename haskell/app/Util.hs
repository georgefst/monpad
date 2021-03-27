{-# LANGUAGE PackageImports #-}
module Util where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Functor
import Data.List
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Dhall.Core qualified as D
import Dhall.Core qualified as Dhall
import Dhall.Import qualified as D
import Dhall.Parser qualified as Dhall
import Streamly
import Streamly.Prelude qualified as SP
import System.Directory
import System.FilePath
import System.Info.Extra
import "georgefst-utils" Util

-- | Attach an extra action to each element of the stream.
traceStream :: (a -> IO ()) -> Serial a -> Serial a
traceStream f = SP.mapM \x -> f x >> pure x

-- | Ignore events which are followed within a given number of milliseconds.
lastOfGroup :: Int -> Async a -> Serial a
lastOfGroup interval = f2 . asyncly . f1
  where
    -- delay everything by `interval`, and insert 'Nothing' markers where the value first came in
    f1 = SP.concatMapWith (<>) \x ->
        SP.yieldM (pure Nothing) <> SP.yieldM (threadDelay interval >> pure (Just x))
    -- ignore any event which appears within `interval` of a 'Nothing'
    f2 = SP.mapMaybe id . SP.map snd . flip SP.postscanlM' (const False, undefined) \(tooSoon, _) -> \case
        Just x -> do
            t <- getCurrentTime
            pure (tooSoon, guard (not $ tooSoon t) $> x)
        Nothing -> do
            t <- getCurrentTime
            pure (\t' -> diffUTCTime t' t < realToFrac interval, Nothing)

--TODO this is a pretty egregious workaround for Dhall's inability to parse paths beginning with C:\
-- | Make an absolute path relative, at all costs.
windowsHack :: Text -> IO Text
windowsHack e = if isWindows
    then case e' of
        '"' : c : _ | c /= '.' -> case reads e' of -- path is not relative - remove quotes around drive
            (y, ys) : _ -> windowsHack . T.pack $ y <> ys
            _ -> error "malformed input expression - check quoting"
        c : ':' : '/' : xs | isUpper c -> fmap (T.pack . ("./" ++)) . makeRelativeToCurrentDirectory' $ xs
        _ -> pure $ T.pack e'
    else pure e
  where
    e' = T.unpack e
    -- ventures where 'makeRelative' fears to tread - namely, introduces ".." (in fact it's very keen to do so...)
    -- also uses forward slash regardless of OS
    makeRelativeToCurrentDirectory' p = getCurrentDirectory <&> \curr ->
        intercalate "/" $ replicate (length $ splitPath curr) ".." <> pure p

-- | Returns list of files (transitively) imported by the expression, grouped by directory.
dhallImports :: MonadIO io => Text -> io [(FilePath, NonEmpty Text)]
dhallImports t = do
    e <- Dhall.throws $ Dhall.exprFromText "" t
    --TODO we could return and reuse the ignored expression, rather than re-running the previous step in the outer scope
    (_e', s) <- liftIO $ flip runStateT (D.emptyStatus $ dropFileName "") $ D.loadWith e
    pure $ classifyOnFst $ nubOrd $ mapMaybe (importFile . D.chainedImport . D.child) $ D._graph s
  where
    importFile x = case D.importType $ D.importHashed x of
        D.Local _ file -> Just (joinPath $ reverse $ map T.unpack $ D.components $ D.directory file, D.file file)
        _ -> Nothing
