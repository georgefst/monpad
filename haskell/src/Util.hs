module Util where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Traversable
import Data.Tuple.Extra
import Optics
import System.FilePath
import Util.Util

import Control.Exception (try)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Either.Validation (validationToEither)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Dhall (FromDhall)
import Dhall qualified as D
import Dhall.Core qualified as D
import Dhall.Import qualified as D
import Dhall.Parser qualified as D
import Dhall.TypeCheck qualified as D
import Network.HostName (getHostName)
import Network.Socket (
    AddrInfo (addrAddress),
    HostAddress,
    HostName,
    SockAddr (SockAddrInet),
    getAddrInfo,
    hostAddressToTuple,
 )
import Streamly.Prelude qualified as SP
import System.IO (hPrint, stderr)

zipEndo :: Endo a -> Endo b -> Endo (a, b)
zipEndo (Endo sf1) (Endo sf2) = Endo $ sf1 *** sf2

getLocalIp :: IO (Maybe HostAddress)
getLocalIp = do
    h <- getHostName'
    sockAddrs <- map addrAddress <$> getAddrInfo Nothing (Just $ h <> ".local") Nothing
    pure . find bitOfAHack $ flip mapMaybe sockAddrs \case
        SockAddrInet _ a -> Just a
        _ -> Nothing
  where
    --TODO
    bitOfAHack = (== 192) . fst4 . hostAddressToTuple

-- adapted from an internal function of the same name in Network.Socket.Info
showHostAddress :: HostAddress -> Text
showHostAddress ip =
    let (u3, u2, u1, u0) = hostAddressToTuple ip
     in T.intercalate "." $ map showT [u3, u2, u1, u0]

--TODO if maintainer doesn't respond to my email fixing this, fork
getHostName' :: IO HostName
getHostName' = f <$> getHostName
  where
    f x = maybe x T.unpack $ T.stripSuffix ".local" $ T.pack x

-- | Attach an extra action to each element of the stream.
traceStream :: (a -> IO ()) -> SP.Serial a -> SP.Serial a
traceStream f = SP.mapM \x -> f x >> pure x

-- | Ignore events which are followed within a given number of microseconds.
lastOfGroup :: Int -> SP.Async a -> SP.Serial a
lastOfGroup interval = f2 . SP.adapt . f1
  where
    -- delay everything by `interval`, and insert 'Nothing' markers where the value first came in
    f1 = SP.concatMapWith (<>) \x ->
        SP.cons Nothing $ SP.fromEffect (threadDelay interval >> pure (Just x))
    -- ignore any event which appears within `interval` of a 'Nothing'
    f2 = SP.mapMaybe id . SP.map snd . flip SP.postscanlM' (pure (const False, error "lastOfGroup")) \(tooSoon, _) -> \case
        Just x -> do
            t <- getCurrentTime
            pure (tooSoon, guard (not $ tooSoon t) $> x)
        Nothing -> do
            t <- getCurrentTime
            pure (\t' -> diffUTCTime t' t < (realToFrac interval / 1_000_000), Nothing)

type DhallExpr = D.Expr D.Src D.Import

-- | Returns list of files (transitively) imported by the expression, grouped by directory.
dhallImports :: MonadIO io => DhallExpr -> io [(FilePath, NonEmpty Text)]
dhallImports e = do
    (_e', s) <- liftIO $ runStateT (D.loadWith e) $ D.emptyStatus ""
    pure $ classifyOnFst $ nubOrd $ mapMaybe (importFile . D.chainedImport . D.child) $ D._graph s
  where
    importFile x = case D.importType $ D.importHashed x of
        D.Local _ file -> Just (joinPath $ reverse $ map T.unpack $ D.components $ D.directory file, D.file file)
        _ -> Nothing

{-TODO can we guarantee this is totally safe? and why doesn't the library provide it?
https://github.com/dhall-lang/dhall-haskell/issues/2254
-}
-- | Resolve imports. A version of 'Dhall.load' which doesn't throw exceptions.
dhallLoadSafe :: DhallExpr -> IO (Either (D.SourcedException D.MissingImports) (D.Expr D.Src Void))
dhallLoadSafe = try @(D.SourcedException D.MissingImports) . D.load

dhallExprFromText :: Text -> MaybeT IO DhallExpr
dhallExprFromText = printError . D.exprFromText ""

-- | Get a Haskell value from a Dhall expression, and return the hash.
dhallToHs :: FromDhall a => DhallExpr -> MaybeT IO (a, Text)
dhallToHs e = do
    e0 <- printError =<< liftIO (dhallLoadSafe e)
    expectedType <- printError $ validationToEither expected
    _ <- printError . D.typeOf $ D.Annot e0 expectedType
    let e' = D.normalize e0
    x <- printError . D.toMonadic . extract $ D.renote e'
    pure (x, D.hashExpressionToCode e')
  where
    D.Decoder{extract, expected} = D.auto

printError :: Show e => Either e a -> MaybeT IO a
printError = either (\e -> liftIO (hPrint stderr e) >> mzero) pure

-- | Ensure no two members have the same value for the provided 'Text' field, by appending numbers when necessary.
uniqueNames :: Traversable t => Lens' a Text -> t a -> t a
uniqueNames l xs = flip evalState allNames $ for xs \x ->
    state (Map.insertLookupWithKey (\_ _ old -> old + 1) (view l x) err) >>= \case
        Nothing -> err
        Just 0 -> pure x
        Just n -> pure $ over l (<> " [" <> showT n <> "]") x
  where
    -- the state map stores the number of occurrences of each name seen so far
    allNames = Map.fromList $ zip (view l <$> toList xs) (repeat (0 :: Int))
    err = error "broken invariant in `uniqueNames` - all names should be in map by construction"

{- | Returns 'True' iff element is new.
'Set.size' is O(1), so this is quicker than checking membership first.
--TODO add to `containers`? https://github.com/haskell/containers/issues/31
-}
setInsert' :: Ord a => a -> Set a -> (Bool, Set a)
setInsert' x s = (Set.size s /= Set.size s', s')
  where
    s' = Set.insert x s

{- | Returns 'True' iff element was present.
'Set.size' is O(1), so this is quicker than checking membership first.
-}
setDelete' :: Ord a => a -> Set a -> (Bool, Set a)
setDelete' x s = (Set.size s /= Set.size s', s')
  where
    s' = Set.delete x s
