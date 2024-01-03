module Util where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.List.Extra
import Data.Maybe
import Data.Monoid
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
import Data.Stream.Infinite (Stream)
import Data.Stream.Infinite qualified as Stream
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
import Streamly.Data.Stream.Prelude qualified as S
import System.Directory (getHomeDirectory)

data Logger = Logger
    { log :: Text -> IO ()
    , logError :: Text -> IO ()
    , ansi :: Bool
    -- ^ Can we use ANSI escape codes?
    }
-- TODO is this derivable? it would be if I replaced `Bool` with `All`, but that would need wrapping at every use site
instance Semigroup Logger where
    a <> b = Logger
        { log = a.log <> b.log
        , logError = a.logError <> b.logError
        , ansi = a.ansi && b.ansi
        }
instance Monoid Logger where
    mempty = Logger mempty mempty True

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
traceStream :: Monad m => (a -> m ()) -> S.Stream m a -> S.Stream m a
traceStream f = S.mapM \x -> f x >> pure x

type DhallExpr = D.Expr D.Src D.Import

-- | Returns list of files (transitively) imported by the expression, grouped by directory.
dhallImports :: MonadIO io => DhallExpr -> io [(FilePath, NonEmpty Text)]
dhallImports e = do
    (_e', s) <- liftIO $ runStateT (D.loadWith e) $ D.emptyStatus ""
    home <- liftIO getHomeDirectory
    pure $ classifyOnFst $ nubOrd $ mapMaybe (importFile home . D.chainedImport . D.child) $ D._graph s
  where
    importFile home x = case D.importType $ D.importHashed x of
        D.Local prefix file -> Just
            ( filePrefix home prefix </> joinPath (reverse $ map T.unpack $ D.components $ D.directory file)
            , D.file file
            )
        _ -> Nothing
    filePrefix home = \case
        D.Absolute -> "/"
        D.Here -> ""
        D.Parent -> ".."
        D.Home -> home

{-TODO can we guarantee this is totally safe? and why doesn't the library provide it?
https://github.com/dhall-lang/dhall-haskell/issues/2254
-}
-- | Resolve imports. A version of 'Dhall.load' which doesn't throw exceptions.
dhallLoadSafe :: DhallExpr -> IO (Either (D.SourcedException D.MissingImports) (D.Expr D.Src Void))
dhallLoadSafe = try @(D.SourcedException D.MissingImports) . D.load

dhallExprFromText :: Logger -> Text -> MaybeT IO DhallExpr
dhallExprFromText write = writeError write . D.exprFromText ""

-- | Get a Haskell value from a Dhall expression, and return the hash.
dhallToHs :: FromDhall a => Logger -> DhallExpr -> MaybeT IO (a, Text)
dhallToHs write e = do
    e0 <- writeError write =<< liftIO (dhallLoadSafe e)
    expectedType <- writeError write $ validationToEither expected
    _ <- writeError write . D.typeOf $ D.Annot e0 expectedType
    let e' = D.normalize e0
    x <- writeError write . D.toMonadic . extract $ D.renote e'
    pure (x, D.hashExpressionToCode e')
  where
    D.Decoder{extract, expected} = D.auto

writeError :: Show e => Logger -> Either e a -> MaybeT IO a
writeError write = either (\e -> liftIO (write.logError $ showT e) >> mzero) pure

-- | Ensure no two members have the same value for the provided 'Text' field, by appending numbers when necessary.
uniqueNames :: Traversable t => Lens' a Text -> t a -> t a
uniqueNames l xs = flip evalState allNames $ for xs \x ->
    state (Map.insertLookupWithKey (\_ _ old -> old + 1) (view l x) err) >>= \case
        Nothing -> err
        Just 0 -> pure x
        Just n -> pure $ over l (<> " [" <> showT n <> "]") x
  where
    -- the state map stores the number of occurrences of each name seen so far
    allNames = Map.fromList $ map (,0 :: Int) (view l <$> toList xs)
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

--TODO for some reason, there is no Stream.head: https://github.com/ekmett/streams/pull/19
streamHead :: Stream a -> a
streamHead = (Stream.!! 0)

pairAdjacent :: [a] -> [(a, a)]
pairAdjacent xs = zip xs $ tail xs
