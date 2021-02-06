{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Void (Void)
import Dhall (FromDhall)
import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Monpad
import qualified OS
import Options.Applicative
import Streamly (Async, Serial, asyncly, serially)
import Streamly.FSNotify
import qualified Streamly.Prelude as SP
import System.Directory
import System.Exit
import System.FilePath
import Text.Pretty.Simple

data Args = Args
    { quiet :: Bool
    , systemDevice :: Bool
    , watchLayout :: Maybe Float --TODO just fix the interval length once we've found one that's fine across platforms
    , port :: Int --TODO 'Port'
    , imageDir :: Maybe FilePath
    , dhallLayout :: Text
    }

parser :: Parser Args
parser = do
    quiet <- switch $ short 'q' <> long "quiet"
    systemDevice <- switch $ long "system-device"
    watchLayout <- optional . option auto $ mconcat
        [ long "watch-layout"
        , metavar "SECONDS"
        ]
    port <- option auto $ mconcat
        [ long "port"
        , short 'p'
        , metavar "INT"
        , value 8000
        , showDefault
        , help "Port number for the server to listen on"
        ]
    imageDir <- optional . strOption $ mconcat
        [ long "images"
        , short 'i'
        , metavar "DIR"
        , help "Directory from which to serve image files"
        ]
    dhallLayout <- strOption $ mconcat
        [ long "layout-dhall"
        , short 'l'
        , metavar "EXPR"
        , value $ defaultDhall ()
        , help "Dhall expression to control layout of buttons etc."
        ]
    pure Args{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    Args{..} <- execParser $ info (helper <*> parser) (fullDesc <> header "monpad")
    layoutFile <- canonicalizePath $ T.unpack dhallLayout
    let watchPred = isModification `conj` EventPredicate ((== layoutFile) . eventPath)
    evs <- case watchLayout of
        Just interval -> do
            unlessM (doesFileExist layoutFile) do
                T.putStrLn "Dhall expression provided is not a file, so can't be watched"
                exitFailure
            (_, es) <- liftIO $ watchDirectory (takeDirectory layoutFile) watchPred
            T.putStrLn $ "Watching: " <> T.pack layoutFile
            pure $ lastOfGroup interval es
        Nothing -> mempty
    let run :: forall e s a b. (Monoid e, Monoid s, FromDhall a, FromDhall b) =>
            ServerConfig e s a b -> Layout a b -> IO ()
        run sc l = server port imageDir l $ scPrintStuff quiet <> scSendLayout <> sc
          where
            scSendLayout = mempty
                { updates = serially $
                    traceStream (const $ T.putStrLn "Sending new layout to client") $
                    SP.map (const . const . SetLayout) $
                    SP.mapMaybeM (const $ mkUpdate layoutFile) evs
                }
    if systemDevice
        then join (run . OS.conf) =<< layoutFromDhall dhallLayout
        else run @() @() @Unit @Unit mempty =<< layoutFromDhall dhallLayout
    mempty --TODO bizarrely, this saves us from a type error caused by ApplicativeDo

scPrintStuff :: (Monoid e, Monoid s) => Bool -> ServerConfig e s a b
scPrintStuff quiet = mempty
    { onStart = T.putStrLn "Monpad server started"
    , onNewConnection = \(ClientID i) -> do
        T.putStrLn $ "New client: " <> i
        mempty
    , onMessage = \m -> do
        ClientID c <- asks snd
        unless quiet do
            liftIO $ T.putStrLn $ "Message received from client: " <> c
            pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} m
    , onDroppedConnection = \_ -> do
        ClientID i <- asks snd
        liftIO $ T.putStrLn $ "Client disconnected: " <> i
    }

mkUpdate :: (FromDhall a, FromDhall b) => FilePath -> IO (Maybe (Layout a b))
mkUpdate file = printDhallErrors $ layoutFromDhall =<< dhallResolve file
  where
    {-TODO this may well be incomplete
        anyway, if there isn't a better way of doing this, report to 'dhall-haskell'
    -}
    printDhallErrors = fmap (join . join)
        . h @Dhall.ParseError
        . h @(Dhall.SourcedException Dhall.MissingImports)
        . h @(Dhall.TypeError Dhall.Src Void)
      where
        h :: forall e a. Exception e => IO a -> IO (Maybe a)
        h = handle @e (\x -> print x >> pure Nothing) . fmap Just
    {-TODO using 'pretty' means we're repeating work
        perhaps 'layoutFromDhall' should take an 'Expr Src/Void Void'
        (and be total, while we're at it?)
        also 'normalize' and 'typeOf'
    -}
    dhallResolve p = do
        x <- Dhall.loadRelativeTo (takeDirectory p) Dhall.UseSemanticCache
            =<< Dhall.throws . Dhall.exprFromText p
            =<< T.readFile p
        _ <- Dhall.throws $ Dhall.typeOf x
        T.putStrLn $ "Parsed Dhall expression: " <> Dhall.hashExpressionToCode (Dhall.normalize x)
        pure $ Dhall.pretty x

--TODO better name
-- | Attach an extra action to each element of the stream.
traceStream :: (a -> IO ()) -> Serial a -> Serial a
traceStream f = SP.mapM \x -> f x >> pure x

{-TODO
the issue (seemingly on all three platforms) is that we get too many events, when all we care about is CLOSE_WRITE
    but because 'fsnotify' is cross-platform, there may be no good way to filter
-}
-- | Ignore events which are followed within `interval` seconds.
lastOfGroup :: Float -> Async a -> Serial a
lastOfGroup interval = f2 . asyncly . f1
  where
    -- delay everything by `interval`, and insert 'Nothing' markers where the value first came in
    f1 = SP.concatMapWith (<>) \x ->
        SP.yieldM (pure Nothing) <> SP.yieldM (threadDelay (round $ 1_000_000 * interval) >> pure (Just x))
    -- ignore any event which appears within `interval` of a 'Nothing'
    f2 = SP.mapMaybe id . SP.map snd . flip SP.postscanlM' (const False, undefined) \(tooSoon, _) -> \case
        Just x -> do
            t <- getCurrentTime
            pure (tooSoon, guard (not $ tooSoon t) $> x)
        Nothing -> do
            t <- getCurrentTime
            pure (\t' -> diffUTCTime t' t < realToFrac interval, Nothing)
