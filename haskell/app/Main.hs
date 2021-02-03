{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Void (Void)
import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Monpad
import qualified OS
import Options.Applicative
import Streamly (Serial, asyncly, serially)
import Streamly.FSNotify
import qualified Streamly.Prelude as SP
import System.Directory
import System.Exit
import System.FilePath
import Text.Pretty.Simple

data Args = Args
    { quiet :: Bool
    , systemDevice :: Bool
    , watchLayout :: Bool
    , port :: Int --TODO 'Port'
    , imageDir :: Maybe FilePath
    , dhallLayout :: Text
    }

parser :: Parser Args
parser = do
    quiet <- switch $ short 'q' <> long "quiet"
    systemDevice <- switch $ long "system-device"
    watchLayout <- switch $ long "watch-layout" --TODO check how this works with multiple clients
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
    let watchPred = (isCreation `disj` isModification) `conj` EventPredicate ((== layoutFile) . eventPath)
    evs <- if watchLayout
        then do
            unlessM (doesFileExist layoutFile) do
                T.putStrLn "Dhall expression provided is not a file, so can't be watched"
                exitFailure
            (_, es) <- liftIO (watchDirectory (takeDirectory layoutFile) watchPred)
            T.putStrLn $ "Watching: " <> T.pack layoutFile
            pure $ traceStream (const $ T.putStrLn "Sending new layout to client") $
                SP.mapMaybeM (mkUpdate . eventPath) $ ignoreCloseFollowers es
        else pure mempty
    let run :: ServerConfig e s a b -> Layout a b -> IO ()
        run sc l = server port imageDir l ServerConfig
            { onStart = T.putStrLn "Monpad server started" >> onStart sc
            , onNewConnection = \c@(ClientID i) -> do
                T.putStrLn $ "New client: " <> i
                onNewConnection sc c
            , onMessage = \m -> do
                ClientID c <- asks snd
                unless quiet do
                    liftIO $ T.putStrLn $ "Message received from client: " <> c
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} m
                onMessage sc m
            , onAxis = onAxis sc
            , onButton = onButton sc
            , onDroppedConnection = \e -> do
                ClientID i <- asks snd
                liftIO $ T.putStrLn $ "Client disconnected: " <> i
                onDroppedConnection sc e
            , updates = asyncly $ serially evs <> serially (updates sc)
            }
    if systemDevice
        then join (run . OS.conf) =<< layoutFromDhall dhallLayout
        else run emptyServerConfig =<< layoutFromDhall dhallLayout

mkUpdate :: FilePath -> IO (Maybe ServerUpdate)
mkUpdate file = printDhallErrors $ fmap SetLayout <$> layoutFromDhall =<< dhallResolve file
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
    -}
    dhallResolve p = fmap Dhall.pretty do
        x <- Dhall.loadRelativeTo (takeDirectory p) Dhall.UseSemanticCache
            =<< Dhall.throws . Dhall.exprFromText p
            =<< T.readFile p
        T.putStrLn $ "Parsed Dhall expression: " <> Dhall.hashExpressionToCode (Dhall.normalize x)
        pure x

{-TODO
the issue (on Linux, though Mac seems similar) is that we get too many events, when all we care about is CLOSE_WRITE
    but because 'fsnotify' is cross-platform, there may be no good way to filter
what may be needed on Linux (this is only tested on Mac right now) is to actually take the last of these events
    slightly trickier to implement
test on all platforms, with debug logging on result of 'tooSoon'
-}
-- | Ignore any event which appears within 1s of another.
ignoreCloseFollowers :: Serial a -> Serial a
ignoreCloseFollowers = SP.mapMaybe id . SP.map snd . flip SP.postscanlM' (const False, undefined) \(tooSoon, _) x -> do
    t <- getCurrentTime
    pure (\t' -> diffUTCTime t' t < 1, guard (not $ tooSoon t) $> x)

--TODO better name
-- | Attach an extra action to each element of the stream.
traceStream :: (a -> IO ()) -> Serial a -> Serial a
traceStream f = SP.mapM \x -> f x >> pure x
