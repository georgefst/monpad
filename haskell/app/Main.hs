{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Codec.Picture
import qualified Codec.QRCode as QR
import qualified Codec.QRCode.JuicyPixels as QR
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bifunctor
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

type Port = Int

data Args = Args
    { quiet :: Bool
    , systemDevice :: Bool
    , watchLayout :: Bool
    , port :: Port
    , imageDir :: Maybe FilePath
    , layoutExpr :: Maybe Text
    --TODO this is only really needed due to the fact that Dhall doesn't like absolute paths on Windows
    , layoutFile :: Maybe FilePath
    , externalWS :: Maybe Port
    , qrPath :: Maybe FilePath
    }

parser :: Parser Args
parser = do
    quiet <- switch $ short 'q' <> long "quiet"
    systemDevice <- fmap not $ switch $ long "no-system-device"
    watchLayout <- switch $ mconcat
        [ long "watch-layout"
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
    layoutExpr <- optional . strOption $ mconcat
        [ long "layout-expr"
        , short 'l'
        , metavar "EXPR"
        , help "Dhall expression to control layout of buttons etc."
        ]
    layoutFile <- optional . strOption $ mconcat
        [ long "layout-file"
        , metavar "FILE"
        , help "Dhall file containing expression to control layout of buttons etc."
        ]
    qrPath <- optional . strOption $ mconcat
        [ long "qr"
        , metavar "PATH"
        , help "Write QR encoding of server address as a PNG file"
        ]
    externalWS <- optional . option auto $ mconcat
        [ long "ext-ws"
        , metavar "PORT"
        , help "Don't run the websocket server. Frontend will instead look for an external server at the given port."
        ]
    pure Args{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    Args{..} <- execParser $ info (helper <*> parser) (fullDesc <> header "monpad")
    let checkDhallFile file = doesFileExist file >>= \case
            True -> Right <$> canonicalizePath file
            False -> pure $ Left "file does not exist"
    (dhallFile, dhallLayout) <- case (layoutExpr, layoutFile) of
        (Just e, Nothing) -> (,e) . first (<> ": maybe expression is not a file import?")
            <$> checkDhallFile (T.unpack e)
        (Nothing, Just f) -> (,) <$> checkDhallFile f <*> T.readFile f
        (Nothing, Nothing) -> pure
            ( Left "no layout file specified"
            , defaultDhall ()
            )
        (Just _, Just _) -> T.putStrLn "You can only specify one of --layout-expr and --layout-file" >> exitFailure
    case externalWS of
        Just wsPort -> serverExtWs (maybe mempty writeQR qrPath) port wsPort imageDir
            =<< layoutFromDhall @() @() dhallLayout
        Nothing -> if systemDevice
            then join (run . OS.conf) =<< layoutFromDhall dhallLayout
            else run @() @() @Unit @Unit mempty =<< layoutFromDhall dhallLayout
          where
            run :: forall e s a b. (Monoid e, Monoid s, FromDhall a, FromDhall b) =>
                ServerConfig e s a b -> Layout a b -> IO ()
            run sc l = do
                scSendLayout <- if watchLayout
                    then case dhallFile of
                        Right file -> do
                            (_, es) <- liftIO $ watchDirectory (takeDirectory file) watchPred
                            T.putStrLn $ "Watching: " <> T.pack file
                            pure mempty
                                { updates = serially $
                                    traceStream (const $ T.putStrLn "Sending new layout to client") $
                                    SP.map (const . const . SetLayout) $
                                    SP.mapMaybeM (const $ mkUpdate file) $
                                    lastOfGroup es
                                }
                          where watchPred = isModification `conj` EventPredicate ((== file) . eventPath)
                        Left s -> T.putStrLn ("Cannot watch layout: " <> s) >> exitFailure
                    else mempty
                server port imageDir l $ scPrintStuff quiet <> scSendLayout <> maybe mempty scQR qrPath <> sc

scQR :: (Monoid e, Monoid s) => FilePath -> ServerConfig e s a b
scQR path = mempty{onStart = writeQR path}

writeQR :: FilePath -> Text -> IO ()
writeQR path0 url = case QR.encodeText (QR.defaultQRCodeOptions QR.M) QR.Iso8859_1OrUtf8WithoutECI url of
    Nothing -> T.putStrLn "Failed to encode URL as QR code"
    Just qr -> do
        path <- doesDirectoryExist path0 <&> \case
            True -> path0 </> "monpad-address-qr.png"
            False -> path0
        savePngImage path . ImageY8 $ QR.toImage 4 100 qr
        T.putStrLn $ "Server address encoded as: " <> T.pack path

scPrintStuff :: (Monoid e, Monoid s) => Bool -> ServerConfig e s a b
scPrintStuff quiet = mempty
    { onStart = \url -> T.putStrLn $ "Monpad server started at " <> url
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
-- | Ignore events which are followed within 0.1s.
lastOfGroup :: Async a -> Serial a
lastOfGroup = f2 . asyncly . f1
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
    interval = 0.1 :: Float
