{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Codec.Picture
import Codec.QRCode qualified as QR
import Codec.QRCode.JuicyPixels qualified as QR
import Control.Concurrent
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Char
import Data.Either.Extra
import Data.Functor
import Data.List
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Tuple.Extra
import Data.Void (Void)
import Dhall (FromDhall)
import Dhall.Core qualified as Dhall
import Dhall.Import qualified as Dhall
import Dhall.Parser qualified as Dhall
import Dhall.TypeCheck qualified as Dhall
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Layout
import Monpad
import OS qualified
import Options.Applicative
import Streamly (Async, Serial, asyncly, serially)
import Streamly.FSNotify
import Streamly.Prelude qualified as SP
import System.Directory
import System.Exit
import System.FilePath
import System.Info.Extra
import Text.Pretty.Simple
import Util

type Port = Int

data Args = Args
    { quiet :: Bool
    , systemDevice :: Bool
    , watchLayout :: Bool
    , port :: Port
    , imageDir :: Maybe FilePath
    , layoutExprs :: [Text]
    , externalWS :: Maybe Port
    , qrPath :: Maybe FilePath
    , pingFrequency :: Int
    , displayPing :: Bool
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
        , metavar "PORT"
        , value 8000
        , showDefault
        , help "Port number for the server to listen on."
        ]
    pingFrequency <- option auto $ mconcat
        [ long "ping"
        , metavar "SECONDS"
        , value 30
        , showDefault
        , help "How often to send the client a ping message to keep it awake."
        ]
    imageDir <- optional . strOption $ mconcat
        [ long "assets"
        , metavar "DIR"
        , help "Directory from which to serve image/audio files etc."
        ]
    layoutExprs <- many . strOption $ mconcat
        [ long "layout"
        , short 'l'
        , metavar "EXPR"
        , help "Dhall expression to control layout of buttons etc. The first of these will be used initially."
        ]
    qrPath <- optional . strOption $ mconcat
        [ long "qr"
        , metavar "PATH"
        , help "Write QR encoding of server address as a PNG file."
        ]
    displayPing <- switch $ mconcat
        [ long "show-ping"
        , help "Indicate the user's current ping in the top-right of the screen."
        ]
    externalWS <- optional . option auto $ mconcat
        [ long "ext-ws"
        , metavar "PORT"
        , help
            "Don't run the websocket server. Frontend will instead look for an external server at the given port. \
            \Note that options such as --ping, --show-ping and --watch-layout will have no effect in this mode."
        ]
    pure Args{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    Args{..} <- execParser $ info (helper <*> parser) (fullDesc <> header "monpad")
    layoutExprs' <- traverse windowsHack layoutExprs
    (dhallFile, dhallLayouts) <- case nonEmpty layoutExprs' of
        Just layouts -> (,layouts) . maybeToEither "file does not exist: maybe expression is not a file import?"
            <$> (canonicalizePathSafe . T.unpack $ NE.head layouts)
        Nothing -> pure (Left "no layout file specified", pure (defaultDhall ()))
    case externalWS of
        Just wsPort -> serverExtWs @() @() (maybe mempty writeQR qrPath) port wsPort imageDir
            =<< layoutsFromDhall dhallLayouts
        Nothing -> if systemDevice
            then join (run . OS.conf . NE.head) =<< layoutsFromDhall dhallLayouts
            else run @() @() @Unit @Unit mempty =<< layoutsFromDhall dhallLayouts
          where
            run :: forall e s a b. (Monoid e, Monoid s, FromDhall a, FromDhall b) =>
                ServerConfig e s a b -> Layouts a b -> IO ()
            run sc ls = do
                scSendLayout <- if watchLayout
                    then case dhallFile of
                        Right file -> do
                            (_, es) <- liftIO $ watchDirectory (takeDirectory file) watchPred
                            T.putStrLn $ "Watching: " <> T.pack file
                            pure mempty
                                { updates = const . serially $
                                    traceStream (const $ T.putStrLn "Sending new layout to client") $
                                    SP.map (pure . const . SetLayout) $
                                    SP.mapMaybeM (const $ mkLayout file) $
                                    lastOfGroup es
                                }
                          where watchPred = isModification `conj` EventPredicate ((== file) . eventPath)
                        Left s -> T.putStrLn ("Cannot watch layout: " <> s) >> exitFailure
                    else mempty
                let scBase = mconcat
                        [ scPrintStuff quiet
                        , scSendLayout
                        , maybe mempty scQR qrPath
                        , sc
                        ]
                    runServer = server pingFrequency port imageDir ls
                if displayPing then
                    runServer $ combineConfs (scPing . viewBox $ NE.head ls) scBase
                else
                    runServer scBase

scPing :: forall s a b. Monoid s => ViewBox -> ServerConfig (MVar NominalDiffTime) s a b
scPing vb =
    let onNewConnection = const $ (,mempty) <$> newEmptyMVar
        onPong = putMVar
        textElementId = ElementID "_internal_ping_text"
        indicatorElementId = ElementID "_internal_ping_indicator"
        (location, size) =
            let ViewBox{..} = vb
                s = min w h `div` 4
             in ( V2 (x + w - s `div` 2) (y + h - s `div` 2)
                , s
                )
        square = Rectangle $ fromIntegral <$> V2 size size
        initialUpdate =
            [ AddElement $ FullElement
                { location
                , name = indicatorElementId
                , showName = Nothing
                , element = Indicator Indicator'
                    { hollowness = 0
                    , arcStart = 0
                    , arcEnd = 1
                    , centre = 0
                    , colour = Colour 1 1 1 1 -- white
                    , shape = square
                    }
                }
            , AddElement $ FullElement
                { location
                , name = textElementId
                , showName = Nothing
                , element = TextBox TextBox'
                    { text = "Ping"
                    , style = TextStyle 50 (Colour 0 0 0 1) False False False
                    }
                }
            ]
        updates m = SP.cons (map const initialUpdate) $ const <<$>> SP.repeatM do
            time <- takeMVar m
            let okPing = 1 / 10 -- time in seconds to map to 0.5 goodness
                scaleFactor = negate $ log 0.5 / okPing
                goodness = exp $ negate (realToFrac time) * scaleFactor -- in range (0, 1]
            pure
                [ SetText textElementId $ showT time
                , SetIndicatorColour indicatorElementId $ Colour (1 - goodness) goodness 0 1
                ]
     in ServerConfig
            { onNewConnection
            , onPong
            , updates
            , onStart = mempty
            , onMessage = mempty
            , onAxis = mempty
            , onButton = mempty
            , onDroppedConnection = mempty
            }

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
        ClientID c <- asks thd3
        unless quiet do
            liftIO $ T.putStrLn $ "Message received from client: " <> c
            pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} m
    , onDroppedConnection = \_ -> do
        ClientID i <- asks thd3
        liftIO $ T.putStrLn $ "Client disconnected: " <> i
    }

mkLayout :: (FromDhall a, FromDhall b) => FilePath -> IO (Maybe (Layout a b))
mkLayout file = printDhallErrors $ layoutFromDhall =<< dhallResolve file
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

canonicalizePathSafe :: FilePath -> IO (Maybe FilePath)
canonicalizePathSafe p = doesFileExist p >>= \case
    True -> Just <$> canonicalizePath p
    False -> mempty

--TODO this is a pretty egregious workaround for Dhall's inability to parse paths beginning with C:\
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
