{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Codec.Picture
import Codec.QRCode qualified as QR
import Codec.QRCode.JuicyPixels qualified as QR
import Control.Concurrent
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Traversable
import Data.Tuple.Extra
import Data.Void (Void)
import Dhall (FromDhall)
import Dhall.Core qualified as Dhall
import Dhall.Import qualified as Dhall
import Dhall.Parser qualified as Dhall
import Dhall.TypeCheck qualified as Dhall
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Monpad
import OS qualified
import Options.Applicative
import Streamly (serially)
import Streamly.FSNotify
import Streamly.Prelude qualified as SP
import System.Directory
import System.FilePath
import Text.Pretty.Simple
import Util
import "georgefst-utils" Util

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
    , loginImageUrl :: Maybe Text
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
    loginImageUrl <- optional . strOption $ mconcat
        [ long "login-image"
        , metavar "URL"
        , help "Background image for login page."
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
    let dhallLayouts = case nonEmpty layoutExprs' of
            Just layouts -> layouts
            Nothing -> pure $ defaultDhall ()
    case externalWS of
        Just wsPort -> serverExtWs @() @() (maybe mempty writeQR qrPath) port wsPort loginImageUrl imageDir
            =<< layoutsFromDhall dhallLayouts
        Nothing -> if systemDevice
            then join (run . OS.conf . NE.head) =<< layoutsFromDhall dhallLayouts
            else run @() @() @Unit @Unit mempty =<< layoutsFromDhall dhallLayouts
          where
            run :: forall e s a b. (Monoid e, Monoid s, FromDhall a, FromDhall b) =>
                ServerConfig e s a b -> Layouts a b -> IO ()
            run sc ls =
                if displayPing then
                    runServer $ combineConfs (scPing . viewBox $ NE.head ls) scBase
                else
                    runServer scBase
              where
                runServer = server pingFrequency port loginImageUrl imageDir ls
                scBase = mconcat
                    [ scPrintStuff quiet
                    , if watchLayout then scSendLayout $ NE.head dhallLayouts else mempty
                    , maybe mempty scQR qrPath
                    , sc
                    ]

{-TODO
find a way to remove 'lastOfGroup' workaround
the issue (seemingly on all three platforms) is that we get too many events, when all we care about is CLOSE_WRITE
    but because 'fsnotify' is cross-platform, there may be no good way to filter
-}
scSendLayout :: (Monoid e, Monoid s, FromDhall a, FromDhall b) => Text -> ServerConfig e s a b
scSendLayout exprText = mempty
    { updates = const do
        imports <- dhallImports exprText
        evss <- for imports \(dir, toList -> files) -> liftIO do
            let isImport = EventPredicate $ (`elem` files) . T.pack . takeFileName . eventPath
            T.putStrLn $ "Watching: " <> T.pack dir <> " (" <> T.intercalate ", " files <> ")"
            snd <$> watchDirectory dir (isModification `conj` isImport)
        flip foldMap evss $ serially
            . traceStream (const $ T.putStrLn "Sending new layout to client")
            . SP.map (pure . const . SetLayout)
            . SP.mapMaybeM (const $ mkLayout exprText)
            . lastOfGroup 100_000
    }

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

mkLayout :: (FromDhall a, FromDhall b) => Text -> IO (Maybe (Layout a b))
mkLayout expr = printDhallErrors $ layoutFromDhall =<< dhallResolve expr
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
    dhallResolve e = do
        x <- Dhall.load =<< Dhall.throws (Dhall.exprFromText "" e)
        _ <- Dhall.throws $ Dhall.typeOf x
        T.putStrLn $ "Parsed Dhall expression: " <> Dhall.hashExpressionToCode (Dhall.normalize x)
        pure $ Dhall.pretty x
