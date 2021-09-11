{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main (main) where

import Control.Monad
import Data.Char
import Data.Either.Extra
import Data.Functor
import Data.List
import Data.List.Extra
import Data.Maybe
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Info.Extra
import Text.Read
import Util.Util

import Data.List.NonEmpty (nonEmpty, NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dhall (FromDhall)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Monpad
import Monpad.Plugins
import Monpad.Plugins.LayoutSwitcher qualified as LayoutSwitcher
import Monpad.Plugins.Logger qualified as Logger
import Monpad.Plugins.PingIndicator qualified as PingIndicator
import Monpad.Plugins.QR qualified as QR
import Monpad.Plugins.WatchLayout qualified as WatchLayout
import OS qualified

type Port = Int

data Args = Args
    { verbosity :: Maybe Logger.Settings
    , systemDevice :: Bool
    , watchLayout :: Bool
    , port :: Port
    , assetsDir :: Maybe FilePath
    , layoutExprs :: [Text]
    , externalWS :: Maybe Port
    , qrPath :: Maybe FilePath
    , pingFrequency :: Int
    , displayPing :: Bool
    , scale :: Double
    , loginImageUrl :: Maybe Text
    }

parser :: Parser Args
parser = do
    verbosity <-
        let reader = eitherReader $ maybeToEither errorString . (invert' allValues verbosityToInt <=< readMaybe)
            errorString = "value must be an integer between 0 and " <> show (length allValues - 1)
            allValues = Nothing : map Just enumerate
            verbosityToInt = \case
                Nothing -> 0 :: Int
                Just Logger.Quiet -> 1
                Just Logger.Normal -> 2
                Just Logger.Loud -> 3
         in option reader $ mconcat
            [ short 'v'
            , help $
                "Verbosity (0 to "
                    ++ show (fromEnum (maxBound @Logger.Settings) + 1)
                    ++ ") - how much info to log to stdout about messages from clients etc."
            , value $ Just Logger.Quiet
            , showDefaultWith $ show . verbosityToInt
            ]
    port <- option auto $ mconcat
        [ long "port"
        , short 'p'
        , metavar "PORT"
        , value 8000
        , showDefault
        , help "Port number for the server to listen on."
        ]
    assetsDir <- optional . strOption $ mconcat
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
    watchLayout <- switch $ mconcat
        [ long "watch-layout"
        , help "Watch all files involved in the layout expression, updating client with any changes."
        ]
    qrPath <- optional . strOption $ mconcat
        [ long "qr"
        , metavar "PATH"
        , help "Write QR encoding of server address as a PNG file."
        ]
    pingFrequency <- option auto $ mconcat
        [ long "ping"
        , metavar "SECONDS"
        , value 30
        , showDefault
        , help "How often to send the client a ping message to keep it awake."
        ]
    displayPing <- switch $ mconcat
        [ long "show-ping"
        , help "Indicate the user's current ping in the top-right of the screen."
        ]
    scale <- option auto $ mconcat
        [ long "scale"
        , metavar "NUMBER"
        , value 1
        , showDefault
        , help
            "Scale UI elements \
            \(this only affects those which aren't specified by the layout - e.g. the ping indicator)."
        ]
    systemDevice <- fmap not $ switch $ mconcat
        [ long "no-system-device"
        , help "Don't create an OS-level virtual device."
        ]
    externalWS <- optional . option auto $ mconcat
        [ long "ext-ws"
        , metavar "PORT"
        , help
            "Don't run the websocket server. Frontend will instead look for an external server at the given port. \
            \Note that options such as --ping, --show-ping and --watch-layout will have no effect in this mode."
        ]
    loginImageUrl <- optional . strOption $ mconcat
        [ long "login-image"
        , metavar "URL"
        , help "Background image for login page."
        ]
    pure Args{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    Args{..} <- execParser $ info (helper <*> parser) (fullDesc <> header "monpad")
    dhallLayouts <- fromMaybe (pure $ defaultDhall ()) . nonEmpty <$> traverse windowsHack layoutExprs
    case externalWS of
        Just wsPort -> serverExtWs (maybe mempty writeQR qrPath) port wsPort loginImageUrl assetsDir
            =<< mkLayouts dhallLayouts
          where
            writeQR path url = withPlugin (QR.plugin path) $ flip onStart url
        Nothing -> if systemDevice
            --TODO with `ImpredicativeTypes`, we can remove the explicit lambdas here and go point-free
            then mkLayouts dhallLayouts >>= \ls ->
                withPlugin (plugins [plugin OS.keyUnknown, Plugin OS.conf]) $ runPlugin ls
            else mkLayouts dhallLayouts >>= \ls ->
                withPlugin (plugin @() ()) $ runPlugin ls
          where
            plugin :: forall a b. (FromDhall a, FromDhall b, Show a, Show b) => b -> Plugin a b
            plugin unknown = plugins
                $ applyWhen displayPing (PingIndicator.plugin scale :)
                $ applyWhen watchLayout (WatchLayout.plugin :)
                $ applyWhen (length dhallLayouts > 1) (LayoutSwitcher.plugin scale unknown :)
                $ maybe id ((:) . QR.plugin) qrPath
                $ maybe id ((:) . Logger.plugin T.putStrLn) verbosity
                []
            runPlugin :: Layouts a b -> ServerConfig e s a b -> IO ()
            runPlugin = server pingFrequency port loginImageUrl assetsDir

-- | Run 'layoutsFromDhall' and exit if it fails.
mkLayouts :: (FromDhall a, FromDhall b) => NonEmpty Text -> IO (Layouts a b)
mkLayouts = maybe exitFailure pure <=< layoutsFromDhall

--TODO this is a pretty egregious workaround for Dhall's inability to parse paths beginning with C:\
-- see: https://github.com/dhall-lang/dhall-lang/issues/1153
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
