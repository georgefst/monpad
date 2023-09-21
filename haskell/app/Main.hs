{-# LANGUAGE ApplicativeDo #-}

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
import System.IO
import Text.Read
import Util.Util

import Control.Concurrent.Lock qualified as Lock
import Data.List.NonEmpty (nonEmpty, NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dhall (FromDhall)
import GHC.IO.Encoding (setLocaleEncoding)

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
    , dumpHTML :: Maybe FilePath
    , encoding :: Encoding
    , qrPath :: Maybe FilePath
    , pingFrequency :: Int
    , displayPing :: Bool
    , scale :: Double
    , nColours :: Int
    , windowTitle :: Maybe Text
    , wsCloseMessage :: Maybe Text
    , loginPageTitle :: Maybe Text
    , loginImageUrl :: Maybe Text
    , loginUsernamePrompt :: Maybe Text
    , loginUsernamePromptStyle :: Maybe Text
    , loginSubmitButtonStyle :: Maybe Text
    , loginSubmitButtonText :: Maybe Text
    , loginSubmitButtonTextStyle :: Maybe Text
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
    dumpHTML <- optional . option auto $ mconcat
        [ long "dump-html"
        , metavar "PATH"
        , help "Don't run any server. Just dump the HTML to a file. Doesn't handle login or serving assets."
        ]
    encoding <- flag BinaryEncoding JSONEncoding $ mconcat
        [ long "json"
        , help "Send messages as JSON, instead of more compact binary encoding."
        ]
    nColours <- option auto $ mconcat
        [ long "colours"
        , metavar "INT"
        , help "Number of colours associated with each user (default 1)."
        , value 1
        ]
    windowTitle <- optional . strOption $ mconcat
        [ long "window-title"
        , metavar "STRING"
        ]
    wsCloseMessage <- optional . strOption $ mconcat
        [ long "ws-close-message"
        , metavar "STRING"
        ]
    loginPageTitle <- optional . strOption $ mconcat
        [ long "login-title"
        , metavar "STRING"
        ]
    loginImageUrl <- optional . strOption $ mconcat
        [ long "login-image"
        , metavar "URL"
        ]
    loginUsernamePrompt <- optional . strOption $ mconcat
        [ long "login-prompt"
        , metavar "STRING"
        ]
    loginUsernamePromptStyle <- optional . strOption $ mconcat
        [ long "login-prompt-style"
        , metavar "STRING"
        ]
    loginSubmitButtonText <- optional . strOption $ mconcat
        [ long "login-submit"
        , metavar "STRING"
        ]
    loginSubmitButtonStyle <- optional . strOption $ mconcat
        [ long "login-submit-style"
        , metavar "STRING"
        ]
    loginSubmitButtonTextStyle <- optional . strOption $ mconcat
        [ long "login-submit-text-style"
        , metavar "STRING"
        ]
    pure Args{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    args <- execParser $ info (helper <*> parser) (fullDesc <> header "monpad")
    dhallLayouts <- fromMaybe (pure $ defaultDhall ()) . nonEmpty <$> traverse windowsHack args.layoutExprs
    stdoutMutex <- Lock.new -- to ensure atomicity of writes to `stdout`
    let write = Logger
            { log = \t -> Lock.acquire stdoutMutex >> T.putStrLn t >> Lock.release stdoutMutex
            , logError = \t -> Lock.acquire stdoutMutex >> T.hPutStrLn stderr t >> Lock.release stdoutMutex
            , ansi = True
            }
        wsCloseMessage = fromMaybe "Connection lost. See console for details." args.wsCloseMessage
        windowTitle = fromMaybe "monpad" args.windowTitle
        loginOpts = LoginPageOpts
            { pageTitle = fromMaybe defaultLoginPageOpts.pageTitle args.loginPageTitle
            , imageUrl = args.loginImageUrl
            , usernamePrompt = fromMaybe defaultLoginPageOpts.usernamePrompt args.loginUsernamePrompt
            , usernamePromptStyle = fromMaybe defaultLoginPageOpts.usernamePromptStyle args.loginUsernamePromptStyle
            , submitButtonStyle = fromMaybe defaultLoginPageOpts.submitButtonStyle args.loginSubmitButtonStyle
            , submitButtonText = fromMaybe defaultLoginPageOpts.submitButtonText args.loginSubmitButtonText
            , submitButtonTextStyle = fromMaybe defaultLoginPageOpts.submitButtonTextStyle args.loginSubmitButtonTextStyle
            }
    case (args.externalWS, args.dumpHTML) of
        --TODO bit hacky
        -- try to specify mutual-exclusivity as part of parser definition, by making it modal (will change CLI)
        -- also would be good way to make clearer which flags are relevant in which modes
        -- we actually use the --port flag here for the WS port, when it's usually the HTML one, which is confusing
        (Just _, Just _) -> error "--dump-html and --ext-ws can't be used together"
        (Nothing, Just outFile) ->
            justDumpHTML
                args.encoding
                outFile
                args.port
                windowTitle
                wsCloseMessage
                =<< mkLayouts write dhallLayouts
        (Just wsPort, Nothing) ->
            serverExtWs
                (maybe mempty writeQR args.qrPath)
                args.encoding
                args.port
                wsPort
                windowTitle
                wsCloseMessage
                loginOpts
                args.nColours
                args.assetsDir
                =<< mkLayouts write dhallLayouts
          where
            writeQR path url = withPlugin (QR.plugin write path) $ flip (.onStart) url
        (Nothing, Nothing) -> if args.systemDevice
            then withPlugin (plugins [plugin OS.keyUnknown, Plugin OS.conf]) . runPlugin
                =<< mkLayouts write dhallLayouts
            else withPlugin (plugin @() ()) . runPlugin
                =<< mkLayouts write dhallLayouts
          where
            plugin :: forall a b. (FromDhall a, FromDhall b, Show a, Show b) => b -> Plugin a b
            plugin unknown = plugins
                $ applyWhen args.displayPing (PingIndicator.plugin args.scale :)
                $ applyWhen args.watchLayout (WatchLayout.plugin write :)
                $ applyWhen (length dhallLayouts > 1) (LayoutSwitcher.plugin args.scale unknown :)
                $ maybe id ((:) . QR.plugin write) args.qrPath
                $ maybe id ((:) . Logger.plugin write) args.verbosity
                []
            {- HLINT ignore "Eta reduce" -}
            runPlugin :: Layouts a b -> (forall e s. ServerConfig e s a b -> IO ())
            runPlugin ls = server write args.pingFrequency args.encoding args.port windowTitle wsCloseMessage loginOpts args.nColours args.assetsDir ls

-- | Run 'layoutsFromDhall' and exit if it fails.
mkLayouts :: (FromDhall a, FromDhall b) => Logger -> NonEmpty Text -> IO (Layouts a b)
mkLayouts write = maybe exitFailure pure <=< layoutsFromDhall write

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
