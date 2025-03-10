{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Concurrent.Extra
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
    { common :: CommonArgs
    , mode :: Either NormalArgs ModeArgs
    }
data ModeArgs
    = ExtWs Port
    | DumpHTML {loginFile :: Maybe FilePath, mainFile :: FilePath, optsFile :: Maybe FilePath, noWs :: Bool}
data CommonArgs = CommonArgs
    { port :: Port
    , assetsDir :: Maybe FilePath
    , layoutExprs :: [Text]
    , encoding :: Encoding
    , qrPath :: Maybe FilePath
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
data NormalArgs = NormalArgs
    { verbosity :: Maybe Logger.Settings
    , systemDevice :: Bool
    , watchLayout :: Bool
    , pingFrequency :: Int
    , displayPing :: Bool
    , scale :: Double
    , deadzone :: Maybe Double
    }

parser :: Parser Args
parser = do
    common <- parserCommon
    mode <- asum
        [ fmap Right . hsubparser $ mconcat
            [ command "ext-ws" $ info (ExtWs <$> argument auto (metavar "PORT")) $ progDesc
                "Don't run the websocket server. Frontend will instead look for an external server at the given port."
            , command "dump-html" $ info dumpHtmlParser $ progDesc
                "Don't run any server, even for assets. Just dump the HTML in to the given files. \
                \It is intended that the login HTML is served when a request has no \"username\" parameter."
            ]
        , Left <$> parserNormal
        ]
    pure Args{common, mode}
  where
    dumpHtmlParser = do
        loginFile <- optional $ strOption $ long "login" <> metavar "FILE"
        mainFile <- strOption $ long "main" <> metavar "FILE"
        optsFile <- optional $ strOption $ mconcat
            [ long "opts"
            , metavar "FILE"
            , help "A path to a JSON file on the server containing extra configuration."
            ]
        noWs <- switch $ mconcat
            [ long "no-ws"
            , help "Just send and receive custom DOM events instead of using websockets."
            ]
        pure DumpHTML{..}
parserCommon :: Parser CommonArgs
parserCommon = do
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
    qrPath <- optional . strOption $ mconcat
        [ long "qr"
        , metavar "PATH"
        , help "Write QR encoding of server address as a PNG file."
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
    pure CommonArgs{..}
parserNormal :: Parser NormalArgs
parserNormal = do
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
    systemDevice <- fmap not $ switch $ mconcat
        [ long "no-system-device"
        , help "Don't create an OS-level virtual device."
        ]
    watchLayout <- switch $ mconcat
        [ long "watch-layout"
        , help "Watch all files involved in the layout expression, updating client with any changes."
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
    deadzone <- optional $ option auto $ mconcat
        [ long "deadzone"
        , metavar "NUMBER"
        , help
            "Add a mimimum magnitude (between 0 and 1) to all non-zero joystick events, \
            \in order to counteract some application's use of a deadzone."
        ]
    pure NormalArgs{..}

main :: IO ()
main = do
    setLocaleEncoding utf8
    Args
        { common = CommonArgs
            { wsCloseMessage = fromMaybe "Connection lost. See console for details." -> wsCloseMessage
            , windowTitle = fromMaybe "monpad" -> windowTitle --TODO get GHC to accept `fromMaybe "monpad" -> windowTitle`
            , loginPageTitle = fromMaybe defaultLoginPageOpts.pageTitle -> pageTitle
            , loginImageUrl = imageUrl
            , loginUsernamePrompt = fromMaybe defaultLoginPageOpts.usernamePrompt -> usernamePrompt
            , loginUsernamePromptStyle = fromMaybe defaultLoginPageOpts.usernamePromptStyle -> usernamePromptStyle
            , loginSubmitButtonStyle = fromMaybe defaultLoginPageOpts.submitButtonStyle -> submitButtonStyle
            , loginSubmitButtonText = fromMaybe defaultLoginPageOpts.submitButtonText -> submitButtonText
            , loginSubmitButtonTextStyle = fromMaybe defaultLoginPageOpts.submitButtonTextStyle -> submitButtonTextStyle
            , ..
            }
        , mode = modeArgs
        } <- execParser $ info (helper <*> parser) (fullDesc <> header "monpad")
    dhallLayouts <- fromMaybe (pure $ defaultDhall ()) . nonEmpty <$> traverse windowsHack layoutExprs
    stdoutMutex <- newLock -- to ensure atomicity of writes to `stdout`
    let write = Logger
            { log = withLock stdoutMutex . T.putStrLn
            , logError = withLock stdoutMutex . T.hPutStrLn stderr
            , ansi = True
            }
        loginOpts = LoginPageOpts{..}
    case modeArgs of
        Left NormalArgs{..} -> if systemDevice
            then withPlugin (plugins [plugin OS.keyUnknown, Plugin OS.conf]) . runPlugin
                =<< mkLayouts write dhallLayouts
            else withPlugin (plugin @() ()) . runPlugin
                =<< mkLayouts write dhallLayouts
          where
            plugin :: forall a b. (FromDhall a, FromDhall b, Show a, Show b) => b -> Plugin a b
            plugin unknown = plugins
                $ applyWhen displayPing (PingIndicator.plugin scale :)
                $ applyWhen watchLayout (WatchLayout.plugin write :)
                $ applyWhen (length dhallLayouts > 1) (LayoutSwitcher.plugin scale unknown :)
                $ maybe id ((:) . QR.plugin write) qrPath
                $ maybe id ((:) . Logger.plugin write) verbosity
                []
            runPlugin :: Layouts a b -> (forall e s. ServerConfig e s a b -> IO ())
            runPlugin = server write pingFrequency encoding (pure <$> deadzone) port windowTitle wsCloseMessage loginOpts nColours assetsDir
        Right (ExtWs wsPort) ->
            serverExtWs
                (maybe mempty writeQR qrPath)
                encoding
                port
                wsPort
                windowTitle
                wsCloseMessage
                loginOpts
                nColours
                assetsDir
                =<< mkLayouts write dhallLayouts
          where
            writeQR path url = withPlugin (QR.plugin write path) $ flip (.onStart) url
        Right (DumpHTML{..}) ->
            dumpHTML
                encoding
                loginFile
                mainFile
                optsFile
                (if noWs then Nothing else Just port)
                windowTitle
                wsCloseMessage
                loginOpts
                nColours
                =<< mkLayouts write dhallLayouts

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
