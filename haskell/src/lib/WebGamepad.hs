module WebGamepad (
    server,
    ServerConfig(..),
    defaultConfig,
    Args(..),
    defaultArgs,
    getCommandLineArgs,
    argParser,
    Message(..),
    ClientID(..),
    Update(..),
    Button(..),
    V2(..),
) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Aeson (eitherDecode, ToJSON, FromJSON)
import Data.Composition
import Data.List
import Data.Maybe
import Data.Proxy
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Embed
import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Linear
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets qualified as WS
import Options.Applicative
import Orphans.Aeson
import Servant
import Servant.HTML.Lucid
import Text.Pretty.Simple

newtype ClientID = ClientID Text
    deriving newtype (Eq,Ord,Show,IsString)

data Button
    = Blue
    | Yellow
    | Red
    | Green
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)

data Update
    = ButtonUp Button
    | ButtonDown Button
    | Stick (V2 Double) -- always a vector within the unit circle
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)

data Message = Message
    { clientId :: ClientID
    , message :: Update
    } deriving (Eq, Ord, Show)

type Root = "gamepad"
type UsernameParam = "username"
type API = Root :> QueryParam UsernameParam Text :> Get '[HTML] (Html ())

--TODO add styling
loginHtml :: Html ()
loginHtml = doctypehtml_ $ form_ [action_ $ textSym @Root] $
    title_ "Gamepad: login"
        <>
    style_ (mainCSS ())
        <>
    label_ [for_ nameBoxId] "Username:"
        <>
    br_ []
        <>
    input_ [type_ "text", id_ nameBoxId, name_ $ textSym @UsernameParam]
        <>
    input_ [type_ "submit", value_ "Go!"]
  where
    nameBoxId = "name"

--TODO investigate performance - is it expensive to reassemble the HTML for a new username?
-- mainHtml :: Monad m => StaticData -> Text -> HtmlT m ()
mainHtml :: Args -> Text -> Html ()
mainHtml Args{address,wsPort} username = doctypehtml_ $
    style_ (mainCSS ())
        <>
    script_ [type_ jsScript] (elmJS ())
        <>
    script_ [type_ jsScript, makeAttribute "username" username, makeAttribute "wsAddress" wsAddr] (jsJS ())
  where
    wsAddr = "ws://" <> T.pack address <> ":" <> showT wsPort
    jsScript = "text/javascript"

defaultArgs :: Args
defaultArgs = Args
    { httpPort = 8000
    , wsPort = 8001
    , address = "localhost"
    , wsPingTime = 30
    }

--TODO better name (perhaps this should be 'ServerConfig'...)
--TODO stronger typing for addresses etc.
data Args = Args
    { httpPort :: Port
    , wsPort :: Port
    , address :: String --TODO only affects WS, not HTTP (why do we only need config for the former?)
    , wsPingTime :: Int
    }
    deriving Show

getCommandLineArgs :: IO Args
getCommandLineArgs = execParser opts
  where
    opts = info (helper <*> argParser) (fullDesc <> header "Web gamepad")

argParser :: Parser Args
argParser = Args
    <$> option auto
        (  long "http-port"
        <> short 'p'
        <> metavar "PORT"
        <> value httpPort
        <> showDefault
        <> help "Port for the HTTP server" )
    <*> option auto
        (  long "ws-port"
        <> short 'w'
        <> metavar "PORT"
        <> value wsPort
        <> showDefault
        <> help "Port for the websocket server" )
    <*> strOption
        (  long "address"
        <> short 'a'
        <> metavar "ADDRESS"
        <> value address
        <> showDefault
        <> help "Address for the websocket server" )
    <*> option auto
        (  long "ws-ping-time"
        <> help "Interval (in seconds) between pings to each websocket"
        <> value wsPingTime
        <> showDefault
        <> metavar "INT" )
  where
    Args{httpPort,wsPort,address,wsPingTime} = defaultArgs

-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s = ServerConfig
    { onStart :: Args -> IO ()
    , onNewConnection :: ClientID -> IO (e,s)
    , onMessage :: Message -> e -> s -> IO s
    , onEnd :: ClientID -> e -> IO () --TODO take s? not easy due to 'bracket' etc...
    , getArgs :: IO Args
    }

defaultConfig :: ServerConfig () ()
defaultConfig = ServerConfig
    { onStart = \Args{httpPort,address} -> T.putStrLn $
        "Server started at: " <> T.pack address <> ":" <> showT httpPort <> "/" <> textSym @Root
    , onNewConnection = \(ClientID i) -> fmap ((),) $ T.putStrLn $ "New client: " <> i
    , onMessage = \m () () -> pPrint m
    , onEnd = \(ClientID i) () -> T.putStrLn $ "Client disconnected: " <> i
    , getArgs = return defaultArgs
    }

--TODO security - currently we just trust the names
server :: ServerConfig e s -> IO ()
server sc = do
    args <- getArgs sc
    onStart sc args
    httpServer args `race_` websocketServer args sc

--TODO reject when username is already in use
httpServer :: Args -> IO ()
httpServer args@Args{httpPort} = do
    let handleMain = return . mainHtml args
        handleLogin = return loginHtml
    run httpPort $ serve (Proxy @API) $ maybe handleLogin handleMain

--TODO use warp rather than 'WS.runServer' (see jemima)
--TODO JSON is unnecessarily expensive - use binary once API is stable?
--TODO under normal circumstances, connections will end with a 'WS.ConnectionException'
    -- we may actually wish to respond to different errors differently
websocketServer :: Args -> ServerConfig e s -> IO ()
websocketServer Args{wsPort,address,wsPingTime} ServerConfig{onNewConnection,onMessage,onEnd} =
    WS.runServer address wsPort $ \pending -> do
        conn <- WS.acceptRequest pending
        clientId <- ClientID <$> WS.receiveData conn --TODO we send this back and forth rather a lot...
        bracket (onNewConnection clientId) (onEnd clientId . fst) $ \(e,s0) ->
            WS.withPingThread conn wsPingTime (return ()) $ flip iterateM_ s0 $ \s ->
                (eitherDecode <$> WS.receiveData conn) >>= \case
                    Left err -> pPrint err >> return s --TODO handle error
                    Right message -> onMessage Message{clientId,message} e s


{- Util -}

textSym :: forall a. KnownSymbol a => Text
textSym = T.pack $ symbolVal $ Proxy @a

showT :: Show a => a -> Text
showT = T.pack . show
