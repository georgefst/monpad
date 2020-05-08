{-# OPTIONS_GHC -Wno-orphans #-} --TODO move to separate module
module Lib (
    elm,
    server,
    ServerConfig(..),
    defaultConfig,
    Args(..),
    getCommandLineArgs,
    Message(..),
    ClientID(..),
    Update(..),
    Button(..),
    V2(..),
) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.IO.Class
import           Control.Concurrent.Async
import           Data.Aeson (ToJSON,FromJSON,Value)
import qualified Data.Aeson as Aeson
import           Data.Bool
import qualified Data.ByteString as BS
import           Data.Composition
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.List
import           Data.IORef
import           Data.String (IsString)
import           Embed
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)
import           GHC.TypeLits (KnownSymbol,symbolVal)
import qualified Language.Elm.Definition as Elm
import qualified Language.Elm.Expression as ElmExpr
import qualified Language.Elm.Name as Elm
import qualified Language.Elm.Pretty as Elm
import qualified Language.Elm.Simplification as Elm
import qualified Language.Elm.Type as Elm
import           Language.Haskell.To.Elm
import           Linear
import           Lucid
import           Lucid.Base (makeAttribute)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import qualified Network.WebSockets as WS
import           Options.Generic
import           Servant
import           Servant.API.Verbs
import           Servant.HTML.Lucid
import           Servant.To.Elm
import           System.Directory
import           System.FilePath
import           Text.Pretty.Simple

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
    head_ (
        style_ mainCSS
            <>
        script_ [type_ jsScript] elmJS
            <>
        --TODO 0.7 is totally arbitrary (chosen to work with George's phone) - test other devices
        meta_ [name_ "viewport", content_ "maximum-scale=0.7, user-scalable=no"]
    )
        <>
    body_ mempty
        <>
    script_ [type_ jsScript, makeAttribute "username" username, makeAttribute "wsAddress" wsAddr] jsJS
  where
    wsAddr = "ws://" <> T.pack address <> ":" <> showT wsPort
    jsScript = "text/javascript"

--TODO manual parser to allow short options, defaults, help text etc.
defaultArgs :: Args
defaultArgs = Args
    { httpPort = 8000
    , wsPort = 8001
    , address = "localhost"
    , wsPingTime = 30
    }

--TODO export parser instead, so it can be composed
getCommandLineArgs :: Text -> IO Args
getCommandLineArgs = getRecord

--TODO better name (perhaps this should be 'ServerConfig'...)
--TODO stronger typing for addresses etc.
-- ./web-gamepad-test --httpPort 8000 --wsPort 8001 --address 192.168.0.18 --wsPingTime 30
data Args = Args
    { httpPort :: Port
    , wsPort :: Port
    , address :: String --TODO only affects WS, not HTTP (why do we only need config for the former?)
    , wsPingTime :: Int
    }
    deriving (Show,Generic,ParseRecord)

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
    { onStart = \Args{httpPort,wsPort,address} -> do
        T.putStrLn $ "Starting server at: " <> T.pack address
        T.putStrLn $ "  HTTP server at port: " <> showT httpPort
        T.putStrLn $ "  Websocket server at port: " <> showT wsPort
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
websocketServer :: Args -> ServerConfig e s -> IO ()
websocketServer Args{wsPort,address,wsPingTime} ServerConfig{onNewConnection,onMessage,onEnd} =
    WS.runServer address wsPort $ \pending -> do
        conn <- WS.acceptRequest pending
        clientId <- ClientID <$> WS.receiveData conn --TODO we send this back and forth rather a lot...
        bracket (onNewConnection clientId) (onEnd clientId . fst) $ \(e,s0) ->
            WS.withPingThread conn wsPingTime (return ()) $ flip iterateM_ s0 $ \s -> do
                msg <- Aeson.eitherDecode <$> WS.receiveData conn
                case msg of
                    Left err -> pPrint err >> return s --TODO handle error
                    Right message -> onMessage Message{clientId,message} e s


--TODO move to separate module
{- Elm generation -}

elm :: FilePath -> IO ()
elm src =
    let definitions = Elm.simplifyDefinition <$>
            jsonDefinitions @Button <> jsonDefinitions @Update <> jsonDefinitions @(V2 Double)
        modules = Elm.modules definitions
        auto = src </> "Auto"
    in do
        createDirectoryIfMissing False auto
        mapM_ (removeFile . (auto </>)) =<< listDirectory auto
        forM_ (HashMap.toList modules) \(moduleName, contents) ->
            T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
                renderStrict $ layoutPretty defaultLayoutOptions contents

--TODO we don't actually need to decode from elm (only encode)
--TODO could these be derived more easily? this is all *very* generic
    -- 'https://github.com/folq/haskell-to-elm' for some explanations
    -- keep an eye on 'https://github.com/folq/haskell-to-elm/issues/6'
instance HasElmEncoder Value Button where
    elmEncoderDefinition = elmEnc @Button "Auto.Button.encode"
instance HasElmDecoder Value Button where
    elmDecoderDefinition = elmDec @Button "Auto.Button.decode"
instance HasElmType Button where
    elmDefinition = elmTyp @Button "Auto.Button.Button"
instance HasElmEncoder Value Update where
    elmEncoderDefinition = elmEnc @Update "Auto.Update.encode"
instance HasElmDecoder Value Update where
    elmDecoderDefinition = elmDec @Update "Auto.Update.decode"
instance HasElmType Update where
    elmDefinition = elmTyp @Update "Auto.Update.Update"

elmEnc :: forall a. DeriveParameterisedElmEncoderDefinition 0 Value a => Elm.Qualified -> Maybe Elm.Definition
elmEnc s = Just $ deriveElmJSONEncoder @a defaultOptions Aeson.defaultOptions s
elmDec :: forall a. DeriveParameterisedElmDecoderDefinition 0 Value a => Elm.Qualified -> Maybe Elm.Definition
elmDec s = Just $ deriveElmJSONDecoder @a defaultOptions Aeson.defaultOptions s
elmTyp :: forall a. DeriveParameterisedElmTypeDefinition 0 a => Elm.Qualified -> Maybe Elm.Definition
elmTyp s = Just $ deriveElmTypeDefinition @a defaultOptions s

-- link 'V2 Double' with Elm's Vec2
deriving instance SOP.Generic (V2 Double)
deriving instance SOP.HasDatatypeInfo (V2 Double)
deriving instance ToJSON (V2 Double)
deriving instance FromJSON (V2 Double)
instance HasElmEncoder Value (V2 Double) where
    elmEncoder = ElmExpr.Global $ Elm.Qualified ["Util"] "encodeVec2"
instance HasElmDecoder Value (V2 Double) where
    elmDecoder = ElmExpr.Global $ Elm.Qualified ["Util"] "decodeVec2"
instance HasElmType (V2 Double) where
    elmType = Elm.Global $ Elm.Qualified ["Math","Vector2"] "Vec2"


{- Util -}

textSym :: forall a. KnownSymbol a => Text
textSym = T.pack $ symbolVal $ Proxy @a

showT :: Show a => a -> Text
showT = T.pack . show
