{-# OPTIONS_GHC -Wno-orphans #-} --TODO move to separate module
module Server (
    elm,
    server,
    ServerConfig(..),
    defaultConfig,
    Message(..),
    Update(..),
    Button(..),
    V2(..),
) where

import           Control.Monad
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
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)
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
import           Servant
import           Servant.API.Verbs
import           Servant.HTML.Lucid
import           Servant.To.Elm
import           System.Directory
import           System.FilePath
import           Text.Pretty.Simple

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
    { clientId :: Text
    , message :: Update
    } deriving (Eq, Ord, Show)

type API = "gamepad" :> QueryParam "username" Text :> Get '[HTML] (Html ())

--TODO add styling
loginHtml :: Html ()
loginHtml = doctypehtml_ $ form_ [action_ "gamepad"] $
    label_ [for_ "name"] "Username:"
        <>
    br_ []
        <>
    input_ [type_ "text", id_ "name", name_ "username"]
        <>
    input_ [type_ "submit", value_ "Go!"]

--TODO investigate performance - is it expensive to reassemble the HTML for a new username?
-- mainHtml :: Monad m => StaticData -> Text -> HtmlT m ()
mainHtml :: ServerConfig a -> StaticData -> Text -> Html ()
mainHtml ServerConfig{wsAddress,wsPort} StaticData{elmJS,jsJS,mainCSS} username = doctypehtml_ $
    head_ (
        style_ mainCSS
            <>
        script_ [type_ "text/javascript"] elmJS
    )
        <>
    body_ mempty
        <>
    script_ [type_ "text/javascript", makeAttribute "username" username, makeAttribute "wsAddress" wsAddr] jsJS
  where
    wsAddr = T.pack $ "ws://" <> wsAddress <> ":" <> show wsPort

--TODO we want more straightforward paths - maybe bring elm project inside the haskell one...
    -- also make sure the directory doesn't contain anything we don't want to serve
    -- actually it might be cool to use file-embed and keep to a single executable (removing 'StaticData' type)
loadStaticData :: IO StaticData
loadStaticData = do
    elmJS <- T.readFile "../client/dist/elm.js"
    jsJS <- T.readFile "../client/manual/js.js"
    mainCSS <- T.readFile "../client/manual/main.css"
    return StaticData{elmJS,jsJS,mainCSS}

data StaticData = StaticData
    { elmJS :: Text
    , jsJS :: Text
    , mainCSS :: Text
    }

--TODO newConnection, startup, end?
    -- expand to full-blown State
--TODO stronger typing for addresses etc.
--TODO use command line args for most of these
data ServerConfig a = ServerConfig
    { onMessage :: Message -> a -> IO ()
    , onStart :: Text -> IO a --TODO newtype for ID
    , httpPort :: Port
    , wsPort :: Port
    , wsAddress :: String
    , wsPingTime :: Int
    }

defaultConfig :: ServerConfig ()
defaultConfig = ServerConfig
    { onMessage = \m () -> pPrint m
    , onStart = \clientId -> T.putStrLn $ "New client: " <> clientId
    , httpPort = 8080
    , wsPort = 8001
    , wsAddress = "localhost"
    , wsPingTime = 30
    }

--TODO security - currently we just trust the names
server :: ServerConfig a -> IO ()
server sc = httpServer sc `race_` websocketServer sc

--TODO reject when username is already in use
httpServer :: ServerConfig a -> IO ()
httpServer sc@ServerConfig{httpPort} = do
    sd <- loadStaticData
    let handleMain = return . mainHtml sc sd
        handleLogin = return loginHtml
    run httpPort $ serve (Proxy @API) $ maybe handleLogin handleMain

--TODO use warp rather than 'WS.runServer' (see jemima)
websocketServer :: ServerConfig a -> IO ()
websocketServer ServerConfig{onMessage,onStart,wsPort,wsAddress,wsPingTime} =
    WS.runServer wsAddress wsPort application
  where
    --TODO JSON is unnecessarily expensive - use binary once API is stable?
    application pending = do
        conn <- WS.acceptRequest pending
        clientId <- WS.receiveData conn --TODO we send this back and forth rather a lot...
        s <- onStart clientId
        WS.withPingThread conn wsPingTime (return ()) $ forever $ do
            msg <- Aeson.eitherDecode <$> WS.receiveData conn
            case msg of
                Left e -> pPrint e --TODO handle error
                Right message -> onMessage Message{clientId,message} s


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
