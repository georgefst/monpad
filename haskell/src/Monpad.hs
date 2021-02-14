{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad (
    server,
    serverExtWs,
    Monpad,
    runMonpad,
    ServerConfig (..),
    ClientID (..),
    Update (..),
    ServerUpdate (..),
    V2 (..),
    Unit (..),
    elm,
    test,
    testExt,
    Layout,
    layoutFromDhall,
    defaultDhall,
    defaultSimple,
    allAxesAndButs,
) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.Aeson as J
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor
import Data.Composition
import Data.List
import Data.Map (Map, (!?))
import Data.Maybe
import qualified Data.Map as Map
import Data.Proxy
import Data.Semigroup.Monad
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Tuple.Extra (snd3, thd3)
import Generic.Data (Generically(..))
import GHC.Generics (Generic)
import GHC.IO.Encoding (setLocaleEncoding)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Lens.Micro
import Linear
import Lucid
import Lucid.Base (makeAttribute)
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Network.Wai
import qualified Network.WebSockets as WS
import Servant.API.WebSocket
import Servant hiding (layout)
import Servant.HTML.Lucid
import Streamly
import qualified Streamly.Prelude as SP
import qualified Streamly.Internal.Prelude as SP
import System.FilePath
import System.IO
import Text.Pretty.Simple

--TODO shouldn't really use this in library code
import Data.Generics.Labels ()

import DhallHack
import Embed
import Layout
import Orphans.V2 ()
import Util
import Util.Elm (Unit(Unit))
import qualified Util.Elm as Elm

newtype ClientID = ClientID Text
    deriving (Eq, Ord, Show)
    deriving newtype FromHttpApiData

-- | A message sent by a client.
data Update
    = ButtonUp Text
    | ButtonDown Text
    | StickMove Text (V2 Double) -- always a vector within the unit circle
    | SliderMove Text Double -- abs <= 1
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, FromJSON, HasElmType, HasElmEncoder J.Value) via Elm.Via Update

data ServerUpdate a b
    = SetImageURL Text Text
    | SetLayout (Layout a b)
    | AddElement (FullElement a b)
    | RemoveElement Text
    deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via Elm.Via2 ServerUpdate
deriving via (Elm.Via2 ServerUpdate) instance ToJSON (ServerUpdate Unit Unit)
instance Bifunctor ServerUpdate where
    bimap f g = \case
       SetLayout l -> SetLayout $ bimap f g l
       AddElement e -> AddElement $ bimap f g e
       SetImageURL i u -> SetImageURL i u
       RemoveElement i -> RemoveElement i

-- | The arguments with which the frontend is initialised.
data ElmFlags = ElmFlags
    { layout :: Layout Unit Unit
    , username :: Text
    }
    deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder J.Value) via Elm.Via ElmFlags

type Root = "monpad"
type UsernameParam = "username"
type ImageApi = "images" :> Raw
type CoreApi = QueryParam UsernameParam ClientID :> Get '[HTML] (Html ())
type HttpApi = Root :> (CoreApi :<|> ImageApi)
type WsApi = QueryParam UsernameParam ClientID :> WebSocketPending

loginHtml :: Html ()
loginHtml = doctypehtml_ . form_ [action_ $ symbolValT @Root] $ mconcat
    [ title_ "monpad: login"
    , style_ (mainCSS ())
    , label_ [for_ nameBoxId] "Username:"
    , br_ []
    , input_ [type_ "text", id_ nameBoxId, name_ $ symbolValT @UsernameParam]
    , input_ [type_ "submit", value_ "Go!"]
    ]
  where
    nameBoxId = "name"

mainHtml :: Layout a b -> Port -> ClientID -> Html ()
mainHtml layout wsPort (ClientID username) = doctypehtml_ $ mconcat
    [ style_ (mainCSS ())
    , script_ [type_ jsScript] (elmJS ())
    , script_
        [ type_ jsScript
        , makeAttribute "layout" . TL.toStrict $ encodeToLazyText $ bimap (const Unit) (const Unit) layout
        , makeAttribute "wsPort" $ showT wsPort
        , makeAttribute "username" username
        ]
        (jsJS ())
    ]
  where
    jsScript = "text/javascript"

--TODO use dedicated record types for State and Reader, and expose a cleaner interface
-- | The Monpad monad
newtype Monpad e s a b x = Monpad {unMonpad :: StateT (Layout a b, ElementMaps a b, s) (ReaderT (e, ClientID) IO) x}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (e, ClientID), MonadState (Layout a b, ElementMaps a b, s))
deriving via Action (Monpad e s a b) instance (Semigroup (Monpad e s a b ()))
deriving via Action (Monpad e s a b) instance (Monoid (Monpad e s a b ()))
runMonpad :: Layout a b -> ClientID -> e -> s -> Monpad e s a b x -> IO x
runMonpad l c e s mon = runReaderT (evalStateT (unMonpad mon) (l, mkElementMaps l.elements, s)) (e, c)
data MonpadException = WebSocketException WS.ConnectionException | UpdateDecodeException String
    deriving (Eq, Show)

{-TODO impredicative types should allow us to use a forall for the stream type
getting rid of some 'asyncly', 'serially' boilerplate
-}
-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s a b = ServerConfig
    { onStart :: Text -> IO () -- takes url
    , onNewConnection :: ClientID -> IO (e, s)
    , onMessage :: Update -> Monpad e s a b ()
    , onAxis :: a -> Double -> Monpad e s a b ()
    , onButton :: b -> Bool -> Monpad e s a b ()
    , onDroppedConnection :: MonpadException -> Monpad e s a b ()
    , updates :: Async (e -> s -> ServerUpdate a b)
    }
    deriving Generic
    deriving (Semigroup, Monoid) via Generically (ServerConfig e s a b)

-- | Maps of element names to axes and buttons.
data ElementMaps a b = ElementMaps
    { stickMap :: Map Text (a, a)
    , sliderMap :: Map Text a
    , buttonMap :: Map Text b
    }
    deriving (Show, Generic)

mkElementMaps :: Foldable t => t (FullElement a b) -> ElementMaps a b
mkElementMaps = foldl' (flip addToElementMaps) $ ElementMaps mempty mempty mempty

addToElementMaps :: FullElement a b -> ElementMaps a b -> ElementMaps a b
addToElementMaps e = case e.element of
    Stick s -> over #stickMap $ Map.insert e.name (s.stickDataX, s.stickDataY)
    Slider s -> over #sliderMap $ Map.insert e.name s.sliderData
    Button b -> over #buttonMap $ Map.insert e.name b.buttonData
    Image _ -> id

server :: Port -> Maybe FilePath -> Layout a b -> ServerConfig e s a b -> IO ()
server port imageDir layout conf = do
    addr <- fromMaybe (error "Couldn't get local ip") <$> getLocalIp
    onStart conf $ "http://" <> showHostAddress addr <> ":" <> showT port <> "/" <> symbolValT @Root
    run port . serve (Proxy @(HttpApi :<|> WsApi)) $
        httpServer port imageDir layout :<|> websocketServer layout conf

-- | Runs HTTP server only. Expected that an external websocket server will be run from another program.
serverExtWs ::
    -- | HTTP port
    Port ->
    -- | WS port
    Port ->
    Maybe FilePath ->
    Layout a b ->
    IO ()
serverExtWs httpPort = run httpPort . serve (Proxy @HttpApi) .:. httpServer

httpServer :: Port -> Maybe FilePath -> Layout a b -> Server HttpApi
httpServer wsPort imageDir layout =
    (pure . maybe loginHtml (mainHtml layout wsPort))
        :<|> maybe
            (pure $ const ($ responseLBS status404 [] "no image directory specified"))
            serveDirectoryWebApp
            imageDir

websocketServer :: Layout a b -> ServerConfig e s a b -> Server WsApi
websocketServer layout ServerConfig{..} mu pending = liftIO case mu of
    Nothing -> T.putStrLn ("Rejecting WS connection: " <> err) >> WS.rejectRequest pending (encodeUtf8 err)
      where err = "no username parameter"
    Just clientId -> do
        conn <- WS.acceptRequest pending
        (e, s0) <- onNewConnection clientId
        let stream = asyncly $ (Left <$> updates) <> (Right <$> serially (SP.repeatM $ getUpdate conn))
        WS.withPingThread conn 30 mempty . runMonpad layout clientId e s0 . SP.drain $
            flip SP.takeWhileM (SP.hoist liftIO stream) \case
                Left su -> do
                    s <- gets thd3
                    let update = su e s
                    case update of
                        SetImageURL _ _ -> pure ()
                        SetLayout l -> put (l, mkElementMaps l.elements, s)
                        AddElement el -> modify . first $ addToElementMaps el
                        RemoveElement el -> modify . first $
                            over #stickMap (Map.delete el)
                                . over #sliderMap (Map.delete el)
                                . over #buttonMap (Map.delete el)
                    sendUpdate conn (bimap (const Unit) (const Unit) update)
                    pure True
                Right (Right u) -> do
                    ElementMaps{..} <- gets snd3
                    onMessage u
                    case u of
                        ButtonUp t -> lookup' buttonMap t $ flip onButton False
                        ButtonDown t -> lookup' buttonMap t $ flip onButton True
                        StickMove t (V2 x y) -> lookup' stickMap t \(x', y') -> onAxis x' x >> onAxis y' y
                        SliderMove t x -> lookup' sliderMap t $ flip onAxis x
                    pure True
                  where
                    lookup' m t f = case m !? t of
                        Just b -> f b
                        Nothing -> liftIO $ T.hPutStrLn stderr $ "Warning: element id not found: " <> t
                Right (Left err) -> onDroppedConnection err >> pure False
      where
        sendUpdate conn = liftIO . WS.sendTextData conn . encode
        getUpdate conn = liftIO $ try (WS.receiveData conn) <&> \case
            Left err -> Left $ WebSocketException err
            Right b -> first UpdateDecodeException $ eitherDecode b

{- | Auto generate Elm datatypes, encoders/decoders etc.
It's best to run this via GHCI or HLS.
We could make it externally executable and fully integrate with the build process, but there wouldn't be much point
since the kinds of changes we're likely to make which would require re-running this,
are likely to require manual changes to Elm code anyway.
e.g. if we added an extra case to 'Update', it would need to be handled in various Elm functions.
-}
-- >>> elm "elm"
elm :: FilePath -> IO ()
elm pathToElm = Elm.writeDefs (pathToElm </> "src") $ mconcat
    [ Elm.decodedTypes @Update
    , Elm.decodedTypes @(V2 Double)
    , Elm.encodedTypes @(ServerUpdate () ())
    , Elm.encodedTypes @ElmFlags
    , Elm.encodedTypes @ViewBox
    , Elm.encodedTypes @Colour
    , Elm.encodedTypes @(Layout () ())
    , Elm.encodedTypes @(FullElement () ())
    , Elm.encodedTypes @(Element () ())
    , Elm.encodedTypes @(Stick ())
    , Elm.encodedTypes @(Slider ())
    , Elm.encodedTypes @(Button ())
    , Elm.encodedTypes @Image
    , Elm.encodedTypes @Shape
    , Elm.encodedTypes @(V2 Int)
    , Elm.encodedTypes @Unit
    ]

-- 'runghc Build.hs assets' before using this
test :: IO ()
test = do
    setLocaleEncoding utf8
    layout <- defaultSimple
    server 8000 (Just "../dist/images") layout config
  where
    config = ServerConfig
        { onStart = pPrint . ("started" :: Text,)
        , onNewConnection = \c -> do
            pPrint ("connected" :: Text, c)
            pure ((), ())
        , onMessage = \u -> do
            c <- asks snd
            pPrint (c, u)
        , onAxis = mempty
        , onButton = mempty
        , onDroppedConnection = \c -> pPrint ("disconnected" :: Text, c)
        , updates = mempty
        }
testExt :: IO ()
testExt = serverExtWs 8000 8001 (Just "../dist/images") =<< defaultSimple
