{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad (
    server,
    serverExtWs,
    Monpad,
    runMonpad,
    ServerConfig (..),
    combineConfs,
    ClientID (..),
    Update (..),
    ServerUpdate (..),
    V2 (..),
    Unit (..),
    elm,
    test,
    testExt,
    defaultDhall,
    defaultSimple,
    module Layout
) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor
import Data.IORef
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Proxy
import Data.Semigroup.Monad
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Time.Clock.POSIX
import Data.Tuple.Extra hiding (first, second)
import GHC.Generics (Generic)
import GHC.IO.Encoding (setLocaleEncoding)
import Generic.Data (Generically (..))
import Generic.Functor
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Linear (V2 (..))
import Lucid
import Lucid.Base (makeAttribute)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection qualified as WS
import Optics hiding (Empty)
import Servant hiding (layout)
import Servant.API.WebSocket
import Servant.HTML.Lucid
import Streamly
import Streamly.Internal.Prelude qualified as SP
import System.IO
import Text.Pretty.Simple

import DhallHack
import Embed
import Layout
import Orphans.Elm ()
import Orphans.Generic ()
import Orphans.V2 ()
import Util
import Util.Util
import Util.Elm (Unit (Unit))
import Util.Elm qualified as Elm

newtype ClientID = ClientID Text
    deriving (Eq, Ord, Show)
    deriving newtype FromHttpApiData

-- | A message sent by a client.
data Update
    = ButtonUp ElementID
    | ButtonDown ElementID
    | StickMove ElementID (V2 Double) -- always a vector within the unit circle
    | SliderMove ElementID Double -- between 0 and 1
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (FromJSON, HasElmType, HasElmEncoder J.Value) via Elm.Via Update

data ServerUpdate a b
    = SetImageURL ElementID Text
    | PlayAudioURL Text
    | Vibrate [Int]
    -- ^ millisecond intervals: https://developer.mozilla.org/en-US/docs/Web/API/Vibration_API#vibration_patterns
    | SetText ElementID Text
    | SetLayout (Layout a b)
    | SwitchLayout LayoutID
    | AddElement (FullElement a b)
    | RemoveElement ElementID
    | SetBackgroundColour Colour
    | SetIndicatorHollowness ElementID Double
    | SetIndicatorArcStart ElementID Double
    | SetIndicatorArcEnd ElementID Double
    | SetIndicatorShape ElementID Shape
    | SetIndicatorCentre ElementID (V2 Double)
    | SetIndicatorColour ElementID Colour
    | SetSliderPosition ElementID Double
    | ResetLayout ResetLayout
    deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, Functor)
    deriving (HasElmType, HasElmDecoder J.Value) via Elm.Via2 ServerUpdate Unit Unit
    deriving (ToJSON) via Elm.Via2 ServerUpdate a b
    deriving (Bifunctor) via GenericBifunctor ServerUpdate

data ResetLayout
    = StateReset
    -- ^ just stick positions, buttons pressed, etc.
    | FullReset
    -- ^ return to the layout the program was initialised with (undo add/remove elements etc.)
    deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder J.Value) via Elm.Via ResetLayout

-- | The arguments with which the frontend is initialised.
data ElmFlags = ElmFlags
    { layouts :: Layouts Unit Unit
    , username :: Text
    }
    deriving (Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON, HasElmType, HasElmDecoder J.Value) via Elm.Via ElmFlags

type Root = "monpad"
type UsernameParam = "username"
type AssetsApi = Raw
type CoreApi = QueryParam UsernameParam ClientID :> Get '[HTML] (Html ())
type HttpApi = Root :> (CoreApi :<|> AssetsApi)
type WsApi = QueryParam UsernameParam ClientID :> WebSocketPending

serverAddress :: Port -> IO Text
serverAddress port = do
    addr <- maybe "localhost" showHostAddress <$> getLocalIp
    pure $ "http://" <> addr <> ":" <> showT port <> "/" <> symbolValT @Root

loginHtml :: Maybe Text -> Html ()
loginHtml imageUrl = doctypehtml_ . body_ imageStyle . form_ [action_ $ symbolValT @Root] $ mconcat
    [ title_ "monpad: login"
    , style_ (commonCSS ())
    , style_ (loginCSS ())
    , label_ [for_ nameBoxId] "Username:"
    , br_ []
    , input_ [type_ "text", id_ nameBoxId, name_ $ symbolValT @UsernameParam]
    , input_ [type_ "submit", value_ "Go!"]
    ]
  where
    nameBoxId = "name"
    imageStyle = maybe [] (pure . style_ . ("background-image: url(" <>) . (<> ")")) imageUrl

mainHtml :: Layouts a b -> Port -> ClientID -> Html ()
mainHtml layouts wsPort (ClientID username) = doctypehtml_ $ mconcat
    [ style_ (commonCSS ())
    , style_ (appCSS ())
    , script_ [type_ jsScript] (elmJS ())
    , script_
        [ type_ jsScript
        , makeAttribute "layouts" . TL.toStrict . encodeToLazyText $
            bimap (const Unit) (const Unit) <$> layouts
        , makeAttribute "wsPort" $ showT wsPort
        , makeAttribute "username" username
        ]
        (jsJS ())
    ]
  where
    jsScript = "text/javascript"

--TODO use dedicated record types for State and Reader, and expose a cleaner interface
-- | The Monpad monad
newtype Monpad e s a b x = Monpad
    {unMonpad :: ReaderT (Map LayoutID (Layout a b), e, ClientID) (StateT (Layout a b, ElementMaps a b, s) IO) x}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Map LayoutID (Layout a b), e, ClientID)
        , MonadState (Layout a b, ElementMaps a b, s)
        )
deriving via Action (Monpad e s a b) instance {-# OVERLAPS #-} (Semigroup (Monpad e s a b ()))
deriving via Action (Monpad e s a b) instance {-# OVERLAPS #-} (Monoid (Monpad e s a b ()))
instance {-# OVERLAPPABLE #-} Semigroup x => (Semigroup (Monpad e s a b x)) where
    x <> y = (<>) <$> x <*> y
instance {-# OVERLAPPABLE #-} Monoid x => (Monoid (Monpad e s a b x)) where
    mempty = mempty @x <$ mempty @(Monpad e s a b ())
runMonpad :: Layouts a b -> ClientID -> e -> s -> Monpad e s a b x -> IO x
runMonpad ls c e s mon = evalStateT
    (runReaderT
        (unMonpad mon)
        (Map.fromList . NE.toList $ ((.name) &&& id) <$> ls, e, c)
    )
    (l, mkElementMaps l.elements, s)
  where l = NE.head ls
data MonpadException = WebSocketException WS.ConnectionException | UpdateDecodeException String
    deriving (Eq, Show)

{-TODO impredicative types should allow us to use a forall for the stream type
getting rid of some 'asyncly', 'serially' boilerplate
-}
-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s a b = ServerConfig
    { onStart :: Text -> IO () -- takes url
    , onNewConnection :: ClientID -> IO (e, s, [ServerUpdate a b])
    , onMessage :: Update -> Monpad e s a b [ServerUpdate a b]
    , onAxis :: a -> Double -> Monpad e s a b [ServerUpdate a b]
    -- ^ the argument here always ranges from -1 to 1, even for sliders
    , onButton :: b -> Bool -> Monpad e s a b [ServerUpdate a b]
    , onDroppedConnection :: MonpadException -> Monpad e s a b [ServerUpdate a b]
    , onPong :: e -> NominalDiffTime -> IO ()
    -- ^ when the client sends a pong, this gives us the time since the corresponding ping
    , updates :: e -> Async (s -> [ServerUpdate a b])
    }
    deriving Generic
    deriving (Semigroup, Monoid) via Generically (ServerConfig e s a b)

-- | Run two `ServerConfig`s with different states and environments.
combineConfs :: ServerConfig e1 s1 a b -> ServerConfig e2 s2 a b -> ServerConfig (e1, e2) (s1, s2) a b
combineConfs sc1 sc2 = ServerConfig
    { onStart = pure (<>)
        <*> sc1.onStart
        <*> sc2.onStart
    , onNewConnection = pure (pure \(e1, s1, u1) (e2, s2, u2) -> ((e1, e2), (s1, s2), u1 ++ u2))
        <<*>> sc1.onNewConnection
        <<*>> sc2.onNewConnection
    , onMessage = pure f
        <*> sc1.onMessage
        <*> sc2.onMessage
    , onAxis = pure (pure f)
        <<*>> sc1.onAxis
        <<*>> sc2.onAxis
    , onButton = pure (pure f)
        <<*>> sc1.onButton
        <<*>> sc2.onButton
    , onDroppedConnection = pure f
        <*> sc1.onDroppedConnection
        <*> sc2.onDroppedConnection
    , onPong = \(e1, e2) -> pure (<>)
        <*> sc1.onPong e1
        <*> sc2.onPong e2
    , updates = \(e1, e2) ->
        ((. fst) <$> sc1.updates e1)
            <>
        ((. snd) <$> sc2.updates e2)
    }
  where
    f :: Monoid x => Monpad ex sx a b x -> Monpad ey sy a b x -> Monpad (ex, ey) (sx, sy) a b x
    f x y = do
        (ml, (ex, ey), c) <- ask
        (l0, me0, (sx, sy)) <- get
        let rx = runReaderT (unMonpad x) (ml, ex, c)
            ry = runReaderT (unMonpad y) (ml, ey, c)
        (ax, (l1, me1, sx')) <- liftIO $ runStateT rx (l0, me0, sx)
        (ay, (l2, me2, sy')) <- liftIO $ runStateT ry (l1, me1, sy)
        put (l2, me2, (sx', sy'))
        pure $ ax <> ay

-- | Maps of element names to axes and buttons.
data ElementMaps a b = ElementMaps
    { stickMap :: Map ElementID (a, a)
    , sliderMap :: Map ElementID a
    , buttonMap :: Map ElementID b
    }
    deriving (Show, Generic)

mkElementMaps :: Foldable t => t (FullElement a b) -> ElementMaps a b
mkElementMaps = foldl' (flip addToElementMaps) $ ElementMaps mempty mempty mempty

addToElementMaps :: FullElement a b -> ElementMaps a b -> ElementMaps a b
addToElementMaps e = case e.element of
    Stick s -> over #stickMap $ Map.insert e.name (s.stickDataX, s.stickDataY)
    Slider s -> over #sliderMap $ Map.insert e.name s.sliderData
    Button b -> over #buttonMap $ Map.insert e.name b.buttonData
    Indicator _ -> id
    Empty -> id

server :: Int -> Port -> Maybe Text -> Maybe FilePath -> Layouts a b -> ServerConfig e s a b -> IO ()
server pingFrequency port loginImage assetsDir layouts conf = do
    onStart conf =<< serverAddress port
    run port . serve (Proxy @(HttpApi :<|> WsApi)) $
        httpServer port loginImage assetsDir layouts :<|> websocketServer pingFrequency layouts conf

-- | Runs HTTP server only. Expected that an external websocket server will be run from another program.
serverExtWs ::
    -- | Callback to handle server address
    (Text -> IO ()) ->
    -- | HTTP port
    Port ->
    -- | WS port
    Port ->
    Maybe Text ->
    Maybe FilePath ->
    Layouts a b ->
    IO ()
serverExtWs onStart httpPort wsPort loginImage assetsDir layouts = do
    onStart =<< serverAddress httpPort
    run httpPort . serve (Proxy @HttpApi) $ httpServer wsPort loginImage assetsDir layouts

httpServer :: Port -> Maybe Text -> Maybe FilePath -> Layouts a b -> Server HttpApi
httpServer wsPort loginImage assetsDir layouts =
    (pure . maybe (loginHtml loginImage) (mainHtml layouts wsPort))
        :<|> maybe
            (pure $ const ($ responseLBS status404 [] "no asset directory specified"))
            serveDirectoryWebApp
            assetsDir

websocketServer :: Int -> Layouts a b -> ServerConfig e s a b -> Server WsApi
websocketServer pingFrequency layouts ServerConfig{..} mu pending0 = liftIO case mu of
    Nothing -> T.putStrLn ("Rejecting WS connection: " <> err) >> WS.rejectRequest pending0 (encodeUtf8 err)
      where err = "no username parameter"
    Just clientId -> do
        lastPing <- newIORef Nothing
        (e, s0, u0) <- onNewConnection clientId
        let onPing = writeIORef lastPing . Just =<< getPOSIXTime
            onPong' = readIORef lastPing >>= \case
                Nothing -> warn "pong before ping"
                Just t0 -> do
                    t1 <- getPOSIXTime
                    onPong e $ t1 - t0
            pending = pending0 & (#pendingOptions % #connectionOnPong) %~ (<> onPong')
        conn <- WS.acceptRequest pending
        let stream = asyncly $ (Left <$> SP.cons (const u0) (updates e))
                <> (Right <$> serially (SP.repeatM $ getUpdate conn))
            handleUpdates = sendUpdates conn . map (bimap (const Unit) (const Unit)) <=< traverse \update -> do
                s <- gets thd3
                case update of
                    SetLayout l -> put (l, mkElementMaps l.elements, s)
                    SwitchLayout i -> asks ((!? i) . fst3) >>= \case
                        Just l -> put (l, mkElementMaps l.elements, s)
                        Nothing -> warn $ "layout id not found: " <> i.unwrap
                    AddElement el -> modify . first $ addToElementMaps el
                    RemoveElement el -> modify . first $
                        over #stickMap (Map.delete el)
                            . over #sliderMap (Map.delete el)
                            . over #buttonMap (Map.delete el)
                    SetBackgroundColour{} -> mempty
                    SetImageURL{} -> mempty
                    PlayAudioURL{} -> mempty
                    Vibrate{} -> mempty
                    SetText{} -> mempty
                    SetIndicatorHollowness{} -> mempty
                    SetIndicatorArcStart{} -> mempty
                    SetIndicatorArcEnd{} -> mempty
                    SetIndicatorShape{} -> mempty
                    SetIndicatorCentre{} -> mempty
                    SetIndicatorColour{} -> mempty
                    SetSliderPosition{} -> mempty
                    ResetLayout{} -> mempty
                pure update
        WS.withPingThread conn pingFrequency onPing . runMonpad layouts clientId e s0 . SP.drain $
            flip SP.takeWhileM (SP.hoist liftIO stream) \case
                Left sus -> do
                    s <- gets thd3
                    handleUpdates $ sus s
                    pure True
                Right (Right u) -> do
                    ElementMaps{..} <- gets snd3
                    handleUpdates =<< onMessage u
                    handleUpdates =<< case u of
                        ButtonUp t -> lookup' buttonMap t $ flip onButton False
                        ButtonDown t -> lookup' buttonMap t $ flip onButton True
                        StickMove t (V2 x y) -> lookup' stickMap t \(x', y') -> onAxis x' x >> onAxis y' y
                        SliderMove t x -> lookup' sliderMap t $ flip onAxis $ x * 2 - 1
                    pure True
                  where
                    lookup' m t f = case m !? t of
                        Just b -> f b
                        Nothing -> warn ("element id not found: " <> t.unwrap) >> mempty
                Right (Left err) -> do
                    handleUpdates =<< onDroppedConnection err
                    pure False
      where
        sendUpdates conn = liftIO . WS.sendTextData conn . encode
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
elm pathToElm = Elm.writeDefs pathToElm $ mconcat
    [ Elm.decodedTypes @Update
    , Elm.decodedTypes @(V2 Double)
    , Elm.encodedTypes @(ServerUpdate () ())
    , Elm.encodedTypes @ResetLayout
    , Elm.encodedTypes @ElmFlags
    , Elm.encodedTypes @ViewBox
    , Elm.encodedTypes @Colour
    , Elm.encodedTypes @TextStyle
    , Elm.encodedTypes @(Layout () ())
    , Elm.encodedTypes @(FullElement () ())
    , Elm.encodedTypes @(Element () ())
    , Elm.encodedTypes @(Stick ())
    , Elm.encodedTypes @(Slider ())
    , Elm.encodedTypes @(Button ())
    , Elm.encodedTypes @Image
    , Elm.encodedTypes @TextBox
    , Elm.encodedTypes @Indicator
    , Elm.encodedTypes @Shape
    , Elm.encodedTypes @(V2 Int)
    , Elm.encodedTypes @Unit
    ]

-- 'runghc Build.hs assets' before using this
test :: IO ()
test = do
    setLocaleEncoding utf8
    layouts <- sequence $ defaultSimple :| []
    server
        30
        8000
        Nothing
        (Just "../dist/assets")
        layouts
        config
  where
    config = mempty
        { onStart = pPrint . ("started" :: Text,)
        , onNewConnection = \c -> do
            pPrint ("connected" :: Text, c)
            pure ((), (), [])
        , onMessage = \u -> do
            c <- asks thd3
            pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = True} (c, u)
            mempty
        , onDroppedConnection = \c -> pPrint ("disconnected" :: Text, c) >> mempty
        , onPong = const $ pPrint . ("pong" :: Text,)
        }
testExt :: IO ()
testExt = serverExtWs mempty 8000 8001 Nothing (Just "../dist/assets") =<< sequence (defaultSimple :| [])

{- Util -}

--TODO colours
warn :: MonadIO m => Text -> m ()
warn s = liftIO $ T.hPutStrLn stderr $ "Warning: " <> s
