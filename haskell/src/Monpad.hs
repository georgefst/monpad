{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad (
    server,
    serverExtWs,
    Monpad,
    runMonpad,
    MonpadEnv(..),
    MonpadState(..),
    getCurrentLayout,
    ServerConfig (..),
    combineConfs,
    ClientID (..),
    Update (..),
    ClientUpdate (..),
    ServerUpdate (..),
    ResetLayout(..),
    V2 (..),
    ElmFlags(..),
    defaultDhall,
    warn,
    module Layout,
    module ServerUpdate,
) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.IORef
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe
import Data.Proxy
import Data.Monoid
import Data.Semigroup.Monad
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Time.Clock.POSIX
import Data.Traversable
import Data.Tuple.Extra hiding (first, second)
import Deriving.Aeson (CustomJSON (CustomJSON))
import GHC.Generics (Generic)
import Generic.Data (Generically (..))
import Linear (V2 (..))
import Lucid hiding (for_)
import Lucid qualified as Html
import Lucid.Base (makeAttribute)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection qualified as WS
import Optics hiding (Empty)
import Optics.State.Operators
import Servant hiding (layout)
import Servant.API.WebSocket
import Servant.HTML.Lucid
import Streamly.Prelude qualified as SP
import Streamly.Internal.Data.Stream.IsStream qualified as SP
import System.IO
import Util.Util

import Embed
import Layout
import Opts qualified
import Orphans.Generic ()
import ServerUpdate
import Util

newtype ClientID = ClientID Text
    deriving (Eq, Ord, Show)
    deriving newtype FromHttpApiData

data Update a b
    = ClientUpdate ClientUpdate
    | ServerUpdate (ServerUpdate a b)
    deriving (Show)

-- | A message sent by a client.
data ClientUpdate
    = ButtonUp ElementID
    | ButtonDown ElementID
    | StickMove ElementID (V2 Double) -- always a vector within the unit circle
    | SliderMove ElementID Double -- between 0 and 1
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Opts.JSON ClientUpdate

-- | The arguments with which the frontend is initialised.
data ElmFlags = ElmFlags
    { layouts :: NonEmpty (Layout () ())
    , username :: Text
    }
    deriving (Show, Generic)
    deriving (ToJSON) via CustomJSON Opts.JSON ElmFlags

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

data UsernameError
    = EmptyUsername
loginHtml :: Maybe UsernameError -> Maybe Text -> Html ()
loginHtml err imageUrl = doctypehtml_ . body_ imageStyle . form_ [action_ $ symbolValT @Root] . mconcat $
    [ title_ "monpad: login"
    , style_ (commonCSS ())
    , style_ (loginCSS ())
    , label_ [Html.for_ nameBoxId] "Username:"
    , br_ []
    , input_ [type_ "text", id_ nameBoxId, name_ $ symbolValT @UsernameParam]
    , input_ [type_ "submit", value_ "Go!"]
    ] <> case err of
        Nothing -> []
        Just EmptyUsername ->
            [ p_
                [ style_ "color: red" ]
                "Empty usernames are not allowed!"
            ]
  where
    nameBoxId = "name"
    imageStyle = maybe [] (pure . style_ . ("background-image: url(" <>) . (<> ")")) imageUrl

mainHtml :: Layouts a b -> Port -> Html ()
mainHtml layouts wsPort = doctypehtml_ $ mconcat
    [ style_ (commonCSS ())
    , style_ (appCSS ())
    , script_ [type_ jsScript] (elmJS ())
    , script_
        [ type_ jsScript
        , makeAttribute "layouts" . TL.toStrict . encodeToLazyText $ biVoid . fst <$> layouts
        , makeAttribute "wsPort" $ showT wsPort
        ]
        (jsJS ())
    ]
  where
    jsScript = "text/javascript"

--TODO expose a cleaner interface, rather than requiring use of overloaded-labels lenses?
-- | The Monpad monad
newtype Monpad e s a b x = Monpad
    { unMonpad ::
        ReaderT
            (MonpadEnv e a b)
            (StateT
                (MonpadState s a b)
                (ExceptT MonpadException IO)
            )
            x
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadBase IO
        , MonadBaseControl IO
        , MonadReader (MonpadEnv e a b)
        , MonadState (MonpadState s a b)
        , MonadError MonpadException
        )
deriving via Mon (Monpad e s a b) x instance Semigroup x => (Semigroup (Monpad e s a b x))
deriving via Mon (Monpad e s a b) x instance Monoid x => (Monoid (Monpad e s a b x))
data MonpadEnv e a b = MonpadEnv
    { client :: ClientID
    , initialLayouts :: Map LayoutID (Layout a b, Maybe DhallExpr)
    , extra :: e
    }
    deriving (Show, Generic)
data MonpadState s a b = MonpadState
    { layouts :: Map LayoutID (Layout a b)
    , layout :: LayoutID
    -- ^ this is always a key in 'layouts'
    , extra :: s
    }
    deriving (Show, Generic)
getCurrentLayout :: Monpad e s a b (Layout a b)
getCurrentLayout = gets $ fromMaybe currentLayoutError . view currentLayoutMaybe
runMonpad :: Layouts a b -> ClientID -> e -> s -> Monpad e s a b x -> IO (Either MonpadException x)
runMonpad ls c e s mon = runExceptT $ evalStateT
    (runReaderT
        (unMonpad mon)
        (MonpadEnv c layoutMap e)
    )
    (MonpadState (fst <$> layoutMap) (fst $ NE.head ls).name s)
  where
    layoutMap = Map.fromList . NE.toList $ (((.name) . fst) &&& id) <$> ls
data MonpadException = WebSocketException WS.ConnectionException | UpdateDecodeException String
    deriving (Eq, Show)

{-TODO impredicative types should allow us to use a forall for the stream type
getting rid of some 'asyncly', 'serially' boilerplate
-}
-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s a b = ServerConfig
    { onStart :: Text -> IO ()
    -- ^ do something with the URL, when the server starts
    , onNewConnection :: Layouts a b -> ClientID -> IO (e, s, [ServerUpdate a b])
    , onDroppedConnection :: ClientID -> MonpadException -> e -> IO ()
    , onPong :: e -> NominalDiffTime -> IO (Endo s, [ServerUpdate a b])
    -- ^ when the client sends a pong, this gives us the time since the corresponding ping
    , updates :: MonpadEnv e a b -> SP.Async (MonpadState s a b -> [ServerUpdate a b])
    , onUpdate :: Update a b -> Monpad e s a b [ServerUpdate a b]
    -- ^ we need to be careful not to cause an infinite loop here, by always generating new events we need to respond to
    }
    deriving Generic
    deriving (Semigroup, Monoid) via Generically (ServerConfig e s a b)

-- | Run two `ServerConfig`s with different states and environments.
combineConfs :: ServerConfig e1 s1 a b -> ServerConfig e2 s2 a b -> ServerConfig (e1, e2) (s1, s2) a b
combineConfs sc1 sc2 = ServerConfig
    { onStart = pure (<>)
        <*> sc1.onStart
        <*> sc2.onStart
    , onNewConnection = pure (pure (pure \(e1, s1, u1) (e2, s2, u2) -> ((e1, e2), (s1, s2), u1 ++ u2)))
        <<<*>>> sc1.onNewConnection
        <<<*>>> sc2.onNewConnection
    , onDroppedConnection = pure (pure \f1 f2 (e1, e2) -> f1 e1 >> f2 e2)
        <<*>> sc1.onDroppedConnection
        <<*>> sc2.onDroppedConnection
    , onPong = \(e1, e2) t -> do
        (Endo sf1, u1) <- sc1.onPong e1 t
        (Endo sf2, u2) <- sc2.onPong e2 t
        pure (Endo $ sf1 *** sf2, u1 <> u2)
    , updates = \e ->
        ((. over #extra fst) <$> sc1.updates (over #extra fst e))
            <>
        ((. over #extra snd) <$> sc2.updates (over #extra snd e))
    , onUpdate = pure f
        <*> sc1.onUpdate
        <*> sc2.onUpdate
    }
  where
    f :: Monoid x => Monpad ex sx a b x -> Monpad ey sy a b x -> Monpad (ex, ey) (sx, sy) a b x
    f (Monpad x) (Monpad y) = Monpad $ ReaderT \e -> StateT \s0 -> do
        (rx, s1) <- runStateT (runReaderT x $ over #extra fst e) $ over #extra fst s0
        (ry, s2) <- runStateT (runReaderT y $ over #extra snd e) $ over #extra (const . snd $ view #extra s0) s1
        pure (rx <> ry, over #extra (view #extra s1,) s2)

server :: Int -> Port -> Maybe Text -> Maybe FilePath -> Layouts a b -> ServerConfig e s a b -> IO ()
server pingFrequency port loginImage assetsDir (uniqueNames (_1 % #name % coerced) -> layouts) conf = do
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
httpServer wsPort loginImage assetsDir layouts = core :<|> assets
  where
    core :: Server CoreApi
    core = \case
        Nothing -> pure $ loginHtml Nothing loginImage
        Just u -> -- there is a username query param in the URL
            if u == ClientID ""
                then pure $ loginHtml (Just EmptyUsername) loginImage
                else pure $ mainHtml layouts wsPort
    assets :: Server AssetsApi
    assets = maybe
        (pure $ const ($ responseLBS status404 [] "no asset directory specified"))
        serveDirectoryWebApp
        assetsDir

websocketServer :: Int -> Layouts a b -> ServerConfig e s a b -> Server WsApi
websocketServer pingFrequency layouts ServerConfig{..} mu pending0 = liftIO case mu of
    Nothing -> T.putStrLn ("Rejecting WS connection: " <> err) >> WS.rejectRequest pending0 (encodeUtf8 err)
      where err = "no username parameter"
    Just clientId -> do
        lastPing <- newIORef Nothing
        (e, s0, u0) <- onNewConnection layouts clientId
        extraUpdates <- newEmptyMVar
        extraModifys <- newEmptyMVar
        let onPing = writeIORef lastPing . Just =<< getPOSIXTime
            onPong' = readIORef lastPing >>= \case
                Nothing -> warn "pong before ping"
                Just t0 -> do
                    t1 <- getPOSIXTime
                    (Endo f, us) <- onPong e (t1 - t0)
                    putMVar extraUpdates us
                    putMVar extraModifys f
            pending = pending0 & (#pendingOptions % #connectionOnPong) %~ (<> onPong')
        conn <- WS.acceptRequest pending
        let allUpdates = SP.fromAsync $ (pure . ClientUpdate <$> clientUpdates) <> (ServerUpdate <<$>> serverUpdates)
            clientUpdates = SP.fromSerial . SP.repeatM $ getUpdate conn
            serverUpdates = ask >>= \env -> foldMap SP.fromSerial
                [ SP.cons u0 do
                    s <- get
                    f <- SP.hoist liftIO $ SP.fromAsync $ updates env
                    pure $ f s
                , SP.repeatM do
                    liftIO $ takeMVar extraUpdates
                , SP.repeatM do
                    modify . over #extra =<< liftIO (takeMVar extraModifys)
                    mempty
                ]
            handleUpdates = sendUpdates conn . map biVoid . concat <=< traverse (go . pure)
              where
                go us = if null us
                    then mempty
                    else fmap (sus ++) $ go . map ServerUpdate . concat =<< for us \u -> do
                        case u of
                            -- this needs to be equivalent to the same handling in the Elm code
                            ServerUpdate su -> case su of
                                PlayAudioURL{} -> mempty
                                Vibrate{} -> mempty
                                SetImageURL i x ->
                                    (currentLayout % el i % #image % _Just % #url) .= x
                                AddImage i x ->
                                    (currentLayout % el i % #image) .= Just x
                                DeleteImage i ->
                                    (currentLayout % el i % #image) .= Nothing
                                SetText i x ->
                                    (currentLayout % el i % #text % _Just % #text) .= x
                                AddText i x ->
                                    (currentLayout % el i % #text) .= Just x
                                DeleteText i ->
                                    (currentLayout % el i % #text) .= Nothing
                                SetLayout l ->
                                    currentLayout .= l
                                SwitchLayout i ->
                                    #layout .= i
                                HideElement i ->
                                    (currentLayout % el i % #hidden) .= True
                                ShowElement i ->
                                    (currentLayout % el i % #hidden) .= False
                                AddElement x ->
                                    (currentLayout % #elements) %= (x :)
                                RemoveElement i ->
                                    (currentLayout % #elements) %= filter ((/= i) . view #name)
                                SetBackgroundColour x ->
                                    (currentLayout % #backgroundColour) .= x
                                SetIndicatorHollowness i x ->
                                    (currentLayout % el i % #element % #_Indicator % #hollowness) .= x
                                SetIndicatorArcStart i x ->
                                    (currentLayout % el i % #element % #_Indicator % #arcStart) .= x
                                SetIndicatorArcEnd i x ->
                                    (currentLayout % el i % #element % #_Indicator % #arcEnd) .= x
                                SetIndicatorShape i x ->
                                    (currentLayout % el i % #element % #_Indicator % #shape) .= x
                                SetIndicatorCentre i x ->
                                    (currentLayout % el i % #element % #_Indicator % #centre) .= x
                                SetIndicatorColour i x ->
                                    (currentLayout % el i % #element % #_Indicator % #colour) .= x
                                SetSliderPosition{} -> mempty
                                SetButtonColour i x ->
                                    (currentLayout % el i % #element % #_Button % #colour) .= x
                                SetButtonPressed{} -> mempty
                                ResetLayout x -> case x of
                                    StateReset -> mempty -- this only affects the frontend
                                    FullReset -> do
                                        i <- gets $ view #layout
                                        l <- asks $ maybe currentLayoutError fst . Map.lookup i . view #initialLayouts
                                        currentLayout .= l
                            ClientUpdate _ -> mempty
                        onUpdate u
                  where
                    -- traverse all elements whose names match
                    el :: ElementID -> Traversal' (Layout a b) (FullElement a b)
                    el i = #elements % traversed % elMaybe i % _Just
                    elMaybe i = lens
                        (\x -> guard (view #name x == i) $> x)
                        (\x my -> fromMaybe x $ guard (view #name x == i) >> my)
                    sus = us & mapMaybe \case
                        ServerUpdate s -> Just s
                        ClientUpdate _ -> Nothing
        WS.withPingThread conn pingFrequency onPing
            . (=<<) (either (flip (onDroppedConnection clientId) e) pure)
            . runMonpad layouts clientId e s0
            . SP.drain
            $ SP.mapM handleUpdates allUpdates
      where
        sendUpdates conn = liftIO . WS.sendTextData conn . encode
        getUpdate conn = liftIO (try $ WS.receiveData conn) >>= \case
            Left err -> throwError $ WebSocketException err
            Right b -> case eitherDecode b of
                Left err -> throwError $ UpdateDecodeException err
                Right x -> pure x

--TODO colours
warn :: MonadIO m => Text -> m ()
warn s = liftIO $ T.hPutStrLn stderr $ "Warning: " <> s

{- Util -}

currentLayout :: AffineTraversal' (MonpadState s a b) (Layout a b)
currentLayout = currentLayoutMaybe % _Just

currentLayoutMaybe :: Lens' (MonpadState s a b) (Maybe (Layout a b))
currentLayoutMaybe = lens
    (\s -> view #layouts s !? view #layout s)
    (\s l -> s & over #layouts (maybe id (Map.insert $ view #layout s) l))

currentLayoutError :: Layout a b
currentLayoutError = error "current layout is not in map!"
