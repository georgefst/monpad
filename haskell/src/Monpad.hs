{-# LANGUAGE UndecidableInstances #-}
module Monpad (
    server,
    serverExtWs,
    LoginPageOpts(..),
    defaultLoginPageOpts,
    Monpad,
    runMonpad,
    MonpadEnv (..),
    MonpadState (..),
    Logger (..),
    Encoding (..),
    getCurrentLayout,
    ServerConfig (..),
    combineConfs,
    Client (..),
    showClient,
    ClientID (..),
    ElementHash(..),
    Update,
    Update' (..),
    ClientUpdate,
    ClientUpdate' (..),
    ServerUpdate (..),
    ResetLayout (..),
    V2 (..),
    ElmFlags (..),
    defaultDhall,
    warn,
    internalElementTag,
    module Layout,
    module ServerUpdate,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor
import Data.Binary.Get qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Colour (Colour)
import Data.Functor
import Data.Hash.Murmur
import Data.IORef
import Data.Int
import Data.List.Extra (chunksOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Time.Clock.POSIX
import Data.Traversable
import Data.Tuple.Extra hiding (first, second)
import Data.Word
import Deriving.Aeson (CustomJSON (CustomJSON))
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
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
import Streamly.Internal.Data.Stream.IsStream qualified as SP (hoist)
import System.Console.ANSI
import Streamly.Prelude qualified as SP
import System.IO
import Util.Util

import Embed
import Layout
import Opts qualified
import Orphans.Colour ()
import Orphans.Generic ()
import ServerUpdate
import Util

data Client = Client
    { id :: ClientID
    , colour :: [Colour Float]
    }
    deriving (Eq, Show)
newtype ClientID = ClientID {unwrap :: Text}
    deriving (Eq, Ord, Show)
    deriving newtype FromHttpApiData
showClient :: Bool -> Client -> Text
showClient ansiColour client = applyWhen ansiColour (withTermCols client.colour) client.id.unwrap
  where
    coloured c = (<> setSGRCode []) . (setSGRCode [SetRGBColor Background c] <>)
    withTermCols cs = if nCols == 0 then id else
        T.pack . mconcat . map (concat . zipWith coloured cs . map pure) . chunksOf nCols . T.unpack
      where nCols = length cs
data UsernameError
    = EmptyUsername
    | DuplicateUsername
        Bool
        -- ^ is the user with this name fully connected? as opposed to being in the waiting set
        ClientID
    deriving (Show)
data Clients = Clients
    { connected :: TVar (Set ClientID)
    -- ^ full WS connection
    , waiting :: TVar (Set ClientID)
    -- ^ HTTP connection but not yet WS
    }
    deriving (Generic)

type Update a b = Update' a b ElementID
data Update' a b i
    = ClientUpdate (ClientUpdate' i)
    | ServerUpdate (ServerUpdate a b)
    deriving (Show, Functor, Foldable, Traversable)

-- | A message sent by a client.
type ClientUpdate = ClientUpdate' ElementID
data ClientUpdate' m
    = ButtonUp m
    | ButtonDown m
    | StickMove m (V2 Double) -- always a vector within the unit circle
    | SliderMove m Double -- between 0 and 1
    | InputBool m Bool
    | InputNumber m Int32
    | InputText m Text
    | SubmitInput m -- for number and text inputs
    | Pong Text
    -- ^ See 'ServerUpdate.Ping'.
    deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving (FromJSON) via CustomJSON Opts.JSON (ClientUpdate' m)

data Encoding
    = JSONEncoding
    | BinaryEncoding
    deriving (Show, Generic)
    deriving (ToJSON) via CustomJSON Opts.JSON Encoding

newtype ElementHash = ElementHash Word32
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Opts.JSON ElementHash
hashElementID :: ElementID -> ElementHash
hashElementID = ElementHash . fromIntegral . murmur3 0 . encodeUtf8 . (.unwrap)

decodeUpdate :: BSL.ByteString -> Either (BSL.ByteString, B.ByteOffset, String) (ClientUpdate' ElementHash)
decodeUpdate = second thd3 . B.runGetOrFail do
    B.getWord8 >>= \case
        0 -> ButtonUp <$> getElemHash
        1 -> ButtonDown <$> getElemHash
        2 -> StickMove <$> getElemHash <*> getVec
        3 -> SliderMove <$> getElemHash <*> B.getDoublele
        4 -> InputBool <$> getElemHash <*> getBool
        5 -> InputNumber <$> getElemHash <*> B.getInt32le
        6 -> InputText <$> getElemHash <*> getRemainingText
        7 -> SubmitInput <$> getElemHash
        8 -> Pong <$> getRemainingText
        _ -> fail "unknown constructor"
  where
    getElemHash = ElementHash <$> B.getWord32le
    getVec = V2 <$> B.getDoublele <*> B.getDoublele
    getBool = (/= 0) <$> B.getWord8
    getRemainingText = either (fail . show) pure . decodeUtf8' . BSL.toStrict =<< B.getRemainingLazyByteString

-- | The arguments with which the frontend is initialised.
data ElmFlags = ElmFlags
    { layouts :: NonEmpty (Layout () ())
    , username :: Text
    , encoding :: Encoding
    , supportsFullscreen :: Bool
    }
    deriving (Show, Generic)
    deriving (ToJSON) via CustomJSON Opts.JSON ElmFlags

type Root = "monpad"
type UsernameParam = "username"
type ColourParam = "colour"
type AssetsApi = Raw
type CoreApi = QueryParam UsernameParam ClientID :> Get '[HTML] (Html ())
type HttpApi = Root :> (CoreApi :<|> AssetsApi)
type WsApi = QueryParam UsernameParam ClientID :> QueryParams ColourParam (Colour Float) :> WebSocketPending

serverAddress :: Port -> IO Text
serverAddress port = do
    addr <- maybe "localhost" showHostAddress <$> getLocalIp
    pure $ "http://" <> addr <> ":" <> showT port <> "/" <> symbolValT @Root

data LoginPageOpts = LoginPageOpts
    { pageTitle :: Text
    , imageUrl :: Maybe Text
    , usernamePrompt :: Text
    , usernamePromptStyle :: Text
    , submitButtonStyle :: Text
    , submitButtonText :: Text
    , submitButtonTextStyle :: Text
    }
defaultLoginPageOpts :: LoginPageOpts
defaultLoginPageOpts = LoginPageOpts
    { pageTitle = "monpad: login"
    , imageUrl = Nothing
    , usernamePrompt = "Username:"
    , usernamePromptStyle = ""
    , submitButtonStyle = ""
    , submitButtonText = "Go!"
    , submitButtonTextStyle = ""
    }
loginHtml :: Int -> Maybe UsernameError -> LoginPageOpts -> Html ()
loginHtml nColours err opts = doctypehtml_ . body_ imageStyle . form_ [action_ $ symbolValT @Root] . mconcat $
    [ title_ $ fs opts.pageTitle
    , style_ (commonCSS ())
    , style_ (loginCSS ())
    , label_ [Html.for_ nameBoxId, style_ opts.usernamePromptStyle] $ fs opts.usernamePrompt
    , br_ []
    , input_ [type_ "text", id_ nameBoxId, name_ $ symbolValT @UsernameParam, style_ opts.submitButtonStyle]
    , input_ [type_ "submit", value_ opts.submitButtonText, style_ opts.submitButtonTextStyle]
    , br_ []
    ] <>
    [ div_ [ class_ "colours" ]
        $ (mconcat . mconcat . replicate nColours)
        [ input_ [ class_ "colour", type_ "color", name_ "colour"]
        ]
    ] <> case err of
        Nothing -> []
        Just e ->
            [ p_ [ style_ "color: red" ] case e of
                EmptyUsername ->
                    "Empty usernames are not allowed!"
                (DuplicateUsername fully (ClientID u)) ->
                    "The username " <> fs u <> " is already in use!"
                        <> if fully then mempty else " (though not fully connected)"
            ]
  where
    fs = fromString . T.unpack
    nameBoxId = "name"
    imageStyle = maybe [] (pure . style_ . ("background-image: url(" <>) . (<> ")")) opts.imageUrl

mainHtml :: Encoding -> Layouts () () -> Port -> Html ()
mainHtml encoding layouts wsPort = doctypehtml_ $ mconcat
    [ meta_ [name_ "viewport", content_ "initial-scale=1, maximum-scale=1"]
    , style_ (commonCSS ())
    , style_ (appCSS ())
    , script_ [type_ jsScript] (elmJS ())
    , script_
        [ type_ jsScript
        , makeAttribute "layouts" . TL.toStrict . encodeToLazyText $ fst <$> layouts
        , makeAttribute "encoding" . TL.toStrict . encodeToLazyText $ encoding
        , makeAttribute "wsPort" $ showT wsPort
        ]
        (jsJS ())
    ]
  where
    jsScript = "text/javascript"

--TODO expose a cleaner interface, rather than requiring use of overloaded-labels lenses?
-- | The Monpad monad
newtype Monpad e s a b x = Monpad
    { unwrap ::
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
    deriving (Semigroup, Monoid) via Ap (Monpad e s a b) x
data MonpadEnv e a b = MonpadEnv
    { client :: Client
    , initialLayouts :: Map LayoutID (Layout a b, Maybe DhallExpr)
    , extra :: e
    }
    deriving (Show, Generic)
data MonpadState s a b = MonpadState
    { layouts :: Map LayoutID (Layout a b, Map ElementHash ElementID)
    , layout :: LayoutID
    -- ^ this is always a key in 'layouts'
    , extra :: s
    }
    deriving (Show, Generic)
getCurrentLayout :: Monpad e s a b (Layout a b)
getCurrentLayout = gets $ fromMaybe currentLayoutError . fmap fst . view currentLayoutMaybe
runMonpad :: Layouts a b -> Client -> e -> s -> Monpad e s a b x -> IO (Either MonpadException x)
runMonpad ls c e s mon = runExceptT $ evalStateT
    (runReaderT
        mon.unwrap
        (MonpadEnv c ((fst3 &&& snd3) <$> layoutMap) e)
    )
    (MonpadState ((fst3 &&& thd3) <$> layoutMap) (fst $ NE.head ls).name s)
  where
    layoutMap = Map.fromList . NE.toList $ ls <&> \(l, d) ->
        (l.name, (l, d, Map.fromList $ ((hashElementID &&& id) . (.name)) <$> l.elements))
data MonpadException
    = WebSocketException WS.ConnectionException
    | UpdateDecodeException BSL.ByteString B.ByteOffset String
    | JSONUpdateDecodeException String
    deriving (Eq, Show)

-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s a b = ServerConfig
    { onStart :: Text -> IO ()
    -- ^ do something with the URL, when the server starts
    , onNewConnection :: Layouts a b -> Client -> IO (e, s, [ServerUpdate a b])
    , onDroppedConnection :: MonpadException -> Client -> e -> IO ()
    , onPong :: NominalDiffTime -> Client -> e -> IO [ServerUpdate a b]
    -- ^ when the client sends a pong, this gives us the time since the corresponding ping
    , updates :: forall m. (SP.IsStream m, MonadIO (m IO)) => MonpadEnv e a b -> m IO [ServerUpdate a b]
    , onUpdate :: Update a b -> Monpad e s a b [ServerUpdate a b]
    -- ^ we need to be careful not to cause an infinite loop here, by always generating new events we need to respond to
    }
--TODO find some way to remove this boilerplate (we can't derive Generic because of the higher-rank type of `updates`)
instance (Semigroup e, Semigroup s) => Semigroup (ServerConfig e s a b) where
    sc1 <> sc2 = ServerConfig
        { onStart = sc1.onStart <> sc2.onStart
        , onNewConnection = sc1.onNewConnection <> sc2.onNewConnection
        , onDroppedConnection = sc1.onDroppedConnection <> sc2.onDroppedConnection
        , onPong = sc1.onPong <> sc2.onPong
        , updates = sc1.updates' <> sc2.updates'
        , onUpdate = sc1.onUpdate <> sc2.onUpdate
        }
instance (Monoid e, Monoid s) => Monoid (ServerConfig e s a b) where
    mempty = ServerConfig
        { onStart = mempty
        , onNewConnection = mempty
        , onDroppedConnection = mempty
        , onPong = mempty
        , updates = mempty
        , onUpdate = mempty
        }

-- | Run two `ServerConfig`s with different states and environments.
combineConfs :: ServerConfig e1 s1 a b -> ServerConfig e2 s2 a b -> ServerConfig (e1, e2) (s1, s2) a b
combineConfs sc1 sc2 = ServerConfig
    { onStart = pure (<>)
        <*> sc1.onStart
        <*> sc2.onStart
    , onNewConnection = pure (pure (pure \(e1, s1, u1) (e2, s2, u2) -> ((e1, e2), (s1, s2), u1 <> u2)))
        <<<*>>> sc1.onNewConnection
        <<<*>>> sc2.onNewConnection
    , onDroppedConnection = pure (pure \f1 f2 (e1, e2) -> f1 e1 <> f2 e2)
        <<*>> sc1.onDroppedConnection
        <<*>> sc2.onDroppedConnection
    , onPong = pure (pure \f1 f2 (e1, e2) -> (<>) <$> f1 e1 <*> f2 e2)
        <<*>> sc1.onPong
        <<*>> sc2.onPong
    , updates = \e ->
        sc1.updates' (over #extra fst e)
            <>
        sc2.updates' (over #extra snd e)
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

server ::
    Logger ->
    Int ->
    Encoding ->
    Port ->
    LoginPageOpts ->
    Int ->
    Maybe FilePath ->
    Layouts a b ->
    ServerConfig e s a b ->
    IO ()
server write pingFrequency encoding port loginOpts nColours assetsDir (uniqueNames (_1 % #name % coerced) -> layouts) conf = do
    clients <- Clients <$> newTVarIO Set.empty <*> newTVarIO Set.empty
    conf.onStart =<< serverAddress port
    run port . serve @(HttpApi :<|> WsApi) mempty $
        httpServer port loginOpts nColours assetsDir encoding (first biVoid <$> layouts) (Just clients)
            :<|> websocketServer write pingFrequency encoding layouts conf clients

-- | Runs HTTP server only. Expected that an external websocket server will be run from another program.
serverExtWs ::
    -- | Callback to handle server address
    (Text -> IO ()) ->
    Encoding ->
    -- | HTTP port
    Port ->
    -- | WS port
    Port ->
    LoginPageOpts ->
    Int ->
    Maybe FilePath ->
    Layouts () () ->
    IO ()
serverExtWs onStart encoding httpPort wsPort loginOpts nColours assetsDir layouts = do
    onStart =<< serverAddress httpPort
    run httpPort . serve @HttpApi mempty $ httpServer wsPort loginOpts nColours assetsDir encoding layouts clients
  where
    -- we can't detect duplicates when we don't control the websocket, since we don't know when a client disconnects
    clients = Nothing

httpServer :: Port -> LoginPageOpts -> Int -> Maybe FilePath -> Encoding -> Layouts () () -> Maybe Clients -> Server HttpApi
httpServer wsPort loginOpts nColours assetsDir encoding layouts mclients = core :<|> assets
  where
    core :: Server CoreApi
    core = \case
        Nothing -> pure $ loginHtml nColours Nothing loginOpts
        -- there is a username query param in the URL - validate it, and add to waiting list
        Just u -> liftIO $ atomically $
            either (\err -> loginHtml nColours (Just err) loginOpts) (\() -> mainHtml encoding layouts wsPort) <$> runExceptT do
                when (u == ClientID "") $ throwError EmptyUsername
                case mclients of
                    Just Clients{waiting, connected} -> do
                        alreadyConnected <- lift $ Set.member u <$> readTVar connected
                        when alreadyConnected $ throwError $ DuplicateUsername True u
                        isNew <- lift $ stateTVar waiting $ setInsert' u
                        unless isNew $ throwError $ DuplicateUsername False u
                    Nothing -> pure ()
    assets :: Server AssetsApi
    assets = maybe
        (pure $ const ($ responseLBS status404 [] "no asset directory specified"))
        serveDirectoryWebApp
        assetsDir

websocketServer ::
    forall e s a b.
    Logger ->
    Int ->
    Encoding ->
    Layouts a b ->
    ServerConfig e s a b ->
    Clients ->
    Server WsApi
websocketServer write pingFrequency encoding layouts ServerConfig{..} clients mu colour pending = liftIO case mu of
    Nothing -> rejectAndLog "no username parameter"
    Just clientId -> registerConnection clientId >>= \case
        False -> rejectAndLog $ "username not in the waiting set: " <> clientId.unwrap
        True -> do
            let client = Client{id = clientId, colour}
            lastPing <- newIORef Nothing
            (e, s0, u0) <- onNewConnection layouts client
            extraUpdates <- newEmptyMVar
            let onPing = writeIORef lastPing . Just =<< getPOSIXTime
                onPong' = readIORef lastPing >>= \case
                    Nothing -> warn "pong before ping"
                    Just t0 -> do
                        t1 <- getPOSIXTime
                        us <- onPong (t1 - t0) client e
                        putMVar extraUpdates us
            conn <- WS.acceptRequest $ pending & (#pendingOptions % #connectionOnPong) %~ (<> onPong')
            let {- We have to take care here not to attempt a parallel composition in a stateful monad.
                This is not supported by Streamly, but the types don't do anything to disallow it.
                See: https://github.com/composewell/streamly/issues/1203.
                We instead use an ad-hoc, simpler, monad stack, and only lift to `Monpad` once we're back to `Serial`.
                -}
                allUpdates = SP.fromAsync $
                    (pure . ClientUpdate <<$>> clientUpdates)
                        <> (pure . map ServerUpdate <$> serverUpdates)
                clientUpdates = SP.fromSerial . SP.hoist liftIO . SP.repeatM $ getUpdate conn
                serverUpdates = SP.cons u0 $
                    (SP.fromSerial . SP.hoist liftIO . updates =<< ask)
                        <> SP.repeatM (liftIO $ takeMVar extraUpdates)
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
                                    SetTextStyle i x ->
                                        (currentLayout % el i % #text % _Just % #style) .= x
                                    SetTextSize i x ->
                                        (currentLayout % el i % #text % _Just % #style % #size) .= x
                                    SetTextColour i x ->
                                        (currentLayout % el i % #text % _Just % #style % #colour) .= x
                                    SetTextBold i x ->
                                        (currentLayout % el i % #text % _Just % #style % #bold) .= x
                                    SetTextItalic i x ->
                                        (currentLayout % el i % #text % _Just % #style % #italic) .= x
                                    SetTextUnderline i x ->
                                        (currentLayout % el i % #text % _Just % #style % #underline) .= x
                                    SetTextShadow i x ->
                                        (currentLayout % el i % #text % _Just % #style % #shadow) .= x
                                    SetTextFont i x ->
                                        (currentLayout % el i % #text % _Just % #style % #font) .= x
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
                                    AddElement x -> do
                                        (currentLayout % #elements) %= (x :)
                                        currentLayoutHashes %= Map.insert (hashElementID x.name) x.name
                                    RemoveElement i -> do
                                        (currentLayout % #elements) %= filter ((/= i) . view #name)
                                        currentLayoutHashes %= Map.delete (hashElementID i)
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
                                            l <- asks $ maybe currentLayoutError fst . Map.lookup i
                                                . view #initialLayouts
                                            currentLayout .= l
                                    Ping _ -> mempty
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
                . (=<<)
                    ( either
                        ( \err -> do
                            onDroppedConnection err client e
                            atomically $ modifyTVar clients.connected $ Set.delete clientId
                        )
                        pure
                    )
                . runMonpad layouts client e s0
                . SP.drain
                . SP.mapM handleUpdates
                . SP.mapM
                    ( \us -> do
                        l <- use #layout
                        ls <- use #layouts
                        let (!_, hashes) = fromMaybe (currentLayoutError, error "impossible") $ ls !? l
                        pure $ either (fromMaybe (error "hash not found!") . (hashes !?)) id <<$>> us
                    )
                . SP.mapM (either throwError pure)
                . SP.hoist (\x -> liftIO . runReaderT x =<< ask)
                $ allUpdates
          where
            decode = case encoding of
                BinaryEncoding -> bimap (uncurry3 UpdateDecodeException) (fmap Left) . decodeUpdate
                JSONEncoding -> bimap JSONUpdateDecodeException (fmap Right) . eitherDecode
            sendUpdates conn = liftIO . WS.sendTextData conn . encode
            getUpdate conn =
                (decode <=< first WebSocketException)
                    <$> liftIO (try $ WS.receiveData conn)
  where
    rejectAndLog err = do
        write.log $ "Rejecting WS connection: " <> err
        WS.rejectRequest pending $ encodeUtf8 err
    -- attempt to move client from the waiting set to the connected set
    registerConnection clientId = atomically do
        success <- stateTVar clients.waiting $ setDelete' clientId
        when success do
            isNew <- stateTVar clients.connected $ setInsert' clientId
            unless isNew $ error $ "logic error - username in waiting and connected set: " <> show clientId
        pure success

--TODO colours
warn :: MonadIO m => Text -> m ()
warn s = liftIO $ T.hPutStrLn stderr $ "Warning: " <> s

--TODO come up with something more robust, and extensible
internalElementTag :: Text
internalElementTag = "_internal_"

{- Util -}

currentLayout :: AffineTraversal' (MonpadState s a b) (Layout a b)
currentLayout = currentLayoutMaybe % _Just % _1

currentLayoutHashes :: AffineTraversal' (MonpadState s a b) (Map ElementHash ElementID)
currentLayoutHashes = currentLayoutMaybe % _Just % _2

currentLayoutMaybe :: Lens' (MonpadState s a b) (Maybe (Layout a b, Map ElementHash ElementID))
currentLayoutMaybe = lens
    (\s -> view #layouts s !? view #layout s)
    (\s l -> s & over #layouts (maybe id (Map.insert $ view #layout s) l))

currentLayoutError :: Layout a b
currentLayoutError = error "current layout is not in map!"

-- TODO we can't have instances for impredicative types: https://gitlab.haskell.org/ghc/ghc/-/issues/20188
-- workaround from https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Key-examples#example-6-liberal-can-get-you-dysfunctional
-- TODO we can't even use the name "updates": https://gitlab.haskell.org/ghc/ghc/-/issues/18776
instance (SP.IsStream m, MonadIO (m IO), HasField "updates'" (ServerConfig e s a b) (MonpadEnv e a b -> m IO [ServerUpdate a b])) => HasField "updates'" (ServerConfig e s a b) (MonpadEnv e a b -> m IO [ServerUpdate a b]) where
    getField ServerConfig{updates} = updates
