{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad (
    server,
    Monpad,
    runMonpad,
    ServerConfig (..),
    ClientID (..),
    Update (..),
    V2 (..),
    elm,
    test,
    Layout,
    layoutFromDhall,
    allAxesAndButs,
    argParser,
) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Proxy
import Data.Semigroup.Monad
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IO.Encoding (utf8, setLocaleEncoding)
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Lens.Micro
import Linear
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets qualified as WS
import Options.Applicative
import Servant hiding (layout)
import Servant.HTML.Lucid
import System.FilePath
import Text.Pretty.Simple

--TODO shouldn't really use this in library code
import Data.Generics.Labels ()

import Embed
import Layout
import Orphans.V2 ()
import Util
import Util.Elm qualified as Elm

newtype ClientID = ClientID Text
    deriving (Eq, Ord, Show)

-- | A message sent by a client.
data Update
    = ButtonUp Text
    | ButtonDown Text
    | StickMove Text (V2 Double) -- always a vector within the unit circle
    | SliderMove Text Double -- abs <= 1
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, FromJSON)
    deriving (HasElmType, HasElmEncoder J.Value) via Elm.Via Update

-- | The arguments with which the frontend is initialised.
data ElmFlags = ElmFlags
    { layout :: Layout () ()
    , username :: Text
    }
    deriving (Show, Generic, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via Elm.Via ElmFlags

type Root = "monpad"
type UsernameParam = "username"
type API = Root :> QueryParam UsernameParam Text :> Get '[HTML] (Html ())

{- | We don't provide a proper type for args, since backends will want to define their own.
This function just contains the likely common ground.
-}
argParser :: Parser (Port, Text)
argParser = (,)
    <$> (option auto . mconcat)
        [ long "port"
        , short 'p'
        , metavar "INT"
        , value 8000
        , showDefault
        , help "Port number for the server to listen on"
        ]
    <*> (strOption . mconcat)
        [ long "layout-dhall"
        , short 'l'
        , metavar "EXPR"
        , value defaultDhall
        , help "Dhall expression to control layout of buttons etc."
        ]

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

mainHtml :: Port -> Text -> Html ()
mainHtml wsPort username = doctypehtml_ $ mconcat
    [ style_ (mainCSS ())
    , script_ [type_ jsScript] (elmJS ())
    , script_ [type_ jsScript, makeAttribute "username" username, makeAttribute "wsPort" $ showT wsPort] (jsJS ())
    ]
  where
    jsScript = "text/javascript"

-- | The Monpad monad
newtype Monpad e s a = Monpad {unMonpad :: StateT s (ReaderT (e, ClientID) IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (e, ClientID), MonadState s)
deriving via Action (Monpad e s) instance (Semigroup (Monpad e s ()))
deriving via Action (Monpad e s) instance (Monoid (Monpad e s ()))
runMonpad :: ClientID -> e -> s -> Monpad e s a -> IO a
runMonpad c e s mon = runReaderT (evalStateT (unMonpad mon) s) (e, c)
data MonpadException = WebSocketException WS.ConnectionException | UpdateDecodeException String
    deriving (Eq, Show)

-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s a b = ServerConfig
    { onStart :: IO ()
    , onNewConnection :: ClientID -> IO (Layout a b, e, s)
    , onMessage :: Update -> Monpad e s ()
    , onAxis :: a -> Double -> Monpad e s ()
    , onButton :: b -> Bool -> Monpad e s ()
    , onDroppedConnection :: MonpadException -> Monpad e s ()
    }

-- | Maps of element names to axes and buttons.
data ServerEnv a b = ServerEnv
    { stickMap :: Map Text (a, a)
    , sliderMap :: Map Text a
    , buttonMap :: Map Text b
    }
    deriving (Show, Generic)

mkServerEnv :: Foldable t => t (FullElement a b) -> ServerEnv a b
mkServerEnv = foldl' (flip addToEnv) $ ServerEnv mempty mempty mempty
  where
    addToEnv e = case e.element of
        Stick s -> over #stickMap $ Map.insert e.name (s.stickDataX, s.stickDataY)
        Slider s -> over #sliderMap $ Map.insert e.name s.sliderData
        Button b -> over #buttonMap $ Map.insert e.name b.buttonData

server :: Port -> ServerConfig e s a b -> IO ()
server port conf = do
    onStart conf
    run port $ websocketsOr wsOpts (websocketServer conf) httpServer
  where
    httpServer = serve (Proxy @API) $ pure . maybe loginHtml (mainHtml port)
    wsOpts = WS.defaultConnectionOptions

websocketServer :: ServerConfig e s a b -> WS.ServerApp
websocketServer
    ServerConfig{onNewConnection, onMessage, onDroppedConnection, onAxis, onButton}
    pending = do
        conn <- WS.acceptRequest pending
        clientId <- ClientID <$> WS.receiveData conn
        (layout, e, s0) <- onNewConnection clientId
        WS.sendTextData conn $ encodeToLazyText $ biVoid layout
        let ServerEnv{stickMap, sliderMap, buttonMap} = mkServerEnv $ elements layout
            update u = do
                onMessage u
                case u of
                    ButtonUp t -> onButton (buttonMap ! t) False
                    ButtonDown t -> onButton (buttonMap ! t) True
                    StickMove t (V2 x y) -> let (x', y') = stickMap ! t in onAxis x' x >> onAxis y' y
                    SliderMove t x -> onAxis (sliderMap ! t) x
        WS.withPingThread conn 30 mempty
            . runMonpad clientId e s0
            $ onDroppedConnection =<< untilLeft (mapRightM update =<< getUpdate conn)
      where
        getUpdate conn = liftIO $ try (WS.receiveData conn) <&> \case
            Left err -> Left $ WebSocketException err
            Right b -> first UpdateDecodeException $ eitherDecode b

{- | Auto generate Elm datatypes, encoders/decoders etc.
It's best to open this file in GHCI and run 'elm'.
We could make it externally executable and fully integrate with the build process, but there wouldn't be much point
since the kinds of changes we're likely to make which would require re-running this,
are likely to require manual changes to Elm code anyway.
e.g. if we added an extra case to 'Update', it would need to be handled in various Elm functions.
-}
elm :: IO ()
elm = Elm.writeDefs (".." </> "elm" </> "src") $ mconcat
    [ Elm.decodedTypes @Update
    , Elm.decodedTypes @(V2 Double)
    , Elm.encodedTypes @ElmFlags
    , Elm.encodedTypes @Colour
    , Elm.encodedTypes @(Layout () ())
    , Elm.encodedTypes @(FullElement () ())
    , Elm.encodedTypes @(Element () ())
    , Elm.encodedTypes @(Stick ())
    , Elm.encodedTypes @(Slider ())
    , Elm.encodedTypes @(Button ())
    , Elm.encodedTypes @Shape
    , Elm.encodedTypes @(V2 Int)
    ]

--TODO this is a workaround until we have something like https://github.com/dhall-lang/dhall-haskell/issues/1521
test :: IO ()
test = do
    setLocaleEncoding utf8
    server 8000 config
  where
    config = ServerConfig
        { onStart = putStrLn "started"
        , onNewConnection = \c -> do
            putStrLn "connected:"
            pPrint c
            layout <- layoutFromDhall @() @() $ voidLayout <> defaultDhall
            pure (layout, (), ())
        , onMessage = pPrint
        , onAxis = mempty
        , onButton = mempty
        , onDroppedConnection = \c -> liftIO $ putStrLn "disconnected:" >> pPrint c
        }
    voidLayout =
        "let E = ./../dhall/evdev.dhall \
        \let A = E.AbsAxis \
        \let B = E.Key \
        \in (./../dhall/monpad.dhall A B).mapLayout {} {} (λ(_ : A) → {=}) (λ(_ : B) → {=}) "
