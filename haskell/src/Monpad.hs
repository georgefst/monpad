{-# LANGUAGE UndecidableInstances #-}

module Monpad (
    server,
    ServerConfig(..),
    defaultConfig,
    Args(..),
    defaultArgs,
    getCommandLineArgs,
    argParser,
    ClientID(..),
    Update(..),
    V2(..),
    elm,
    test,
) where

import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.Generics.Labels () --TODO shouldn't really use this in library code
import Data.HashMap.Strict qualified as HashMap
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Proxy
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.IO qualified as T
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Dhall (FromDhall)
import Dhall qualified as D
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Language.Elm.Pretty qualified as Elm
import Language.Elm.Simplification qualified as Elm
import Language.Haskell.To.Elm
import Lens.Micro
import Linear
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets qualified as WS
import Options.Applicative hiding (Success, Failure)
import Servant hiding (layout)
import Servant.HTML.Lucid
import System.Directory
import System.FilePath
import System.IO (stderr, hPutStrLn)
import Text.Pretty.Simple

import Embed
import Layout
import Util
import Util.Elm qualified as Elm
import Orphans.V2 ()

newtype ClientID = ClientID Text
    deriving newtype (Eq,Ord,Show,IsString)

data Update
    = ButtonUp Text
    | ButtonDown Text
    | StickMove Text (V2 Double) -- always a vector within the unit circle
    | SliderMove Text Double -- abs <= 1
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, FromJSON)
    deriving (HasElmType, HasElmEncoder J.Value) via Elm.Via Update

data ElmFlags = ElmFlags
    { layout :: Layout () ()
    , username :: Text
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via Elm.Via ElmFlags

type Root = "monpad"
type UsernameParam = "username"
type API = Root :> QueryParam UsernameParam Text :> Get '[HTML] (Html ())

loginHtml :: Html ()
loginHtml = doctypehtml_ $ form_ [action_ $ symbolValT @Root] $
    title_ "monpad: login"
        <>
    style_ (mainCSS ())
        <>
    label_ [for_ nameBoxId] "Username:"
        <>
    br_ []
        <>
    input_ [type_ "text", id_ nameBoxId, name_ $ symbolValT @UsernameParam]
        <>
    input_ [type_ "submit", value_ "Go!"]
  where
    nameBoxId = "name"

mainHtml :: ElmFlags -> Port -> Html ()
mainHtml flags wsPort = doctypehtml_ $
    style_ (mainCSS ())
        <>
    script_ [type_ jsScript] (elmJS ())
        <>
    script_ [type_ jsScript, makeAttribute "elmFlags" flagsEnc, makeAttribute "wsPort" $ showT wsPort] (jsJS ())
  where
    jsScript = "text/javascript"
    flagsEnc = TL.toStrict $ encodeToLazyText flags

defaultArgs :: Args
defaultArgs = Args
    { port = 8000
    , wsPingTime = 30
    , dhallLayout = defaultDhall
    }

data Args = Args
    { port :: Port
    , wsPingTime :: Int
    , dhallLayout :: Text
    }
    deriving (Show, Generic)

getCommandLineArgs :: Args -> IO Args
getCommandLineArgs def = execParser opts
  where
    opts = info (helper <*> argParser def) (fullDesc <> header "monpad")

argParser ::
    -- | defaults
    Args ->
    Parser Args
argParser Args{port, wsPingTime, dhallLayout} = Args
    <$> option auto
        (  long "port"
        <> short 'p'
        <> metavar "INT"
        <> value port
        <> showDefault
        <> help "Port number for the server to listen on" )
    <*> option auto
        (  long "ws-ping-time"
        <> help "Interval (in seconds) between pings to each websocket"
        <> value wsPingTime
        <> showDefault
        <> metavar "INT" )
    <*> strOption
        (  long "layout-dhall"
        <> short 'l'
        <> metavar "EXPR"
        <> value dhallLayout
        <> help "Dhall expression to control layout of buttons etc." )

-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s a b = ServerConfig
    { onStart :: IO ()
    , onNewConnection :: ClientID -> IO (e,s)
    , onMessage :: Update -> e -> s -> IO s
    , onAxis :: a -> Double -> e -> s -> IO ()
    , onButton :: b -> Bool -> e -> s -> IO ()
    , onDroppedConnection :: ClientID -> e -> s -> IO ()
    , args :: Args
    }

defaultConfig :: ServerConfig () () a b
defaultConfig = ServerConfig
    { onStart = putStrLn "Monpad server started"
    , onNewConnection = \(ClientID i) -> fmap ((),) $ T.putStrLn $ "New client: " <> i
    , onMessage = \m () () -> pPrint m
    , onAxis = \_ _ () () -> pure ()
    , onButton = \_ _ () () -> pure ()
    , onDroppedConnection = \(ClientID i) () () -> T.putStrLn $ "Client disconnected: " <> i
    , args = defaultArgs
    }

data ServerEnv a b = ServerEnv
    { stickMap :: Map Text (a,a)
    , sliderMap :: Map Text a
    , buttonMap :: Map Text b
    }
    deriving (Show, Generic)

mkServerEnv :: Foldable t => t (FullElement a b) -> ServerEnv a b
mkServerEnv = foldl' (flip addToEnv) $ ServerEnv mempty mempty mempty
  where
    addToEnv FullElement{name,element} = case element of
        Stick{stickDataX,stickDataY} -> over #stickMap $ Map.insert name (stickDataX, stickDataY)
        Slider{sliderData} -> over #sliderMap $ Map.insert name sliderData
        Button{buttonData} -> over #buttonMap $ Map.insert name buttonData

server :: (FromDhall a, FromDhall b) => ServerConfig e s a b -> IO ()
server conf@ServerConfig{onStart, args = Args{port, dhallLayout}} = do
    layout@Layout{elements} <- D.input D.auto dhallLayout
    onStart
    let handleMain username = return $ mainHtml ElmFlags{layout = biVoid layout, username} port
        handleLogin = return loginHtml
        backupApp = serve (Proxy @API) $ maybe handleLogin handleMain
    run port $ websocketsOr WS.defaultConnectionOptions (websocketServer (mkServerEnv elements) conf) backupApp

websocketServer :: ServerEnv a b -> ServerConfig e s a b -> WS.ServerApp
websocketServer
    ServerEnv {stickMap, sliderMap, buttonMap}
    ServerConfig{onNewConnection,onMessage,onDroppedConnection,onAxis,onButton,args=Args{wsPingTime}}
    pending = do
        conn <- WS.acceptRequest pending
        clientId <- ClientID <$> WS.receiveData conn
        bracket (onNewConnection clientId) (uncurry $ onDroppedConnection clientId) $ \(e,s0) ->
            WS.withPingThread conn wsPingTime (return ()) $ flip iterateM_ s0 $ \s ->
                (eitherDecode <$> WS.receiveData conn) >>= \case
                    Left err -> hPutStrLn stderr ("Could not decode update message from client:\n  " ++ err) >> return s
                    Right upd -> do
                        case upd of
                            ButtonUp t -> onButton (buttonMap ! t) False e s
                            ButtonDown t -> onButton (buttonMap ! t) True e s
                            StickMove t (V2 x y) -> let (x',y') = stickMap ! t in onAxis x' x e s >> onAxis y' y e s
                            SliderMove t x -> onAxis (sliderMap ! t) x e s
                        onMessage upd e s

{- | Auto generate Elm datatypes, encoders/decoders etc.
It's best to open this file in GHCI and run 'elm'.
We could make it externally executable and fully integrate with the build process, but there wouldn't be much point
since the kinds of changes we're likely to make which would require re-running this,
are likely to require manual changes to Elm code anyway.
e.g. if we added an extra case to 'Update', it would need to be handled in various Elm functions.
-}
elm :: FilePath -> IO ()
elm src =
    let definitions = Elm.simplifyDefinition
            <$> Elm.decodedTypes @Update
            <>  Elm.decodedTypes @(V2 Double)
            <>  Elm.encodedTypes @ElmFlags
            <>  Elm.encodedTypes @Colour
            <>  Elm.encodedTypes @(Layout () ())
            <>  Elm.encodedTypes @(FullElement () ())
            <>  Elm.encodedTypes @(Element () ())
            <>  Elm.encodedTypes @Shape
            <>  Elm.encodedTypes @(V2 Int)
        modules = Elm.modules definitions
        autoFull = src </> T.unpack Elm.autoDir
    in do
        createDirectoryIfMissing False autoFull
        mapM_ (removeFile . (autoFull </>)) =<< listDirectory autoFull
        forM_ (HashMap.toList modules) \(moduleName, contents) ->
            T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
                renderStrict $ layoutPretty defaultLayoutOptions contents

--TODO this is a workaround until we have something like https://github.com/dhall-lang/dhall-haskell/issues/1521
test :: IO ()
test = do
    server @() @() @() @() defaultConfig {args = over #dhallLayout (voidLayout <>) defaultArgs}
  where
    voidLayout =
        "let E = ./../dhall/evdev.dhall \
        \let A = E.AbsAxis \
        \let B = E.Key \
        \in (./../dhall/WG.dhall A B).mapLayout {} {} (λ(_ : A) → {=}) (λ(_ : B) → {=}) "
