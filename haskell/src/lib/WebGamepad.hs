--TODO work out why HLS Fourmolu fails on this file
{-# LANGUAGE UndecidableInstances #-}

module WebGamepad (
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
    printDhallLayoutType,
) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Loops
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.Either.Validation
import Data.Composition
import Data.HashMap.Strict qualified as HashMap
import Data.List
import Data.Maybe
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
import Dhall.Core qualified as D
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol)
import Generics.SOP qualified as SOP
import GHC.TypeLits (symbolVal)
import Language.Elm.Definition qualified as Elm
import Language.Elm.Name qualified as Elm
import Language.Elm.Pretty qualified as Elm
import Language.Elm.Simplification qualified as Elm
import Language.Haskell.To.Elm
    ( HasElmDecoder,
      HasElmEncoder (..),
      HasElmType (..),
      deriveElmJSONEncoder,
      deriveElmTypeDefinition,
      elmDecoderDefinition,
      jsonDefinitions,
    )
import Language.Haskell.To.Elm qualified as Elm
import Linear
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets qualified as WS
import Options.Applicative hiding (Success, Failure)
import Servant hiding (layout)
import Servant.HTML.Lucid
import System.Directory
import System.FilePath
import Text.Pretty.Simple
import Type.Reflection (Typeable, typeRep)

import Embed
import Orphans.V2 ()

newtype ClientID = ClientID Text
    deriving newtype (Eq,Ord,Show,IsString)

data Update
    = ButtonUp Text
    | ButtonDown Text
    | StickMove Text (V2 Double) -- always a vector within the unit circle
    deriving (Eq, Ord, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, FromJSON, ToJSON)
    deriving (HasElmType, HasElmEncoder J.Value) via ElmType Update

-- field names chosen to match 'elm-color's 'fromRgba'
data Colour = Colour
    { red :: Double
    , green :: Double
    , blue :: Double
    , alpha :: Double
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via ElmType Colour

data Layout = Layout
    { elements :: [FullElement]
    , grid :: V2 Int
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via ElmType Layout

data ElmFlags = ElmFlags
    { layout :: Layout
    , username :: Text
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via ElmType ElmFlags

data FullElement = FullElement
    { element :: Element
    , location :: V2 Int
    , name :: Text
    , showName :: Bool
    }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via ElmType FullElement

data Element
    = Stick
        { radius :: Int
        , range :: Int
        , stickColour :: Colour
        , backgroundColour :: Colour
        }
    | Button { button :: Button, colour :: Colour }
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via ElmType Element

data Button
    = Circle Int
    | Rectangle (V2 Int)
    deriving (Show, Generic, FromDhall, ToJSON, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (HasElmType, HasElmDecoder J.Value) via ElmType Button

type Root = "gamepad"
type UsernameParam = "username"
type API = Root :> QueryParam UsernameParam Text :> Get '[HTML] (Html ())

--TODO add styling
loginHtml :: Html ()
loginHtml = doctypehtml_ $ form_ [action_ $ symbolValT @Root] $
    title_ "Gamepad: login"
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

--TODO investigate performance - is it expensive to reassemble the HTML for a new username?
mainHtml :: ElmFlags -> Args -> Html ()
mainHtml flags Args{address,wsPort} = doctypehtml_ $
    style_ (mainCSS ())
        <>
    script_ [type_ jsScript] (elmJS ())
        <>
    script_ [type_ jsScript, makeAttribute "elmFlags" flagsEnc, makeAttribute "wsAddress" wsAddr] (jsJS ())
  where
    wsAddr = "ws://" <> T.pack address <> ":" <> showT wsPort
    jsScript = "text/javascript"
    flagsEnc = TL.toStrict $ encodeToLazyText flags

defaultArgs :: Args
defaultArgs = Args
    { httpPort = 8000
    , wsPort = 8001
    , address = "localhost"
    , wsPingTime = 30
    , dhallLayout = defaultDhall
    }

--TODO better name (perhaps this should be 'ServerConfig'...)
--TODO stronger typing for addresses etc.
data Args = Args
    { httpPort :: Port
    , wsPort :: Port
    , address :: String --TODO only affects WS, not HTTP (why do we only need config for the former?)
    , wsPingTime :: Int
    , dhallLayout :: Text
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
    <*> strOption
        (  long "layout-dhall"
        <> short 'l'
        <> metavar "EXPR"
        <> value dhallLayout
        <> help "Dhall expression to control layout of buttons etc." )
  where
    Args{httpPort,wsPort,address,wsPingTime,dhallLayout} = defaultArgs

-- | `e` is a fixed environment. 's' is an updateable state.
data ServerConfig e s = ServerConfig
    { onStart :: Args -> IO ()
    , onNewConnection :: ClientID -> IO (e,s)
    , onMessage :: Update -> e -> s -> IO s
    , onDroppedConnection :: ClientID -> e -> IO () --TODO take s? not easy due to 'bracket' etc...
    , getArgs :: IO Args
    }

defaultConfig :: ServerConfig () ()
defaultConfig = ServerConfig
    { onStart = \Args{httpPort,address} -> T.putStrLn $
        "Server started at: " <> T.pack address <> ":" <> showT httpPort <> "/" <> symbolValT @Root
    , onNewConnection = \(ClientID i) -> fmap ((),) $ T.putStrLn $ "New client: " <> i
    , onMessage = \m () () -> pPrint m
    , onDroppedConnection = \(ClientID i) () -> T.putStrLn $ "Client disconnected: " <> i
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
httpServer args@Args{httpPort,dhallLayout} = do
    let handleMain username = do
            layout <- liftIO $ D.input D.auto dhallLayout
            return $ mainHtml ElmFlags{..} args
        handleLogin = return loginHtml
    run httpPort $ serve (Proxy @API) $ maybe handleLogin handleMain

--TODO use warp rather than 'WS.runServer' (see jemima)
--TODO JSON is unnecessarily expensive - use binary once API is stable?
--TODO under normal circumstances, connections will end with a 'WS.ConnectionException'
    -- we may actually wish to respond to different errors differently
websocketServer :: Args -> ServerConfig e s -> IO ()
websocketServer Args{wsPort,address,wsPingTime} ServerConfig{onNewConnection,onMessage,onDroppedConnection} =
    WS.runServer address wsPort $ \pending -> do
        conn <- WS.acceptRequest pending
        clientId <- ClientID <$> WS.receiveData conn --TODO we send this back and forth rather a lot...
        bracket (onNewConnection clientId) (onDroppedConnection clientId . fst) $ \(e,s0) ->
            --TODO somehow errors aren't shown - e.g. in linux exe, 'toKey = undefined' fails silently
            WS.withPingThread conn wsPingTime (return ()) $ flip iterateM_ s0 $ \s ->
                (eitherDecode <$> WS.receiveData conn) >>= \case
                    Left err -> pPrint err >> return s --TODO handle error
                    Right upd -> onMessage upd e s


{- Elm -}

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
            <$> decodedTypes @Update
            <>  decodedTypes @(V2 Double)
            <>  encodedTypes @ElmFlags
            <>  encodedTypes @Colour
            <>  encodedTypes @Layout
            <>  encodedTypes @FullElement
            <>  encodedTypes @Element
            <>  encodedTypes @Button
            <>  encodedTypes @(V2 Int)
        modules = Elm.modules definitions
        autoFull = src </> T.unpack elmAutoDir
    in do
        createDirectoryIfMissing False autoFull
        mapM_ (removeFile . (autoFull </>)) =<< listDirectory autoFull
        forM_ (HashMap.toList modules) \(moduleName, contents) ->
            T.writeFile (src </> joinPath (map T.unpack moduleName) <.> "elm") $
                renderStrict $ layoutPretty defaultLayoutOptions contents

-- | A type to derive via.
newtype ElmType a = ElmType a
instance (Generic a, J.GToJSON J.Zero (Rep a), Typeable a) => J.ToJSON (ElmType a) where
    toJSON (ElmType a) = J.genericToJSON jsonOptions a
instance (Generic a, J.GFromJSON J.Zero (Rep a), Typeable a) => J.FromJSON (ElmType a) where
    parseJSON = fmap ElmType . J.genericParseJSON jsonOptions
instance (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a), Typeable a) =>
    HasElmType (ElmType a) where
        elmDefinition =
            Just $ deriveElmTypeDefinition @a elmOptions $ Elm.Qualified [elmAutoDir, typeRepT @a] $ typeRepT @a
instance (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmEncoder J.Value) (SOP.Code a), HasElmType (ElmType a), Typeable a) =>
    HasElmEncoder J.Value (ElmType a) where
        elmEncoderDefinition =
            Just $ deriveElmJSONEncoder @a elmOptions jsonOptions $ Elm.Qualified [elmAutoDir, typeRepT @a] "encode"
instance (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmDecoder J.Value) (SOP.Code a), HasElmType (ElmType a), Typeable a) =>
    HasElmDecoder J.Value (ElmType a) where
        elmDecoderDefinition =
            Just $ Elm.deriveElmJSONDecoder @a elmOptions jsonOptions $ Elm.Qualified [elmAutoDir, typeRepT @a] "decode"

elmOptions :: Elm.Options
elmOptions = Elm.defaultOptions

jsonOptions :: J.Options
jsonOptions = J.defaultOptions

elmAutoDir :: Text
elmAutoDir = "Auto"


{- Util -}

printDhallLayoutType :: IO ()
printDhallLayoutType = case D.expected (D.auto @Layout) of
    Success e -> T.putStrLn $ D.pretty $ D.normalize e
    Failure x -> print x

symbolValT :: forall a. KnownSymbol a => Text
symbolValT = T.pack $ symbolVal $ Proxy @a

showT :: Show a => a -> Text
showT = T.pack . show

typeRepT :: forall a. Typeable a => Text
typeRepT = showT $ typeRep @a

-- | Like 'jsonDefinitions', but for types without decoders.
decodedTypes :: forall t. (HasElmEncoder J.Value t) => [Elm.Definition]
decodedTypes = catMaybes
    [ elmDefinition @t
    , elmEncoderDefinition @J.Value @t
    ]

-- | Like 'jsonDefinitions', but for types without encoders.
encodedTypes :: forall t. (HasElmDecoder J.Value t) => [Elm.Definition]
encodedTypes = catMaybes
    [ elmDefinition @t
    , elmDecoderDefinition @J.Value @t
    ]
