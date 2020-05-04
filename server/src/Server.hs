-- {-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-} --TODO move to separate module
-- module Main (main,elm) where
module Server (
    elm,
    server,
    ServerConfig(..),
    Message(..),
    Update(..),
    Button(..),
    V2(..),
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson (ToJSON,FromJSON,Value)
import qualified Data.Aeson as Aeson
import           Data.Bool
import qualified Data.ByteString as BS
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
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.API.Verbs
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

--TODO various things to investigate here:
    -- why can't we use eg. 'PutNoContent'? (https://github.com/folq/servant-to-elm/issues/2)
    -- changing POST to PUT results in a CORS error
    -- why does returing Text only work if we say it's 'JSON' rather than 'PlainText'?
--TODO HTTP is probably a lot more expensive than we really want anyway - use websockets? WebRTC? QUIC?
    -- remember we have the advantage of communicating over LAN
    -- JSON could be a bit expensive as well for that matter...
-- type API = "update" :> Capture "id" Text :> ReqBody '[JSON] Update :> GetNoContent
type API = "update" :> Capture "id" Text :> ReqBody '[JSON] Update :> Verb 'POST 204 '[PlainText] NoContent
-- type API = "update" :> Capture "id" Text :> ReqBody '[JSON] Update :> Verb 'PUT 200 '[PlainText] Text
-- type API = "update" :> Capture "id" Text :> ReqBody '[JSON] Update :> Put '[PlainText] Text

--TODO newConnection, startup?
data ServerConfig = ServerConfig
    { onMessage :: Message -> IO ()
    , port :: Port
    }

server :: ServerConfig -> IO ()
server ServerConfig{onMessage,port} = do
    putStrLn $ "Running server on port " <> show port
    run port $ myCors $ serve (Proxy @API) handler
  where
    handler clientId message = do
        liftIO $ onMessage Message{clientId,message}
        return NoContent

-- from https://github.com/haskell-servant/servant-swagger/issues/45
-- TODO understand this more fully
-- | Allow Content-Type header with values other then allowed by simpleCors.
myCors :: Middleware
myCors = cors $ const $ Just policy
    where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }


{- Elm generation -}

elm :: FilePath -> IO ()
elm src =
    let definitions = Elm.simplifyDefinition <$>
            map (elmEndpointDefinition "Config.urlBase" ["Auto", "Endpoints"]) (elmEndpoints @API)
                <> jsonDefinitions @Button <> jsonDefinitions @Update <> jsonDefinitions @(V2 Double)
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
