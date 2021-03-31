-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Tuple.Extra hiding (first, second)
import GHC.IO.Encoding (setLocaleEncoding)
import System.IO
import Text.Pretty.Simple

import DhallHack
import Monpad
import Monpad.Plugins
import Orphans.Elm ()
import Orphans.Generic ()
import Orphans.V2 ()

test :: [Plugin () ()] -> IO ()
test ps = do
    setLocaleEncoding utf8
    layouts <- sequence $ defaultSimple :| []
    withPlugin
        ( server
            30
            8000
            Nothing
            (Just "../dist/assets")
            layouts
        )
        (plugins $ Plugin config :| ps)
  where
    config =
        mempty
            { onStart = pPrint . ("started" :: Text,)
            , onNewConnection = \c -> do
                pPrint ("connected" :: Text, c)
                pure ((), (), [])
            , onMessage = \u -> do
                c <- asks thd3
                pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg{outputOptionsCompact = True} (c, u)
                mempty
            , onDroppedConnection = \c -> pPrint ("disconnected" :: Text, c) >> mempty
            , onPong = const $ ([] <$) . pPrint . ("pong" :: Text,)
            }

testExt :: IO ()
testExt = serverExtWs mempty 8000 8001 Nothing (Just "../dist/assets") =<< sequence (defaultSimple :| [])
