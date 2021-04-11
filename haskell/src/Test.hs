-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.IO.Encoding (setLocaleEncoding)
import Optics
import System.IO
import Text.Pretty.Simple

import Monpad
import Monpad.Plugins
import Orphans.Elm ()
import Orphans.Generic ()
import Orphans.V2 ()

defaultSimple :: Text
defaultSimple = "(../dhall/lib/map-layout.dhall).void ../dhall/default.dhall"

getLayouts :: IO (NonEmpty (Layout () ()))
getLayouts = layoutsFromDhall $ defaultSimple :| []

test :: [Plugin () ()] -> IO ()
test ps = do
    setLocaleEncoding utf8
    layouts <- getLayouts
    withPlugin
        (plugins $ Plugin config :| ps)
        $ server
            30
            8000
            Nothing
            (Just "../dist/assets")
            layouts
  where
    config =
        mempty
            { onStart = pPrint . ("started" :: Text,)
            , onNewConnection = \_ c -> do
                pPrint ("connected" :: Text, c)
                pure ((), (), [])
            , onUpdate = \u -> do
                c <- asks $ view #layouts
                pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg{outputOptionsCompact = True} (c, u)
                mempty
            , onDroppedConnection = curry $ const . (>> mempty) . pPrint . ("disconnected" :: Text,)
            , onPong = const $ ([] <$) . pPrint . ("pong" :: Text,)
            }

testExt :: IO ()
testExt = serverExtWs mempty 8000 8001 Nothing (Just "../dist/assets") =<< getLayouts
