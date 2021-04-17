-- | Stuff for quickly playing around in GHCI. Call 'runghc Build.hs assets' before using these.
module Test where

import Control.Monad.Reader
import Optics
import System.IO
import Text.Pretty.Simple

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.IO.Encoding (setLocaleEncoding)

import Monpad
import Monpad.Plugins

defaultSimple :: Text
defaultSimple = "(../dhall/lib/map-layout.dhall).void ../dhall/default.dhall"

test :: [Plugin () ()] -> [Text] -> IO ()
test ps ls = do
    setLocaleEncoding utf8
    Just layouts <- layoutsFromDhall $ defaultSimple :| ls
    withPlugin
        (plugins $ Plugin config : ps)
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
                c <- asks $ view #client
                pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg{outputOptionsCompact = True} (c, u)
                mempty
            , onDroppedConnection = curry $ const . (>> mempty) . pPrint . ("disconnected" :: Text,)
            , onPong = const $ ([] <$) . pPrint . ("pong" :: Text,)
            }

testExt :: IO ()
testExt = do
    Just layouts <- layoutsFromDhall @() @() (pure defaultSimple)
    serverExtWs mempty 8000 8001 Nothing (Just "../dist/assets") layouts
