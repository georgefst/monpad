{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad.Plugins.LayoutSwitcher (plugin) where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition
import Data.Tuple.Extra
import Streamly

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra qualified as NE
import Data.Stream.Infinite (Stream ((:>)))
import Data.Stream.Infinite qualified as Stream
import Streamly.Prelude qualified as SP

import Monpad
import Monpad.Plugins
import Monpad.Plugins.Logger qualified as L
import Monpad.Plugins.PingIndicator qualified as PI
import Monpad.Plugins.WatchLayout qualified as WL

plugin :: b -> NonEmpty (LayoutID, ViewBox) -> Plugin a b
plugin = Plugin .: switcher

type LayoutData = (LayoutID, ViewBox)

switcher :: b -> NonEmpty LayoutData -> ServerConfig (MVar ()) (Stream LayoutData) a b
switcher buttonData ls =
    let onNewConnection = const $ (,Stream.cycle ls) <$> newEmptyMVar
        elementId = ElementID "_internal_switcher_button"
        initialUpdate l vb =
            let ViewBox{..} = vb
                s = min w h `div` 4
                (location, size) =
                    ( V2 (x + s `div` 2) (y + s `div` 2)
                    , fromIntegral s
                    )
                square = Rectangle $ V2 size size
             in [ AddElement FullElement
                    { location
                    , name = elementId
                    , element = Button Button'
                        { colour = Colour 1 1 1 1
                        , shape = square
                        , buttonData
                        }
                    , text = Just TextBox
                        { text = "Layout"
                        , style = TextStyle (size `div` 5) (Colour 0 0 0 1) False False False
                        }
                    , image = Nothing
                    }
                , SetText elementId l.unwrap
                ]
     in ServerConfig
            { onNewConnection
            , updates = \m -> serially $ SP.cons (const $ uncurry initialUpdate $ NE.head ls) $ SP.repeatM do
                takeMVar m -- wait for a ButtonUp event
                pure \((l, vb) :> _) -> SwitchLayout l : initialUpdate l vb
            , onStart = mempty
            , onMessage = \case
                ButtonUp t | t == elementId -> do
                    modify $ third3 Stream.tail
                    m <- asks snd3
                    liftIO $ putMVar m ()
                _ -> mempty
            , onAxis = mempty
            , onButton = mempty
            , onDroppedConnection = mempty
            , onPong = mempty
            }
