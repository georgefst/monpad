{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad.Plugins.LayoutSwitcher (plugin) where

import Control.Monad.State
import Data.Composition
import Data.Tuple.Extra
import Streamly

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra qualified as NE
import Data.Stream.Infinite (Stream ((:>)))
import Data.Stream.Infinite qualified as Stream

import Monpad
import Monpad.Plugins
import Monpad.Plugins.Logger qualified as L
import Monpad.Plugins.PingIndicator qualified as PI
import Monpad.Plugins.WatchLayout qualified as WL

plugin :: b -> NonEmpty (LayoutID, ViewBox) -> Plugin a b
plugin = Plugin .: switcher @()

type LayoutData = (LayoutID, ViewBox)

switcher :: Monoid e => b -> NonEmpty LayoutData -> ServerConfig e (Stream LayoutData) a b
switcher buttonData ls =
    let onNewConnection = const $ pure (mempty, Stream.cycle ls)
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
            , updates = const . serially . pure . const . uncurry initialUpdate $ NE.head ls
            , onStart = mempty
            , onMessage = \case
                ButtonUp t | t == elementId -> do
                    modify $ third3 Stream.tail
                    (l, vb) :> _ <- gets thd3
                    pure $ SwitchLayout l : initialUpdate l vb
                _ -> mempty
            , onAxis = mempty
            , onButton = mempty
            , onDroppedConnection = mempty
            , onPong = mempty
            }
