{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad.Plugins.LayoutSwitcher (plugin) where

import Control.Monad.State
import Data.Foldable
import Data.Tuple.Extra
import Util.Util

import Data.List.NonEmpty.Extra qualified as NE
import Data.Stream.Infinite (Stream ((:>)))
import Data.Stream.Infinite qualified as Stream
import Optics (over, view)

import Monpad
import Monpad.Plugins

plugin :: b -> Plugin a b
plugin = Plugin . switcher @()

switcher :: Monoid e => b -> ServerConfig e (Stream ((LayoutID, ViewBox), Bool)) a b
switcher buttonData =
    let onNewConnection = \(fmap ((.name) &&& (.viewBox)) -> ls) _ -> pure
            ( mempty
            , Stream.prepend
                (zip (toList ls) (repeat True))
                (Stream.cycle $ NE.zip ls (NE.repeat False))
            , pure . uncurry initialUpdate $ NE.head ls
            )
        elementId = ElementID "_internal_switcher_button"
        initialUpdate l vb =
            let ViewBox{..} = vb
                s = min w h `div` 4
                (location, size) =
                    ( V2 (x + fromIntegral s `div` 2) (y + fromIntegral s `div` 2)
                    , fromIntegral s
                    )
                square = Rectangle $ V2 size size
             in AddElement FullElement
                    { location
                    , name = elementId
                    , element = Button Button'
                        { colour = Colour 1 1 1 1
                        , shape = square
                        , buttonData
                        }
                    , text = Just TextBox
                        { text = l.unwrap
                        , style = TextStyle (size `div` 5) (Colour 0 0 0 1) False False False [] "sans-serif"
                        }
                    , image = Nothing
                    , hidden = False
                    }
     in ServerConfig
            { onNewConnection
            , updates = mempty
            , onStart = mempty
            , onUpdate = \case
                ClientUpdate (ButtonUp t) | t == elementId -> do
                    modify $ over #extra Stream.tail
                    ((l, vb), new) :> _ <- gets $ view #extra
                    pure $
                        applyWhen new (++ [initialUpdate l vb])
                        [SwitchLayout l]
                _ -> mempty
            , onDroppedConnection = mempty
            , onPong = mempty
            }
