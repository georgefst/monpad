{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Monpad.Plugins.LayoutSwitcher (plugin) where

import Control.Monad.State
import Data.Tuple.Extra
import Optics hiding ((:>))
import Optics.State.Operators

import Data.List.NonEmpty.Extra qualified as NE
import Data.Stream.Infinite (Stream ((:>)))
import Data.Stream.Infinite qualified as Stream

import Monpad
import Monpad.Plugins

plugin :: b -> Plugin a b
plugin = Plugin . switcher @()

switcher :: Monoid e => b -> ServerConfig e (Stream (LayoutID, ViewBox)) a b
switcher buttonData =
    let onNewConnection = \(fmap (((.name) &&& (.viewBox)) . fst) -> ls) _ -> pure
            ( mempty
            , Stream.cycle ls
            , pure . uncurry addButton $ NE.head ls
            )
        elementId = ElementID "_internal_switcher_button"
        addButton l vb =
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
        onUpdateLayoutChange = onLayoutChange \l -> do
            modify . over #extra . Stream.dropWhile $ (/= l.name) . fst
            pure [addButton l.name l.viewBox]
        onUpdateButtonUp = \case
            ClientUpdate (ButtonUp t) | t == elementId -> do
                #extra %= Stream.tail
                (l, vb) :> _ <- use #extra
                pure [SwitchLayout l, addButton l vb]
            _ -> mempty
        onUpdate = onUpdateLayoutChange <> onUpdateButtonUp
     in ServerConfig
            { onNewConnection
            , onUpdate
            , updates = mempty
            , onStart = mempty
            , onDroppedConnection = mempty
            , onPong = mempty
            }
