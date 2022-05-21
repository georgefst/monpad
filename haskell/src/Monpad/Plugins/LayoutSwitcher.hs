module Monpad.Plugins.LayoutSwitcher (plugin) where

import Control.Monad.State
import Data.Tuple.Extra
import Optics
import Optics.State.Operators

import Data.Colour (withOpacity)
import Data.Colour.Names (black, white)
import Data.List.NonEmpty.Extra qualified as NE
import Data.Stream.Infinite (Stream)
import Data.Stream.Infinite qualified as Stream

import Monpad
import Monpad.Plugins
import Util

plugin :: Double -> b -> Plugin a b
plugin scale = Plugin . switcher @() scale

switcher :: Monoid e => Double -> b -> ServerConfig e (Stream (LayoutID, ViewBox)) a b
switcher scale buttonData =
    let onNewConnection = \(fmap (((.name) &&& (.viewBox)) . fst) -> ls) _ -> pure
            ( mempty
            , Stream.cycle ls
            , pure . uncurry addButton $ NE.head ls
            )
        elementId = ElementID $ internalElementTag  <> "switcher_button"
        addButton l vb =
            let ViewBox{..} = vb
                s = round $ scale * fromIntegral (min w h) / 4
                (location, size) =
                    ( V2 (x + s `div` 2) (y + s `div` 2)
                    , fromIntegral s
                    )
                square = Rectangle $ V2 size size
             in AddElement FullElement
                    { location
                    , name = elementId
                    , element = Button Button'
                        { colour = withOpacity black 1
                        , shape = square
                        , buttonData
                        }
                    , text = Just TextBox
                        { text = l.unwrap
                        , style = TextStyle (size `div` 5) (withOpacity white 1) False False False [] "sans-serif"
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
                l <- fst . streamHead <$> use #extra
                pure [SwitchLayout l]
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
