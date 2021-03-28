module Monpad.Plugins.PingIndicator (plugin) where

import Data.Time
import Control.Concurrent
import Util.Util

import Streamly.Prelude qualified as SP

import Monpad
import Monpad.Plugins

plugin :: ViewBox -> Plugin a b
plugin vb = Plugin $ showPing @() vb

showPing :: Monoid s => ViewBox -> ServerConfig (MVar NominalDiffTime) s a b
showPing vb =
    let onNewConnection = const $ (,mempty) <$> newEmptyMVar
        onPong = putMVar
        elementId = ElementID "_internal_ping_indicator"
        (location, size) =
            let ViewBox{..} = vb
                s = min w h `div` 4
             in ( V2 (x + w - s `div` 2) (y + h - s `div` 2)
                , fromIntegral s
                )
        square = Rectangle $ V2 size size
        initialUpdate =
            [ AddElement $ FullElement
                { location
                , name = elementId
                , text = Just TextBox
                    { text = "Ping"
                    , style = TextStyle (size `div` 5) (Colour 0 0 0 1) False False False
                    }
                , image = Nothing
                , element = Indicator Indicator'
                    { hollowness = 0
                    , arcStart = 0
                    , arcEnd = 1
                    , centre = 0
                    , colour = Colour 1 1 1 1 -- white
                    , shape = square
                    }
                }
            ]
        updates m = SP.cons (const initialUpdate) $ const <$> SP.repeatM do
            time <- takeMVar m
            let okPing = 1 / 10 -- time in seconds to map to 0.5 goodness
                scaleFactor = negate $ log 0.5 / okPing
                goodness = exp $ negate (realToFrac time) * scaleFactor -- in range (0, 1]
            pure
                [ SetText elementId $ showT time
                , SetIndicatorColour elementId $ Colour (1 - goodness) goodness 0 1
                ]
     in ServerConfig
            { onNewConnection
            , onPong
            , updates
            , onStart = mempty
            , onMessage = mempty
            , onAxis = mempty
            , onButton = mempty
            , onDroppedConnection = mempty
            }
