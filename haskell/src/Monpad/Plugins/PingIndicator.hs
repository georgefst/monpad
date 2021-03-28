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
        textElementId = ElementID "_internal_ping_text"
        indicatorElementId = ElementID "_internal_ping_indicator"
        (location, size) =
            let ViewBox{..} = vb
                s = min w h `div` 4
             in ( V2 (x + w - s `div` 2) (y + h - s `div` 2)
                , s
                )
        square = Rectangle $ fromIntegral <$> V2 size size
        initialUpdate =
            [ AddElement $ FullElement
                { location
                , name = indicatorElementId
                , showName = Nothing
                , element = Indicator Indicator'
                    { hollowness = 0
                    , arcStart = 0
                    , arcEnd = 1
                    , centre = 0
                    , colour = Colour 1 1 1 1 -- white
                    , shape = square
                    }
                }
            , AddElement $ FullElement
                { location
                , name = textElementId
                , showName = Nothing
                , element = TextBox TextBox'
                    { text = "Ping"
                    , style = TextStyle 50 (Colour 0 0 0 1) False False False
                    }
                }
            ]
        updates m = SP.cons (map const initialUpdate) $ const <<$>> SP.repeatM do
            time <- takeMVar m
            let okPing = 1 / 10 -- time in seconds to map to 0.5 goodness
                scaleFactor = negate $ log 0.5 / okPing
                goodness = exp $ negate (realToFrac time) * scaleFactor -- in range (0, 1]
            pure
                [ SetText textElementId $ showT time
                , SetIndicatorColour indicatorElementId $ Colour (1 - goodness) goodness 0 1
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
