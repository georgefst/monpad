module Monpad.Plugins.PingIndicator (plugin) where

import Data.Colour (withOpacity)
import Data.Colour.Names qualified as Colours
import Data.Colour.SRGB (sRGB)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Streamly.Data.Stream.Prelude qualified as S

import Monpad hiding (min)
import Monpad.Plugins
import Util.Prizm

plugin :: Double -> Plugin a b
plugin scale = Plugin $ showPing @() @() scale

showPing :: (Monoid e, Monoid s) => Double -> ServerConfig e s a b
showPing scale =
    let onNewConnection layouts = const $ pure (mempty, mempty, [addIndicator . fst $ NE.head layouts])
        onPong time _ _ = pure
            let okPing = 1 / 10 -- time in seconds to map to 0.5 goodness
                goodness :: Double = 0.5 ** (realToFrac time / okPing) -- in range (0, 1]
                --TODO these values come from `monpad.dhall` - embed at compile time?
                r = sRGB 0.85 0.28 0.28
                y = sRGB 0.94 0.95 0.33
                g = sRGB 0.2 0.72 0.2
                colour = flip withOpacity 1 $ fromPrizm $ interpolateList goodness [r,y,g]
            in  [ SetText elementId $ T.pack $ formatTime defaultTimeLocale "%04Ess" time
                , SetIndicatorColour elementId colour
                ]
        elementId = ElementID $ internalElementTag  <> "ping_indicator"
        addIndicator Layout{..} =
            let (location, width, height) =
                    let ViewBox{..} = viewBox
                        s = round $ scale * fromIntegral (min w h) / 4
                        h' = s `div` 4
                     in ( V2 (x + fromIntegral (w - s `div` 2)) (y + fromIntegral (h - h' `div` 2))
                        , fromIntegral s
                        , fromIntegral h'
                        )
                square = Rectangle $ V2 width height
             in AddElement $ FullElement
                    { location
                    , name = elementId
                    , text = Just TextBox
                        { text = "Ping"
                        , style = TextStyle (width `div` 5) (withOpacity Colours.black 1) False False False [] 0 Centre "sans-serif"
                        , alignX = Centre
                        , alignY = Middle
                        }
                    , image = Nothing
                    , element = Indicator Indicator'
                        { hollowness = 0
                        , arcStart = 0
                        , arcEnd = 1
                        , centre = 0
                        , colour = withOpacity Colours.white 1
                        , shape = square
                        }
                    , hidden = False
                    }
        onUpdate = onLayoutChange $ pure . pure . addIndicator
     in ServerConfig
            { onNewConnection
            , onPong
            , onUpdate
            , updates = const S.nil
            , onStart = mempty
            , onDroppedConnection = mempty
            }
