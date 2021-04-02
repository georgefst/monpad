module Monpad.Plugins.PingIndicator (plugin) where

import Util.Util

import Data.Colour (Colour)
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.SRGB (sRGB, toSRGB)
import Data.Convertible (convert)
import Data.Prizm.Color as Prizm
import Data.Prizm.Color.CIE as CIE

import Monpad
import Monpad.Plugins

plugin :: ViewBox -> Plugin a b
plugin vb = Plugin $ showPing @() @() vb

showPing :: (Monoid e, Monoid s) => ViewBox -> ServerConfig e s a b
showPing vb =
    let onNewConnection = const $ pure (mempty, mempty, [initialUpdate])
        onPong = const \time -> pure
            let okPing = 1 / 10 -- time in seconds to map to 0.5 goodness
                goodness :: Double = 0.5 ** (realToFrac time / okPing) -- in range (0, 1]
                r = sRGB 0.85 0.28 0.28
                y = sRGB 0.94 0.95 0.33
                g = sRGB 0.2 0.72 0.2
            in  [ SetText elementId $ showT time
                , SetIndicatorColour elementId $ fromPrizm 1 if goodness < 0.5
                    then interpolate (round $ 2 * goodness * 100) (toPrizm r, toPrizm y)
                    else interpolate (round $ (2 * goodness - 1) * 100) (toPrizm y, toPrizm g)
                ]
        elementId = ElementID "_internal_ping_indicator"
        (location, size) =
            let ViewBox{..} = vb
                s = min w h `div` 4
             in ( V2 (x + w - s `div` 2) (y + h - s `div` 2)
                , fromIntegral s
                )
        square = Rectangle $ V2 size size
        initialUpdate = AddElement $ FullElement
            { location
            , name = elementId
            , text = Just TextBox
                { text = "Ping"
                , style = TextStyle (size `div` 5) (Colour 0 0 0 1) False False False "sans-serif"
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
     in ServerConfig
            { onNewConnection
            , onPong
            , updates = mempty
            , onStart = mempty
            , onMessage = mempty
            , onAxis = mempty
            , onButton = mempty
            , onDroppedConnection = mempty
            }

{- Util -}

toPrizm :: Data.Colour.Colour Double -> CIE.LCH
toPrizm = convert @Prizm.RGB . uncurryRGB mkRGB . fmap (round . (* 255)) . toSRGB

fromPrizm :: Double -> CIE.LCH -> Monpad.Colour
fromPrizm alpha = (\(ColorCoord (red, green, blue)) -> Colour{..}) .
    fmap ((/ 255) . fromIntegral) . unRGB . convert @_ @Prizm.RGB
