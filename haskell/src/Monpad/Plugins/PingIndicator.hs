module Monpad.Plugins.PingIndicator (plugin) where

import Data.Monoid
import Data.Time
import Util.Util

import Control.Monad.State (gets)
import Data.Colour qualified
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.SRGB (sRGB, toSRGB)
import Data.Convertible (convert)
import Data.List.NonEmpty qualified as NE
import Data.Prizm.Color as Prizm
import Data.Prizm.Color.CIE as CIE
import Optics (view)

import Monpad
import Monpad.Plugins

plugin :: Plugin a b
plugin = Plugin $ showPing @()

showPing :: (Monoid e) => ServerConfig e (NominalDiffTime, Colour) a b
showPing =
    let s0 = (0, Colour 1 1 1 1) --white
        onNewConnection = \layouts -> const $ pure (mempty, s0, [uncurry addIndicator s0 . fst $ NE.head layouts])
        onPong = \time _ _ -> pure
            let okPing = 1 / 10 -- time in seconds to map to 0.5 goodness
                goodness :: Double = 0.5 ** (realToFrac time / okPing) -- in range (0, 1]
                r = sRGB 0.85 0.28 0.28
                y = sRGB 0.94 0.95 0.33
                g = sRGB 0.2 0.72 0.2
                colour = fromPrizm 1 if goodness < 0.5
                    then interpolate (round $ 2 * goodness * 100) (toPrizm r, toPrizm y)
                    else interpolate (round $ (2 * goodness - 1) * 100) (toPrizm y, toPrizm g)
            in  ( Endo $ const (time, colour)
                ,
                    [ SetText elementId $ showT time
                    , SetIndicatorColour elementId colour
                    ]
                )
        elementId = ElementID "_internal_ping_indicator"
        addIndicator time colour Layout{..} =
            let (location, width, height) =
                    let ViewBox{..} = viewBox
                        s = min w h `div` 4
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
                        { text = showT time
                        , style = TextStyle (width `div` 5) (Colour 0 0 0 1) False False False [] "sans-serif"
                        }
                    , image = Nothing
                    , element = Indicator Indicator'
                        { hollowness = 0
                        , arcStart = 0
                        , arcEnd = 1
                        , centre = 0
                        , colour
                        , shape = square
                        }
                    , hidden = False
                    }
        onUpdate = onLayoutChange \l -> do
            s <- gets $ view #extra
            pure . pure $ uncurry addIndicator s l
     in ServerConfig
            { onNewConnection
            , onPong
            , onUpdate
            , updates = mempty
            , onStart = mempty
            , onDroppedConnection = mempty
            }

{- Util -}

toPrizm :: Data.Colour.Colour Double -> CIE.LCH
toPrizm = convert @Prizm.RGB . uncurryRGB mkRGB . fmap (round . (* 255)) . toSRGB

fromPrizm :: Double -> CIE.LCH -> Monpad.Colour
fromPrizm alpha = (\(ColorCoord (red, green, blue)) -> Colour{..}) .
    fmap ((/ 255) . fromIntegral) . unRGB . convert @_ @Prizm.RGB
