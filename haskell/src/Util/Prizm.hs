{- TODO upstream this?
on the other hand, prizm is a bit of an odd library
with its overuse of `convert`, and strange use of numeric types, particularly percentages as ints up to 100
perhaps we'd be better off upstreaming its colour interpolation logic in to the `colour` library itself
-}

module Util.Prizm where

import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.SRGB (Colour, sRGB, toSRGB)
import Data.Convertible (convert)
import Data.List (genericLength)
import Data.Prizm.Color as Prizm
import Data.Prizm.Color.CIE as CIE

import Util

toPrizm :: Colour Double -> CIE.LCH
toPrizm = convert @Prizm.RGB . uncurryRGB mkRGB . fmap (round . (* 255)) . toSRGB

fromPrizm :: CIE.LCH -> Colour Double
fromPrizm =
    (\(ColorCoord (red, green, blue)) -> sRGB red green blue)
        . fmap ((/ 255) . fromIntegral)
        . unRGB
        . convert @_ @Prizm.RGB

interpolateList :: Double -> [Colour Double] -> LCH
interpolateList weight (pairAdjacent . map toPrizm -> colourPairs) =
    interpolate (round $ f * 100) $ colourPairs !! n
  where
    (n, f) = properFraction $ genericLength colourPairs * weight
