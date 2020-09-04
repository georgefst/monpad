{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Text.IO qualified as T
import Dhall
import Dhall.Core
import Dhall.Import
import Dhall.Parser
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Layout
import Streamly.FSNotify
import Streamly.Prelude qualified as SP
import System.Directory
import System.Environment (getArgs)
import System.FilePath hiding (hasExtension)

main :: IO ()
main =
    getArgs
        >>= \case
            path : _ -> do
                fullPath <- canonicalizePath path
                drawLayout fullPath
                SP.mapM_ (drawLayout . eventPath) . snd
                    =<< watchDirectory
                        (takeDirectory fullPath)
                        ( (isCreation `disj` isModification)
                              `conj` EventPredicate ((== fullPath) . eventPath)
                        )
            [] -> T.putStrLn "No argument given - provide a file to watch!"

drawLayout :: FilePath -> IO ()
drawLayout file = do
    layout <- layoutFromDhall @() @() =<< dhallResolve file
    threadDelay 10000
    let v@(V2 x y) = fi <$> layout.grid
        ss = mkSizeSpec $ Just <$> v
        out = file -<.> "svg"
    renderSVG out ss $
        lw 3 $
            foldMap draw layout.elements <> (rect x y & translate (v / 2) & fc pastelBlue)
    putStrLn $ "Successfully rendered to: " <> out

dhallResolve :: FilePath -> IO Text
dhallResolve file =
    pure . pretty
        =<< loadRelativeTo (takeDirectory file) UseSemanticCache
        =<< throws . exprFromText file
        =<< T.readFile file

class Draw a where
    draw :: a -> Diagram B

instance Draw (FullElement a b) where
    draw e = draw e.element & translate (fi <$> e.location)

instance Draw (Element a b) where
    draw = \case
        Stick s ->
            mconcat
                [ circle (fi s.radius) & fc' s.stickColour
                , circle (fi s.range) & fc' s.backgroundColour
                ]
        Button b -> draw b.shape & fc' b.colour
        Slider s ->
            mconcat
                [ circle (fi s.radius) & fc' s.sliderColour
                , roundedRect (fi s.length) (fi s.width) (fi s.width / 2)
                      & applyWhen s.vertical (rotateBy 0.25)
                      & fc' s.backgroundColour
                ]

instance Draw Shape where
    draw = \case
        Circle r -> circle $ fi r
        Rectangle (V2 x y) -> rect (fi x) (fi y)

fc' :: Layout.Colour -> Diagram B -> Diagram B
fc' = fcA . col

col :: Layout.Colour -> AlphaColour Double
col (Colour r g b a) = sRGB r g b `withOpacity` a

fi :: Int -> Double
fi = fromIntegral

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen b f = if b then f else id

pastelBlue :: (Ord a, Floating a) => Diagrams.Prelude.Colour a
pastelBlue = sRGB24 207 231 247
