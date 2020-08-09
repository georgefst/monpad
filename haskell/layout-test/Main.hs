--TODO RecordDotSyntax/lenses
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Data.Text.IO qualified as T
import Dhall
import Dhall.Core
import Dhall.Import
import Dhall.Parser
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (radius)
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
            [] -> error "no argument given - provide a file to watch"

drawLayout :: FilePath -> IO ()
drawLayout file = do
    Layout {..} <- layoutFromDhall @() @() =<< dhallResolve file
    let (ss :: SizeSpec V2 Double) = mkSizeSpec $ Just . fi <$> grid
        opts = (SVGOptions ss Nothing "" [] True)
        file' = file -<.> "svg"
    renderSVG' file' opts $ foldMap draw elements

dhallResolve :: FilePath -> IO Text
dhallResolve file =
    pure . pretty
        =<< loadRelativeTo (takeDirectory file) UseSemanticCache
        =<< throws . exprFromText file
        =<< T.readFile file

class Draw a where
    draw :: a -> Diagram B

instance Draw (FullElement a b) where
    draw (FullElement {..}) = draw element & translate (fi <$> location)

instance Draw (Element a b) where
    draw = \case
        Stick {..} ->
            mconcat
                [ circle (fi radius) & fc' stickColour
                , circle (fi range) & fc' backgroundColour
                ]
        Button {..} -> draw shape & fc' colour
        Slider {..} ->
            mconcat
                [ circle (fi radius) & fc' sliderColour
                , rect (fi length) (fi width) & fc' backgroundColour & applyWhen vertical (rotateBy 0.25)
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
