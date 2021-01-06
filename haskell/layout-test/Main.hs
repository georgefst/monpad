{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (handle)
import qualified Data.Text.IO as T
import Data.Void
import Dhall
import Dhall.Core
import Dhall.Import
import Dhall.Parser
import Dhall.TypeCheck
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Layout
import Streamly.FSNotify
import qualified Streamly.Prelude as SP
import System.Directory
import System.Environment (getArgs)
import System.FilePath hiding (hasExtension)

main :: IO ()
main = getArgs >>= \case
    path : _ -> do
        fullPath <- canonicalizePath path
        let p = (isCreation `disj` isModification) `conj` EventPredicate ((== fullPath) . eventPath)
        drawLayout fullPath
        SP.mapM_ (drawLayout . eventPath) . snd
            =<< watchDirectory (takeDirectory fullPath) p
    [] -> T.putStrLn "No argument given - provide a file to watch!"

{-TODO 'threadDelay' saves us from often seeing an empty file (on Linux at least)
    the real issue is that we get too many inotify events, when all we care about is CLOSE_WRITE
    but because 'fsnotify' is cross-platform, there may be no good way to filter
-}
drawLayout :: FilePath -> IO ()
drawLayout file = printDhallErrors do
    layout <- layoutFromDhall @() @() =<< dhallResolve file
    threadDelay 10000
    let v@(V2 x y) = fi <$> layout.grid
        ss = mkSizeSpec $ Just <$> v
        out = file -<.> "svg"
    renderSVG out ss $
        lw 3 $
            foldMap draw layout.elements <> (rect x y & translate (v / 2) & fc pastelBlue)
    putStrLn $ "Successfully rendered to: " <> out

{-TODO this may well be incomplete
    anyway, if there isn't a better way of doing this, report to 'dhall-haskell'
-}
printDhallErrors :: IO () -> IO ()
printDhallErrors =
    handle @ParseError print
        . handle @(SourcedException MissingImports) print
        . handle @(TypeError Src Void) print

{-TODO using 'pretty' means we're repeating work
    perhaps 'layoutFromDhall' should take an 'Expr Src Void'
    (and be total, while we're at it?)
-}
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
        Stick s -> mconcat
            [ circle (fi s.radius) & fc' s.stickColour
            , circle (fi s.range) & fc' s.backgroundColour
            ]
        Button b -> draw b.shape & fc' b.colour
        Slider s -> mconcat
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
