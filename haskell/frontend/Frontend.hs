{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend (app) where

import Control.Concurrent
import Control.Monad (replicateM_)
import Control.Monad.IO.Class
import Control.Monad.State (get)
import Data.Aeson qualified as Aeson
import Data.Bool (bool)
import Data.Colour
import Data.Colour.SRGB
import Data.Foldable
import Data.Function (applyWhen)
import Data.List.Extra hiding ((!?))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord (clamp)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Typeable (Typeable, cast)
import Diagrams.Backend.Miso
import Diagrams.Prelude hiding (Attribute, D, Empty, Last, clamp, element, getLast, outer, radius, size, small, text, width, (%=), (%~), (.=), (.~), (^.))
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import Linear (unangle)
import Miso hiding (P, Text, back, for_, onMouseDown)
import Miso.String (ms)
import Monpad.Core hiding (Left, Right)
import Monpad.Core qualified
import Numeric (showHex)
import Optics hiding (Empty, both, element)
import Optics.State.Operators
import Util.Util (mwhen, showT)

import Data.Function.Memoize

deriveMemoizable ''V2

-- deriveMemoizable ''Text
instance Memoizable Text
instance Memoizable Double
instance Memoizable (AlphaColour Double)
deriveMemoizable ''ElementID
deriveMemoizable ''LayoutID
deriveMemoizable ''PosY
deriveMemoizable ''PosX
deriveMemoizable ''TextShadow
deriveMemoizable ''TextStyle
deriveMemoizable ''ViewBox
deriveMemoizable ''Shape
deriveMemoizable ''Indicator
deriveMemoizable ''TextBox
deriveMemoizable ''Image
deriveMemoizable ''TextInput
deriveMemoizable ''NumberInput
deriveMemoizable ''InputType
deriveMemoizable ''Input
deriveMemoizable ''Slider
deriveMemoizable ''Button
deriveMemoizable ''Stick
deriveMemoizable ''Element
deriveMemoizable ''FullElement
deriveMemoizable ''Layout
deriveMemoizable ''Encoding
deriveMemoizable ''ResetLayout

app :: JSM ()
app = do
    -- model <- case JD.decodeValue Auto.ElmFlags.decode f of
    --     Err e -> Task.fail $ JsonError e
    --     Ok flags -> load flags
    (model, initialAction) <- load hardcodedElmFlags
    startApp
        App
            { initialAction
            , model
            , update = flip $ foldlM \m a -> do
                -- TODO for debugging - remove
                -- case a of
                --     ConsoleLog _ -> pure ()
                --     _ -> batchEff () $ pure $ pure [ConsoleLog $ ms $ show a]
                fromTransition (updateModel a) m
            , view = \model' ->
                misoDia
                    ( def
                        & ( lensVL sizeSpec
                                .~ dims2D (fromIntegral model'.windowSize.x) (fromIntegral model'.windowSize.y)
                          )
                        & ( lensVL svgAttributes
                                -- TODO viewbox should probably be part of the `diagrams-miso` API
                                -- oh actually, maybe we're better off doing this within `diagrams`
                                -- otherwise units will be all wrong, right?
                                -- but then how does `elm-collage` make it work?
                                .~
                                -- mkViewbox
                                --     (viewBox.x)
                                --     -(fromIntegral viewBox.h + viewBox.y)
                                --     viewBox.w
                                --     viewBox.h
                                -- <>
                                [ ("width", show model'.windowSize.x <> "px")
                                , ("height", show model'.windowSize.y <> "px")
                                , ("style", "background-color: " <> sRGB24showA model'.layout.layout.backgroundColour)
                                ]
                          )
                    )
                    (viewModel model')
                    (forEventHandlers \e@(EventHandler _) -> handleHandler decoder' e)
            , -- TODO `defaultEvents` seems totally arbitrary...
              -- also, no docs on what the bool means (it eventually becomes `useCapture` arg to `addEventListener`)
              -- docs for `on` should mention this, since the pre-defined handlers seem to correspond to `defaultEvents`
              events = defaultEvents <> Map.fromList (forEventHandlers (\(EventHandler e) -> (getEventName e, False)))
            , subs =
                [ \f -> do
                    -- TODO put stuff from `Scratch.elm` here (configurably)
                    replicateM_ 5 do
                        -- t <- getCurrentTime
                        -- TODO delay doesn't work...
                        -- which makes it hard to test everything like we do in `Scratch.elm`
                        -- liftIO $ threadDelay 1_000_000
                        liftIO $ f [ConsoleLog "ping"]
                        -- liftIO $
                        --     f $
                        --         map
                        --             ServerUpdate
                        --             [ SwitchLayout $ LayoutID "B"
                        --             , SetBackgroundColour $ black `withOpacity` 1
                        --             , ResetLayout StateReset
                        --             , SetSliderPosition (ElementID "0") 0.7
                        --             ]
                ]
            , mountPoint = Nothing
            , logLevel = Off
            }

type D = QDiagram B V2 Double Ann

-- viewModel :: (_) => Model -> D
-- viewModel :: (b ~ B) => Model -> D
viewModel :: Model -> D
viewModel model =
    ( mconcatMap (viewElement model) (model.layout.layout.elements)
    -- <>
    -- -- TODO is there a better way? extend bounding box or envelope directly
    -- -- TODO inconsistent with Elm in that we won't clip out overflowing elements
    -- noAnn
    --     ( rect
    --         (fromIntegral model.layout.layout.viewBox.w)
    --         (fromIntegral model.layout.layout.viewBox.h)
    --         & fcA transparent
    --         & lw 0
    --     )
    --     -- TODO arbitrary - make configurable?
    --     -- also, this is consistent in a way that `elm-collage` never was, which is nice
    --     & lw 3
    )
        & addAnn
            ( Ann' (EventHandler EventPointerMove) \event ->
                maybe [] (\c -> c.onMove.unwrap event) $
                    Map.lookup event.pointerId model.layout.pointerCallbacks
            )
        -- TODO this doesn't get triggered
        -- that might be explainable by the way I think events in `diagrams-miso` work
        -- fundamental issue? component we care about leaving isn't actually tracked (but it is in the SVG...)
        -- I think I may have originally actually used "pointerup" in Elm version
        -- and then presumably changed for obvious reason, i.e. handler pointer being dragged offscreen
        -- NB even "pointerup" version doesn't work with `jsaddle-warp`
        -- & addAnn (Ann' (EventHandler EventPointerLeave) $ pure . PointerUp)
        & addAnn (Ann' (EventHandler EventPointerUp) $ pure . PointerUp)

-- , style "background-color" <| toCssString <| fromRgba model.layout.layout.backgroundColour

viewImage :: V2 Word -> Image -> D
viewImage size img =
    fmap (annotate mempty)
        . htmlDiagram size [style_ [("pointer-events", "none")]]
        $ img_ [src_ img.url, width_ $ ms $ size.x, height_ $ ms size.y]

-- TODO use `diagrams` text support instead?
-- `diagrams-miso` already supports it, and probably implements similarly to this

{- | We use a flexbox to center the text.
Rather than attempting to calculate the actual text size, we just set it to the maximum possible
(which is the size of the viewbox).
-}
viewText :: V2 Word -> TextBox -> D
viewText size x =
    fmap (annotate mempty)
        .
        -- ann mempty .
        htmlDiagram size [style_ [("pointer-events", "none")]]
        $ div_
            [ style_
                [ ("display", "flex")
                ,
                    ( "justify-content"
                    , case x.alignX of
                        Monpad.Core.Left -> "left"
                        Centre -> "center"
                        Monpad.Core.Right -> "right"
                    )
                ,
                    ( "align-items"
                    , case x.alignY of
                        Top -> "start"
                        Middle -> "center"
                        Bottom -> "end"
                    )
                , ("height", "100%")
                , ("user-select", "none")
                ]
            ]
            [ pre_
                [textStyle x.style]
                [text x.text]
            ]

elementSize :: Element () () -> V2 Word
elementSize = \case
    Stick e -> square_ $ e.radius * 2
    Button e -> shapeSize e.shape
    Slider e -> square_ $ e.radius * 2
    Indicator e -> shapeSize e.shape
    Input e -> V2 e.width e.height
    Empty v -> v
  where
    square_ x = V2 x x
    shapeSize s = case s of
        Circle r -> square_ $ r * 2
        Rectangle v -> v

-- viewElement :: (_) => Model -> FullElement () () -> D
viewElement :: Model -> FullElement () () -> D
viewElement = memoize2 \model element ->
    if element.hidden
        then mempty
        else
            -- TODO clean up `P2`/`V2` usage
            -- maybe also elsewhere
            let toOffset =
                    let w = fromIntegral <$> model.windowSize
                        v = V2 (fromIntegral model.layout.layout.viewBox.w) (fromIntegral model.layout.layout.viewBox.h)
                        -- how far we need to scale v down to fit within w
                        sf = min (w.x / v.x) (w.y / v.y)
                        -- pagePos counts down from the top, whereas our other coordinate systems count up from the bottom
                        invertPagePos = (V2 0 w.y +) . reflectY
                        pageToSvg =
                            P
                                . (V2 (fromIntegral model.layout.layout.viewBox.x) (fromIntegral model.layout.layout.viewBox.y) +)
                                . (((v - (w ^* (1 / sf))) ^* (1 / 2)) +)
                                . (^* (1 / sf))
                                . invertPagePos
                     in (.-^ (fromIntegral <$> element.location)) . pageToSvg . unP
             in ( case element.element of
                    Button x ->
                        viewButton element.name x $ Set.member element.name model.layout.pressed
                    Stick x ->
                        viewStick element.name x toOffset $
                            fromMaybe 0 (model.layout.stickPos !? element.name)
                    Slider x ->
                        viewSlider element.name x toOffset $
                            fromMaybe x.initialPosition (model.layout.sliderPos !? element.name)
                    Indicator x ->
                        viewIndicator element.name x
                    Input x ->
                        viewInput element.name x
                    Empty _ -> mempty
                )
                    & maybe id ((<>) . viewImage (elementSize element.element)) element.image
                    & maybe id ((<>) . viewText (elementSize element.element)) element.text
                    & translate (fromIntegral <$> element.location)

viewButton :: ElementID -> Button () -> Bool -> D
viewButton name button pressed =
    let shape = case button.shape of
            Circle (fromIntegral -> r) -> circle r
            Rectangle (fmap fromIntegral -> v) -> rect v.x v.y
     in shape
            & fcA (applyWhen pressed (darken 0.1) button.colour)
            & onPointerDown
                (const $ ButtonDown name)
                PointerCallbacks
                    { onMove = OnMove $ const []
                    , onRelease = [ClientUpdate $ ButtonUp name]
                    }

viewStick :: ElementID -> Stick () -> (P2 Double -> P2 Double) -> V2 Double -> D
viewStick name stick toOffset stickPos =
    let range =
            fromIntegral
                stick.range
        radius =
            fromIntegral
                stick.radius
        getOffset event =
            let
                v0 = toOffset $ toPoint event
                l = min range $ norm v0
             in
                if l == 0
                    then 0 -- (== v0)
                    else unP $ (l / range) *^ normalize v0
        big = circle range & fcA stick.backgroundColour
        small = circle radius & fcA stick.stickColour
     in ( (small & translate (range *^ stickPos))
            <> big
        )
            & onPointerDown
                (StickMove name . getOffset)
                PointerCallbacks
                    { onMove = OnMove \event -> [ClientUpdate $ StickMove name $ getOffset event]
                    , onRelease = [ClientUpdate $ StickMove name 0]
                    }

viewSlider :: ElementID -> Slider () -> (P2 Double -> P2 Double) -> Double -> D
viewSlider name slider toOffset pos =
    let width = fromIntegral slider.width
        r = fromIntegral slider.radius
        v = fromIntegral <$> slider.offset
        getOffset event =
            let P (V2 x y) = toOffset $ toPoint event
             in clamp (0, 1) $ (v.x * x + v.y * y) / quadrance v
        stick =
            circle r
                & fcA slider.sliderColour
                & translate (pos *^ v)
        background =
            roundedRect
                width
                (norm v + width)
                (width / 2)
                & fcA slider.backgroundColour
                & rotate (unangle v - pi / 2 @@ rad)
                & translate ((1 / 2) *^ v)
     in (stick <> background)
            -- & ann (EventPointerDown, \p -> ConsoleLog $ "slider clicked: " <> name.unwrap <> " " <> showT p)
            -- & ann (Ann' (EventHandler EventPointerDown) \e -> [ClientUpdate $ SliderMove name $ getOffset e])
            & onPointerDown
                (SliderMove name . getOffset)
                PointerCallbacks
                    { onMove = OnMove \event -> [ClientUpdate $ SliderMove name $ getOffset event]
                    , onRelease = mwhen slider.resetOnRelease [ClientUpdate $ SliderMove name slider.initialPosition]
                    }

-- TODO no need to use loads of line segments for indicator
-- unlike `elm-collage`, `diagrams` has great support for curves
viewIndicator :: ElementID -> Indicator -> D
viewIndicator _ ind =
    noAnn
        let a = ind.arcStart
            b =
                if ind.arcEnd < ind.arcStart
                    then ind.arcEnd + 1
                    else ind.arcEnd
            -- values in [0, 1) where we need a vertex
            angles = takeWhile (\x -> x < b) $ dropWhile (\x -> x < a) case ind.shape of
                Rectangle _ -> [(0 :: Double) .. 7] <&> \x -> (2 * x + 1) * 1 / 8
                Circle _ -> [(0 :: Double) .. 2 * nPoints - 1] <&> \x -> x / nPoints
                  where
                    nPoints = 256
            outer =
                (a : angles <> [b]) <&> \t -> case ind.shape of
                    Rectangle _ -> V2 (f t) (f $ t - 0.25)
                      where
                        mod1 x = x - fromIntegral (floor @_ @Int x)
                        {- there's nothing I can tell you about this function that you can't get
                            from typing in to Wolfram Alpha:
                           min(1, max(-1, ((abs(mod(x, 1) - 0.5) * 2) - 0.5) * 4))
                        -}
                        f x = clamp (-1, 1) $ ((abs (mod1 x - 0.5) * 2) - 0.5) * 4
                    Circle _ -> V2 (cos t1) (sin t1)
                      where
                        t1 = t * 2 * pi
            inner =
                if b == a + 1 && ind.hollowness == 0
                    then
                        -- Prevent ugly line in to the center.
                        -- Yes, these are float comparisons.
                        -- But we only need this workaround when the user has set exact integer values.
                        []
                    else outer <&> (+ ind.centre) . (^* ind.hollowness) . (- ind.centre)
            scale' = case ind.shape of
                Rectangle v -> (* P ((fromIntegral <$> v) / 2))
                Circle r -> (^* fromIntegral r)
         in (reverse outer <> inner)
                & map P -- TODO we should do this conversion further up
                & map scale'
                & fromVertices
                & fcA ind.colour

viewInput :: ElementID -> Input -> D
viewInput name inp =
    let
        {- TODO
           this is a bit of a hack, to allow us to reset the form
           do we really have to go by ID - can't Elm give us a handle to the element?
        -}
        formId = "form-" <> name.unwrap
     in
        -- padding ts =
        --     let size = ms (ts.size / 2) <> "px"
        --      in [ ("padding-left", size)
        --         , ("padding-right", size)
        --         ]

        fmap (annotate mempty)
            . htmlDiagram (fromIntegral <$> V2 inp.width inp.height) []
            $ form_
                [ style_
                    [ ("display", "flex")
                    , ("justify-content", "center")
                    , ("width", "100%")
                    , ("height", "100%")
                    ]
                , -- , onSubmit [ClientUpdate $ SubmitInput name, ResetForm id]
                  id_ formId
                ]
                [ input_
                    [ type_ case inp.inputType of
                        CheckBox () ->
                            "checkbox"
                        Number _ ->
                            "number"
                        Text _ ->
                            "text"
                    , style_
                        [ ("flex", "auto")
                        , ("width", "100%")
                        , ("height", "100%")
                        ]
                        --   , H.map List.singleton $
                        --         case inp.inputType of
                        --             CheckBox () ->
                        --                 onCheck (ClientUpdate << InputBool name)
                        --             Number _ ->
                        --                 onInput $
                        --                     \s ->
                        --                         case String.toInt s of
                        --                             Just f ->
                        --                                 ClientUpdate $ InputNumber name f
                        --                             Nothing ->
                        --                                 ConsoleLog $ "Failed to decode number input: " <> s
                        --             Text _ ->
                        --                 onInput (ClientUpdate << InputText name)
                    ]
                    -- <> ( case inp.inputType of
                    --         CheckBox () ->
                    --             []
                    --         Number opts ->
                    --             H.step (Maybe.unwrap "any" String.fromFloat opts.step)
                    --                 : padding opts.textStyle
                    --                 <> textStyle opts.textStyle
                    --                 <> Maybe.unwrap [] (List.singleton << H.min << String.fromFloat) opts.min
                    --                 <> Maybe.unwrap [] (List.singleton << H.max << String.fromFloat) opts.max
                    --         Text opts ->
                    --             padding opts.textStyle
                    --                 <> textStyle opts.textStyle
                    --                 <> Maybe.unwrap [] (List.singleton << H.minlength) opts.minLength
                    --                 <> Maybe.unwrap [] (List.singleton << H.maxlength) opts.maxLength
                    --    )
                ]

--  |> shift (unVec2 <| Vec2.scale range stickPos)

-- \|> onPointerDown
--     (StickMove name << getOffset)
--         { onMove = \event -> [ClientUpdate <| StickMove name <| getOffset event]
--         , onRelease = [ClientUpdate <| StickMove name <| vec2 0 0]
--         }

-- viewFullscreenButton : ViewBox -> Collage (List Msg)
-- viewFullscreenButton vb =
--     let
--         -- how much of the screen to cover
--         scale =
--             1 / 6

--         size =
--             toFloat (min vb.w vb.h) * scale

--         arrow =
--             let
--                 width =
--                     0.16

--                 shaft =
--                     0.2

--                 head =
--                     0.4

--                 gap =
--                     0.2
--             in
--             scanl Vec2.add
--                 (vec2 gap (-width / 2))
--                 [ vec2 0 width
--                 , vec2 shaft 0
--                 , vec2 0 ((head - width) / 2)
--                 , vec2 (head / 2) (-head / 2)
--                 , vec2 (-head / 2) (-head / 2)
--                 , vec2 0 ((head - width) / 2)
--                 ]
--                 |> List.map (unVec2 << Vec2.scale size)
--                 |> polygon
--                 |> styled1 (toRgba white)

--         arrows =
--             range 0 3
--                 |> List.map (\x -> arrow |> rotate ((toFloat x + 0.5) * pi / 2))
--                 |> stack
--     in
--     rectangle size size
--         |> styled1 (toRgba black)
--         |> impose arrows
--         |> shift ( toFloat vb.x + size / 2, toFloat (vb.h + vb.y) - size / 2 )
--         |> Collage.on "pointerdown" (JD.succeed [ GoFullscreen ])

-- {- Model -}

data Model = Model
    { username :: Text
    , windowSize :: V2 Word
    , fullscreen :: Bool
    , encoding :: Encoding
    , supportsFullscreen :: Bool
    , windowTitle :: Text
    , startTime :: POSIXTime
    , layout :: LayoutState -- the active layout
    , initialLayouts :: Map LayoutID (Layout () ())
    , layouts :: Map LayoutID LayoutState -- NB. the active layout doesn't get updated here until we switch out of it
    }
    deriving (Eq, Generic)

data LayoutState = LayoutState
    { layout :: Layout () ()
    , pressed :: Set ElementID -- buttons
    , stickPos :: Map ElementID (V2 Double)
    , sliderPos :: Map ElementID Double
    , pointerCallbacks :: Map Int PointerCallbacks -- keys are the ids of the pointers currently held down
    }
    deriving (Eq, Generic)

type Msgs = [Msg]

data Msg
    = NoOp
    | ClientUpdate ClientUpdate
    | ServerUpdate (ServerUpdate () ())
    | PointerDown Int PointerCallbacks
    | PointerUp PointerEvent
    | Resized (V2 Word)
    | GoFullscreen
    | FullscreenChange Bool
    | ResetForm Text
    | ConsoleLog Text
    deriving (Eq, Show)

-- TODO this is problematic due to Miso requiring `Eq` for model
-- having functions in Elm is heavily discouraged for similar reasons, so it's somewhat surprising that it ever worked
data PointerCallbacks = PointerCallbacks
    { onMove :: OnMove
    , onRelease :: [Msg]
    }
    deriving (Eq)
newtype OnMove = OnMove {unwrap :: PointerEvent -> [Msg]}
instance Eq OnMove where
    _ == _ = False
instance Show PointerCallbacks where
    show _ = "<PointerCallbacks>"

-- data Error
--     = JsonError
--         ()
--     | -- Aeson.Error
--       OtherError Text

load :: ElmFlags -> JSM (Model, [Msg])
load flags = do
    startTime <- liftIO getPOSIXTime
    -- windowSize <- getViewportSize
    -- let windowSize = V2 1920 1080
    -- let windowSize = V2 1600 900
    -- let windowSize = V2 2000 1000
    let windowSize = V2 1000 500
    let layouts = Map.fromList $ (NE.toList flags.layouts) <&> \x -> (x.name, loadLayout x)
    pure
        ( Model
            { username = flags.username
            , layout = loadLayout $ NE.head flags.layouts
            , windowSize
            , fullscreen = False
            , encoding = flags.encoding
            , supportsFullscreen = flags.supportsFullscreen
            , windowTitle = flags.windowTitle
            , startTime
            , initialLayouts = (.layout) <$> layouts
            , layouts
            }
        , -- sendInit
          []
        )

loadLayout :: Layout () () -> LayoutState
loadLayout layout =
    LayoutState
        { layout = layout
        , stickPos = mempty
        , pointerCallbacks = mempty
        , pressed = mempty
        , sliderPos = mempty
        }

updateModel :: Msg -> Transition Msgs Model ()
updateModel = \case
    NoOp -> pure ()
    ClientUpdate u ->
        -- sendUpdate model.encoding u >>
        case u of
            ButtonUp b -> #layout % #pressed %= Set.delete b
            ButtonDown b -> #layout % #pressed %= Set.insert b
            StickMove t p -> #layout % #stickPos %= Map.insert t p
            SliderMove t p -> #layout % #sliderPos %= Map.insert t p
            InputBool _ _ -> pure ()
            InputNumber _ _ -> pure ()
            InputText _ _ -> pure ()
            SubmitInput _ -> pure ()
            Pong _ -> pure ()
    ServerUpdate u -> serverUpdate u
    PointerDown pid callbacks -> #layout % #pointerCallbacks %= Map.insert pid callbacks
    PointerUp event -> do
        model <- get
        -- TODO for debugging
        -- scheduleIO $ pure $ pure $ ConsoleLog "released"
        #layout
            % #pointerCallbacks
            %= Map.delete event.pointerId
        -- TODO might be better to do this atomically, so use
        scheduleIO
            . pure
            $ maybe [] (\c -> c.onRelease)
            $ Map.lookup event.pointerId model.layout.pointerCallbacks
    Resized v -> #windowSize .= v
    GoFullscreen -> pure ()
    --  goFullscreen
    FullscreenChange b -> #fullscreen .= b
    ResetForm _ -> pure ()
    -- resetForm id'
    ConsoleLog t -> scheduleIO_ $ consoleLog t
  where

serverUpdate :: ServerUpdate () () -> Transition Msgs Model ()
serverUpdate = \case
    --     -- this needs to be equivalent to the same handling in the Haskell code
    -- TODO we can do this now - factor out somehow
    -- TODO use more lenses (currently fairly direct Elm port)
    PlayAudioURL _url -> pure ()
    -- playAudio url
    Vibrate _intervals -> pure ()
    -- vibrate intervals
    SetImageURL name u -> updateElementFull name \e -> e{image = fmap (\x -> x{url = u}) e.image}
    AddImage name x -> updateElementFull name \e -> e{image = Just x}
    DeleteImage name -> updateElementFull name \e -> e{image = Nothing}
    SetText name t -> updateElementFull name \e -> e{text = fmap (\x -> x{text = t}) e.text}
    SetTextStyle name style -> updateElementFull name \e -> e{text = fmap (\x -> x{style}) e.text}
    SetTextSize name x -> updateTextStyle name \s -> s{size = x}
    SetTextColour name x -> updateTextStyle name \s -> s{colour = x}
    SetTextBold name x -> updateTextStyle name \s -> s{bold = x}
    SetTextItalic name x -> updateTextStyle name \s -> s{italic = x}
    SetTextUnderline name x -> updateTextStyle name \s -> s{underline = x}
    SetTextShadow name x -> updateTextStyle name \s -> s{shadow = x}
    SetTextFont name x -> updateTextStyle name \s -> s{font = x}
    AddText name x -> updateElementFull name \e -> e{text = Just x}
    DeleteText name -> updateElementFull name \e -> e{text = Nothing}
    SetLayout l -> #layout .= loadLayout l
    SwitchLayout l -> do
        model <- get
        case Map.lookup l model.layouts of
            Just l' -> do
                #layout .= l'
                #layouts %= Map.insert model.layout.layout.name model.layout
            Nothing -> scheduleIO $ pure [ConsoleLog $ "Unknown layout: " <> l.unwrap]
    HideElement name -> updateElementFull name \e -> e{hidden = True}
    ShowElement name -> updateElementFull name \e -> e{hidden = False}
    AddElement e -> #layout % #layout % #elements %= (e :)
    RemoveElement e ->
        #layout %= \s ->
            s
                { pressed = Set.delete e s.pressed
                , sliderPos = Map.delete e s.sliderPos
                , stickPos = Map.delete e s.stickPos
                , layout = s.layout{elements = filter (\x -> x.name /= e) s.layout.elements}
                }
    SetBackgroundColour c -> #layout % #layout % #backgroundColour .= c
    SetIndicatorHollowness name x -> updateIndicator name \e -> e{hollowness = x}
    SetIndicatorArcStart name x -> updateIndicator name \e -> e{arcStart = x}
    SetIndicatorArcEnd name x -> updateIndicator name \e -> e{arcEnd = x}
    SetIndicatorShape name x -> updateIndicator name \e -> e{shape = x}
    SetIndicatorCentre name x -> updateIndicator name \e -> e{centre = x}
    SetIndicatorColour name x -> updateIndicator name \e -> e{colour = x}
    SetSliderPosition name x -> #layout % #sliderPos %= Map.insert name x
    SetButtonColour name x -> updateButton name \e -> e{colour = x}
    SetButtonPressed name x -> #layout % #pressed %= if x then Set.insert name else Set.delete name
    ResetLayout x -> case x of
        StateReset -> #layout %= \l -> loadLayout l.layout
        FullReset -> do
            model <- get
            case Map.lookup model.layout.layout.name model.initialLayouts of
                Just l -> #layout .= loadLayout l
                Nothing ->
                    -- this really shouldn't happen, since we never remove anything from the dict
                    scheduleIO $ pure [ConsoleLog $ "Not in initial layouts: " <> model.layout.layout.name.unwrap]
    --  performCmd [ ClientUpdate <| Pong t ] )
    Ping _t -> pure ()
  where
    -- TODO type sig only needed due to record ambiguities
    updateElementFull :: ElementID -> (FullElement () () -> FullElement () ()) -> Transition Msgs Model ()
    updateElementFull name f = #layout % #layout % #elements %= map \e -> if e.name == name then f e else e
    updateElement name f = updateElementFull name \fe -> fe{element = f fe.element}
    updateIndicator name f = updateElement name \e -> case e of
        Indicator ind -> Indicator $ f ind
        _ -> e
    updateButton name f = updateElement name \e -> case e of
        Button x -> Button $ f x
        _ -> e
    updateTextStyle name f = updateElementFull name $ #text %~ fmap (\x -> x{style = f x.style})

-- {- Util -}

-- styled1 : Colour -> Collage.Shape -> Collage msg
-- styled1 c =
--     styled ( uniform <| fromRgba c, defaultLineStyle )

-- subLogErrors : String -> (a -> Msgs) -> Sub (Result JD.Error a) -> Sub Msgs
-- subLogErrors s f =
--     Sub.map
--         (either
--             (\err -> [ ConsoleLog <| "Failed to decode " <> s <> ": " <> JD.errorToString err ])
--             f
--         )

onPointerDown ::
    (PointerEvent -> ClientUpdate) ->
    PointerCallbacks ->
    QDiagram B V2 Double Any ->
    D
onPointerDown f y =
    ann
        ( Ann'
            (EventHandler EventPointerDown)
            \e ->
                [ PointerDown e.pointerId y
                , ClientUpdate $ f e
                ]
        )
textStyle :: TextStyle -> Attribute action
textStyle s =
    style_ $
        [ ("font-size", showT s.size <> "px")
        , ("font-weight", bool "normal" "bold" s.bold)
        , ("font-style", bool "normal" "italic" s.italic)
        , ("text-decoration", bool "none" "underline" s.underline)
        , ("color", T.pack $ sRGB24showA s.colour)
        , ("font-family", s.font)
        ,
            ( "text-align"
            , case s.align of
                Monpad.Core.Left -> "left"
                Centre -> "center"
                Monpad.Core.Right -> "right"
            )
        , ("margin", "0")
        ]
            -- Rotating text is broken on Safari etc. - see https://github.com/georgefst/monpad/issues/45.
            -- We add a guard here so that it at least works when no rotation is actually set.
            <> ( if s.rotation == 0
                    then []
                    else [("transform", "rotate(" <> showT s.rotation <> "rad)")]
               )
            <> ( if null s.shadow
                    then []
                    else
                        [
                            ( "text-shadow"
                            , T.intercalate ", " $
                                s.shadow <&> \shadow ->
                                    T.intercalate
                                        " "
                                        [ showT shadow.offset.x <> "px"
                                        , showT -shadow.offset.y <> "px"
                                        , showT shadow.blur <> "px"
                                        , T.pack $ sRGB24showA shadow.colour
                                        ]
                            )
                        ]
               )

-- TODO don't hardcode (taken from Elm scratchpad for now)
-- get this from JS wrapper like in Elm version? we can probably do some of it here instead
-- also, rename the type
hardcodedElmFlags :: ElmFlags
hardcodedElmFlags =
    ElmFlags
        { username = "GT"
        , encoding = JSONEncoding
        , supportsFullscreen = False
        , windowTitle = "monpad scratch"
        , layouts =
            NE.fromList
                [ Layout
                    { elements =
                        [ FullElement
                            { location = V2 -600 0
                            , image = Nothing
                            , text = Nothing
                            , name = ElementID "indicator"
                            , hidden = False
                            , element =
                                Indicator
                                    Indicator'
                                        { hollowness = 0.5
                                        , arcStart = 0
                                        , arcEnd = 2 / 3
                                        , centre = 0
                                        , colour = opaque $ sRGB 0.8 0 0.5
                                        , shape = Circle 300
                                        }
                            }
                        , FullElement
                            { location = V2 900 0
                            , image = Nothing
                            , text = Nothing
                            , name = ElementID "powerbar"
                            , hidden = False
                            , element =
                                Indicator
                                    Indicator'
                                        { hollowness = 0
                                        , arcStart = 0.5
                                        , arcEnd = 1
                                        , centre = 0
                                        , colour = opaque purple
                                        , shape = Rectangle $ V2 100 800
                                        }
                            }
                        , FullElement
                            { location = V2 600 0
                            , image = Nothing
                            , text = Nothing
                            , name = ElementID "stick"
                            , hidden = False
                            , element =
                                Stick
                                    Stick'
                                        { radius = 80
                                        , range = 300
                                        , backgroundColour = opaque blue
                                        , stickColour = opaque white
                                        , stickDataX = ()
                                        , stickDataY = ()
                                        }
                            }
                        , FullElement
                            { location = V2 -300 -200
                            , image = Nothing
                            , text = Nothing
                            , name = ElementID "slider"
                            , hidden = False
                            , element =
                                Slider
                                    Slider'
                                        { radius = 40
                                        , width = 80
                                        , offset = V2 600 400
                                        , initialPosition = 0.2
                                        , resetOnRelease = True
                                        , backgroundColour = opaque green
                                        , sliderColour = opaque white
                                        , sliderData = ()
                                        }
                            }
                        , FullElement
                            { location = V2 0 350
                            , image = Nothing
                            , name = ElementID "button"
                            , hidden = False
                            , text =
                                Just
                                    TextBox
                                        { style =
                                            TextStyle
                                                { bold = True
                                                , italic = True
                                                , underline = True
                                                , colour = opaque darkgray
                                                , size = 60
                                                , shadow =
                                                    [ TextShadow
                                                        { offset = V2 2 -1
                                                        , blur = 2
                                                        , colour = opaque black
                                                        }
                                                    ]
                                                , rotation = 0.1
                                                , align = Centre
                                                , font = "sans-serif"
                                                }
                                        , alignX = Centre
                                        , alignY = Middle
                                        , text = "c'est un\nbutton"
                                        }
                            , element =
                                Button
                                    Button'
                                        { shape = Rectangle $ V2 300 100
                                        , colour = opaque yellow
                                        , buttonData = ()
                                        }
                            }
                        , FullElement
                            { location = V2 -500 0
                            , name = ElementID "image"
                            , hidden = False
                            , element = Empty $ V2 1000 1000
                            , image =
                                Just
                                    Image
                                        { url = "https://upload.wikimedia.org/wikipedia/commons/c/c2/Hieronymus_prag_a.jpg"
                                        }
                            , text = Nothing
                            }
                        ]
                    , viewBox = ViewBox{x = -1000, y = -500, w = 2000, h = 1000}
                    , backgroundColour = opaque $ sRGB 0.81 0.91 0.97
                    , name = LayoutID "A"
                    }
                , Layout
                    { elements =
                        [ FullElement
                            { location = V2 -600 0
                            , image = Nothing
                            , text = Nothing
                            , name = ElementID "0"
                            , hidden = False
                            , element =
                                Slider
                                    Slider'
                                        { radius = 200
                                        , width = 200
                                        , offset = V2 (600 * 2) 0
                                        , backgroundColour = opaque red
                                        , sliderColour = opaque white
                                        , resetOnRelease = True
                                        , initialPosition = 0
                                        , sliderData = ()
                                        }
                            }
                        ]
                    , viewBox = ViewBox{x = -1000, y = -500, w = 2000, h = 1000}
                    , backgroundColour = opaque white
                    , name = LayoutID "B"
                    }
                ]
        }

-- inpired by https://github.com/cocreature/diagrams-miso/blob/master/example/src/Colors.hs
-- TODO could the library be improved to make all this a bit easier?
-- i.e. add some helpers hardcoded to using `action` as the annotation type
-- or `Decoder action` or whatever we end up with
-- TODO eugh this is getting horrible
-- could we somehow just attach handlers by ID or something instead?
-- maybe depends how well things like `pointerleave` work
-- DummySimpleEvent :: EventHandler' (P2 Double)
type Ann = [Ann']
data Ann' where Ann' :: EventHandler e -> (e -> Msgs) -> Ann'
annotate :: (Monoid p) => p -> Any -> p
annotate c b = mwhen (getAny b) c
ann :: Ann' -> Diagram B -> D
ann a = fmap $ annotate $ pure a
noAnn :: Diagram B -> D
noAnn = fmap (annotate mempty)
addAnn :: Ann' -> D -> D
addAnn a x = fmap (a :) x
class Decodable e where
    decoder' :: Decoder e
instance Decodable PointerEvent where
    decoder' = pointerEventDecoder
data EventHandler e where
    EventHandler :: (Typeable e, Decodable e, ToPoint e) => EventHandler' e -> EventHandler e
deriving instance Eq (EventHandler e)
data EventHandler' e where
    EventPointerDown :: EventHandler' PointerEvent
    EventPointerMove :: EventHandler' PointerEvent
    EventPointerUp :: EventHandler' PointerEvent
    EventPointerLeave :: EventHandler' PointerEvent
deriving instance Eq (EventHandler' e)
forEventHandlers :: (forall e. EventHandler e -> a) -> [a]
forEventHandlers f =
    [ f $ EventHandler EventPointerDown
    , f $ EventHandler EventPointerMove
    , f $ EventHandler EventPointerUp
    , f $ EventHandler EventPointerLeave
    ]
  where
    _f :: EventHandler' e -> ()
    _f = \case
        EventPointerDown -> ()
        EventPointerMove -> ()
        EventPointerUp -> ()
        EventPointerLeave -> ()
class ToPoint a where
    toPoint :: a -> P2 Double
instance ToPoint (P2 Double) where
    toPoint = id
instance ToPoint PointerEvent where
    toPoint e = p2 e.pagePos
getEventName :: EventHandler' e -> Text
getEventName = \case
    EventPointerDown -> "pointerdown"
    EventPointerMove -> "pointermove"
    EventPointerUp -> "pointerup"
    EventPointerLeave -> "pointerleave"
instance Functor Decoder where fmap f d@Decoder{decoder = d'} = d{decoder = fmap f . d'}
handleHandler :: forall e. (Typeable e, ToPoint e) => Decoder e -> EventHandler e -> DiaAttr Ann Msgs
handleHandler d e@(EventHandler e0) =
    mkDiaAttr @_ @_ @Msgs
        (getEventName e0)
        d
        toPoint
        ( \p -> concatMap \case
            -- TODO that's gonna be a lot of noops...
            -- as well as just a lot of iterating through the same handlers
            -- Last (Just (Ann' (EventHandler _) (cast -> Just a)))
            Ann' (EventHandler (cast @_ @(EventHandler' e) -> Just e')) (cast -> Just a)
                | e == (EventHandler e') -> a p
            -- _ -> [NoOp]
            _ -> []
        )

-- mkViewbox :: Int -> Int -> Word -> Word -> Attrs
-- mkViewbox x y w h = [("viewbox", unwords $ map show [x, y] <> map show [w, h])]

-- https://github.com/ekmett/linear/issues/181
instance HasField "x" (V2 a) a where
    getField = (^. lensVL _x)
instance HasField "y" (V2 a) a where
    getField = (^. lensVL _y)
instance HasField "x" (P2 a) a where
    getField = (^. lensVL _x)
instance HasField "y" (P2 a) a where
    getField = (^. lensVL _y)

-- TODO is there a better way?
-- if not then this belongs in a library, maybe `colour` itself
sRGB24showA :: (RealFrac a, Floating a, Show a) => AlphaColour a -> String
sRGB24showA c = sRGB24show c' <> applyWhen (length a == 1) ('0' :) a
  where
    a = showHex (clamp (0, 255) $ floor @_ @Int $ alphaChannel c * 256) ""
    -- TODO this isn't even right - it affects the RGB values, when I really just want to remove the alpha
    -- low priority for now since we only actually use this for background color, which will be opaque in practice
    c' = c `Data.Colour.over` black

-- TODO
-- build more of these
-- try to upstream to Miso?
-- maybe copy from Elm?
-- check MDN for all fields
data PointerEvent = PointerEvent
    { pointerId :: Int
    , pagePos :: (Double, Double)
    }
    deriving (Eq, Show)
pointerEventDecoder :: Decoder PointerEvent
pointerEventDecoder =
    Decoder
        ( Aeson.withObject "event" $ \o ->
            PointerEvent
                <$> (o Aeson..: "pointerId")
                <*> ( (,)
                        <$> (o Aeson..: "pageX")
                        <*> (o Aeson..: "pageY")
                    )
        )
        -- TODO could we actually make use of this? maybe not
        (DecodeTarget [])
