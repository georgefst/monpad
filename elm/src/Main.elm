module Main exposing (..)

import Auto.Button exposing (..)
import Auto.Colour exposing (..)
import Auto.Element as Element
import Auto.ElmFlags exposing (..)
import Auto.FullElement exposing (..)
import Auto.Image exposing (..)
import Auto.Indicator exposing (..)
import Auto.IntVec2 exposing (..)
import Auto.Layout exposing (..)
import Auto.ResetLayout exposing (..)
import Auto.ServerUpdate exposing (..)
import Auto.Shape exposing (..)
import Auto.Slider exposing (..)
import Auto.Stick exposing (..)
import Auto.TextBox exposing (..)
import Auto.TextStyle exposing (..)
import Auto.Update exposing (..)
import Auto.ViewBox exposing (..)
import Basics exposing (..)
import Basics.Extra exposing (..)
import Browser exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Collage exposing (..)
import Collage.Events as Collage
import Collage.Layout exposing (..)
import Collage.Render exposing (svgExplicit)
import Collage.Text as Text
import Color exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as JD
import List exposing (..)
import List.Extra exposing (..)
import Loadable
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Maybe exposing (..)
import Platform.Cmd as Cmd
import Ports exposing (..)
import Set exposing (Set)
import Task
import Time exposing (..)
import Tuple exposing (..)
import Util exposing (..)


main : Loadable.Program JD.Value Model Msgs Error
main =
    Loadable.application app


app :
    { load : JD.Value -> a -> b -> Task.Task Error ( Model, Cmd Msgs )
    , update : Msgs -> Model -> ( Model, Cmd Msgs )
    , view : Model -> Document Msgs
    , subscriptions : Model -> Sub Msgs
    , failCmd : Maybe c
    , loadingView : Maybe d
    , errorView : Maybe (Error -> { title : String, body : List (Html msg) })
    , onUrlRequest : e -> Msgs
    , onUrlChange : f -> Msgs
    }
app =
    { load =
        \f _ _ ->
            case JD.decodeValue Auto.ElmFlags.decode f of
                Err e ->
                    Task.fail <| JsonError e

                Ok flags ->
                    load flags
    , update =
        flip <|
            \model ->
                mapSecond Cmd.batch
                    << foldl
                        (\msg ( m, cs ) -> mapSecond (\c -> c :: cs) (update msg m))
                        ( model, [] )
    , view = view
    , subscriptions =
        always <|
            Sub.batch
                [ receiveUpdates
                    |> subLogErrors "update message" (List.map ServerUpdate)
                , fullscreenChanges
                    |> subLogErrors "fullscreen change" (singleton << FullscreenChange)
                , onResize (\w h -> [ Resized { x = w, y = h } ])
                ]
    , failCmd = Nothing
    , loadingView = Nothing
    , errorView =
        Just <|
            \e ->
                { title = "Whoops"
                , body =
                    [ Html.text "Monpad failed to start. If you're using \"elm reactor\", try Scratch.elm instead."
                    , pre []
                        [ Html.text <|
                            case e of
                                JsonError err ->
                                    JD.errorToString err

                                OtherError s ->
                                    s
                        ]
                    ]
                }
    , onUrlRequest = always []
    , onUrlChange = always []
    }



{- View -}


view : Model -> Document Msgs
view model =
    { title = "monpad"
    , body =
        let
            { x, y, w, h } =
                model.layout.layout.viewBox

            unknownIdMsg event =
                ConsoleLog <| "Unknown pointer id: " ++ String.fromInt event.pointerId
        in
        [ div
            [ style "background-color" <| toCssString <| fromRgba model.layout.layout.backgroundColour ]
            [ svgExplicit
                [ viewBox x -(h + y) w h
                , style "touch-action" "none"
                , Pointer.onMove <|
                    \event ->
                        Dict.get event.pointerId model.layout.pointerCallbacks
                            |> maybe [ unknownIdMsg event ] (\c -> c.onMove event)
                , Pointer.onLeave <|
                    \event ->
                        Dict.get event.pointerId model.layout.pointerCallbacks
                            |> maybe [ unknownIdMsg event ] (\c -> c.onRelease)
                            |> (\msgs -> PointerUp event.pointerId :: msgs)
                , style "width" (String.fromInt model.windowSize.x ++ "px")
                , style "height" (String.fromInt model.windowSize.y ++ "px")
                ]
              <|
                (List.map (viewElement model) model.layout.layout.elements
                    |> applyWhen (not model.fullscreen) (\es -> viewFullscreenButton model.layout.layout.viewBox :: es)
                    |> stack
                )
            ]
        ]
    }


viewText : TextBox -> List (Collage Msgs) -> Collage Msgs
viewText x extra =
    Text.fromString x.text
        |> Text.size x.style.size
        |> Text.color (Color.fromRgba x.style.colour)
        |> applyWhen x.style.bold (Text.weight Text.Bold)
        |> applyWhen x.style.italic (Text.shape Text.Italic)
        |> applyWhen x.style.underline (Text.line Text.Under)
        |> Collage.rendered
        |> impose (stack extra)


viewElement : Model -> FullElement -> Collage Msgs
viewElement model element =
    let
        toOffset =
            let
                w0 =
                    toFloat model.windowSize.x

                h0 =
                    toFloat model.windowSize.y

                w1 =
                    toFloat model.layout.layout.viewBox.w

                h1 =
                    toFloat model.layout.layout.viewBox.h

                rw =
                    w0 / w1

                rh =
                    h0 / h1

                -- adjust for when aspect ratio of layout's viewbox doesn't match page
                ( w2, h2 ) =
                    if rw < rh then
                        ( w0, h1 * rw )

                    else
                        ( w1 * rh, h0 )

                bottomLeft =
                    vec2FromIntRecord model.layout.layout.viewBox

                pageToSvg =
                    Vec2.add (Vec2.scale (1 / 2) <| vec2 (w2 - w0) (h2 - h0))
                        >> scaleVec2
                            { sfX = w1 / w2
                            , sfY = -h1 / h2
                            }
                        >> Vec2.add bottomLeft
                        >> Vec2.add (vec2 0 h1)
            in
            flip Vec2.sub (vec2FromIntRecord element.location) << pageToSvg

        -- stuff imposed on top of the element, which forms part of it for the sake of detecting pointer events etc.
        extra =
            maybe [] (\style -> [ viewText { text = element.name, style = style } [] ]) element.showName
    in
    extra
        |> (case element.element of
                Element.Button x ->
                    viewButton element.name x <| Set.member element.name model.layout.pressed

                Element.Stick x ->
                    viewStick element.name x toOffset <|
                        withDefault zeroVec2 <|
                            Dict.get element.name model.layout.stickPos

                Element.Slider x ->
                    viewSlider element.name x toOffset <|
                        withDefault x.initialPosition <|
                            Dict.get element.name model.layout.sliderPos

                Element.Image x ->
                    viewImage element.name x x.url

                Element.TextBox x ->
                    viewText x

                Element.Indicator x ->
                    viewIndicator element.name x
           )
        |> shift ( Basics.toFloat element.location.x, Basics.toFloat element.location.y )


viewButton : String -> Button -> Bool -> List (Collage Msgs) -> Collage Msgs
viewButton name button pressed extra =
    let
        shape =
            case button.shape of
                Circle r ->
                    circle <| toFloat r

                Rectangle v ->
                    rectangle (Basics.toFloat v.x) (Basics.toFloat v.y)
    in
    shape
        |> styled
            ( uniform <| applyWhen pressed darkColor <| fromRgba button.colour
            , solid thick <| uniform black
            )
        |> impose (stack extra)
        |> onPointerDown (always <| ButtonDown name)
            { onMove = always []
            , onRelease = [ Update <| ButtonUp name ]
            }


viewStick : String -> Stick -> (Vec2 -> Vec2) -> Vec2 -> List (Collage Msgs) -> Collage Msgs
viewStick name stick toOffset stickPos extra =
    let
        range =
            toFloat stick.range

        rad =
            toFloat stick.radius

        getOffset event =
            let
                v0 =
                    toOffset <| uncurry vec2 <| event.pointer.pagePos

                length =
                    min range <| Vec2.length v0
            in
            Vec2.normalize v0 |> Vec2.scale (length / range)

        big =
            circle range |> styled1 stick.backgroundColour

        small =
            circle rad |> styled1 stick.stickColour
    in
    stack (extra ++ [ small |> shift (unVec2 <| Vec2.scale range stickPos), big ])
        |> onPointerDown (StickMove name << getOffset)
            { onMove = \event -> [ Update <| StickMove name <| getOffset event ]
            , onRelease = [ Update <| StickMove name <| vec2 0 0 ]
            }


viewSlider : String -> Slider -> (Vec2 -> Vec2) -> Float -> List (Collage Msgs) -> Collage Msgs
viewSlider name slider toOffset pos extra =
    let
        width =
            toFloat slider.width

        rad =
            toFloat slider.radius

        v =
            vec2FromIntRecord slider.offset

        getOffset event =
            let
                { x, y } =
                    Vec2.toRecord <| toOffset <| uncurry vec2 <| event.pointer.pagePos
            in
            (Vec2.getX v * x + Vec2.getY v * y)
                / Vec2.lengthSquared v
                |> clamp 0 1

        stick =
            circle rad
                |> styled1 slider.sliderColour

        background =
            roundedRectangle width (Vec2.length v + width) (width / 2)
                |> styled1 slider.backgroundColour
                |> rotate (angleVec2 v - pi / 2)
                |> shift (unVec2 <| Vec2.scale (1 / 2) v)
    in
    stack (extra ++ [ stick |> shift (unVec2 <| Vec2.scale pos v), background ])
        |> onPointerDown (SliderMove name << getOffset)
            { onMove = \event -> [ Update <| SliderMove name <| getOffset event ]
            , onRelease =
                if slider.resetOnRelease then
                    [ Update <| SliderMove name <| slider.initialPosition ]

                else
                    []
            }


viewImage : String -> Image -> String -> List (Collage Msgs) -> Collage Msgs
viewImage _ img url extra =
    image (both toFloat ( img.width, img.height )) url
        |> impose (stack extra)


viewIndicator : String -> Indicator -> List (Collage Msgs) -> Collage Msgs
viewIndicator _ ind extra =
    let
        a =
            ind.arcStart

        b =
            if ind.arcEnd < ind.arcStart then
                ind.arcEnd + 1

            else
                ind.arcEnd

        -- values in [0, 1) where we need a vertex
        angles =
            takeWhile (\x -> x < b) <|
                dropWhile (\x -> x < a) <|
                    case ind.shape of
                        Rectangle _ ->
                            range 0 7
                                |> List.map (\x -> (2 * toFloat x + 1) * 1 / 8)

                        Circle _ ->
                            let
                                nPoints =
                                    256
                            in
                            range 0 (2 * nPoints - 1)
                                |> List.map (\x -> toFloat x / nPoints)

        outer =
            (a :: angles ++ [ b ])
                |> List.map
                    (\t ->
                        case ind.shape of
                            Rectangle _ ->
                                let
                                    mod1 x =
                                        x - toFloat (floor x)

                                    {- there's nothing I can tell you about this function that you can't get
                                        from typing in to Wolfram Alpha:
                                       min(1, max(-1, ((abs(mod(x, 1) - 0.5) * 2) - 0.5) * 4))
                                    -}
                                    f x =
                                        clamp -1 1 <| ((abs (mod1 x - 0.5) * 2) - 0.5) * 4
                                in
                                vec2 (f t) (f <| t - 0.25)

                            Circle _ ->
                                let
                                    t1 =
                                        t * 2 * pi
                                in
                                vec2 (cos t1) (sin t1)
                    )

        inner =
            if b == a + 1 && ind.hollowness == 0 then
                -- Prevent ugly line in to the center.
                -- Yes, these are float comparisons.
                -- But we only need this workaround when the user has set exact integer values.
                []

            else
                outer
                    |> List.map (Vec2.add ind.centre << Vec2.scale ind.hollowness << flip Vec2.sub ind.centre)

        scale =
            case ind.shape of
                Rectangle v ->
                    scaleVec2 { sfX = toFloat v.x / 2, sfY = toFloat v.y / 2 }

                Circle r ->
                    Vec2.scale <| toFloat r
    in
    (reverse outer ++ inner)
        |> List.map (unVec2 << scale)
        |> polygon
        |> styled1 ind.colour
        |> impose (stack extra)


viewFullscreenButton : ViewBox -> Collage (List Msg)
viewFullscreenButton vb =
    let
        -- how much of the screen to cover
        scale =
            1 / 6

        size =
            toFloat (min vb.w vb.h) * scale

        arrow =
            let
                width =
                    0.16

                shaft =
                    0.2

                head =
                    0.4

                gap =
                    0.2
            in
            scanl Vec2.add
                (vec2 gap (-width / 2))
                [ vec2 0 width
                , vec2 shaft 0
                , vec2 0 ((head - width) / 2)
                , vec2 (head / 2) (-head / 2)
                , vec2 (-head / 2) (-head / 2)
                , vec2 0 ((head - width) / 2)
                ]
                |> List.map (unVec2 << Vec2.scale size)
                |> polygon
                |> styled1 (toRgba white)

        arrows =
            range 0 3
                |> List.map (\x -> arrow |> rotate ((toFloat x + 0.5) * pi / 2))
                |> stack
    in
    rectangle size size
        |> styled1 (toRgba black)
        |> impose arrows
        |> shift ( toFloat vb.x + size / 2, toFloat (vb.h + vb.y) - size / 2 )
        |> Collage.on "pointerdown" (JD.succeed [ GoFullscreen ])



{- Model -}


type alias Model =
    { username : String
    , windowSize : IntVec2
    , fullscreen : Bool
    , startTime : Posix
    , layout : LayoutState -- the active layout
    , initialLayouts : Dict String Layout
    , layouts : Dict String LayoutState -- NB. the active layout doesn't get updated here until we switch out of it
    }


type alias LayoutState =
    { layout : Layout
    , pressed : Set String -- buttons
    , stickPos : Dict String Vec2
    , sliderPos : Dict String Float
    , pointerCallbacks : Dict Int PointerCallbacks -- keys are the ids of the pointers currently held down
    }


type alias Msgs =
    List Msg


type Msg
    = Update Update
    | ServerUpdate ServerUpdate
    | PointerDown Int PointerCallbacks
    | PointerUp Int
    | Resized IntVec2
    | GoFullscreen
    | FullscreenChange Bool
    | ConsoleLog String


type alias PointerCallbacks =
    { onMove : Pointer.Event -> Msgs, onRelease : Msgs }


type Error
    = JsonError JD.Error
    | OtherError String


load : ElmFlags -> Task.Task Error ( Model, Cmd Msgs )
load flags =
    now
        |> Task.andThen
            (\startTime ->
                getViewportSize
                    |> Task.andThen
                        (\viewport ->
                            case flags.layouts of
                                [] ->
                                    Task.fail <| OtherError "layout list is empty"

                                layout :: layouts1 ->
                                    let
                                        layouts =
                                            (layout :: layouts1)
                                                |> List.map (\x -> ( x.name, loadLayout x ))
                                                |> Dict.fromList
                                    in
                                    Task.succeed <|
                                        ( { username = flags.username
                                          , layout = loadLayout layout
                                          , windowSize = viewport
                                          , fullscreen = False
                                          , startTime = startTime
                                          , initialLayouts = layouts |> Dict.map (always <| \x -> x.layout)
                                          , layouts = layouts
                                          }
                                        , sendInit
                                        )
                        )
            )


loadLayout : Layout -> LayoutState
loadLayout layout =
    { layout = layout
    , stickPos = Dict.empty
    , pointerCallbacks = Dict.empty
    , pressed = Set.empty
    , sliderPos = Dict.empty
    }


update : Msg -> Model -> ( Model, Cmd Msgs )
update msg model =
    let
        layoutState =
            model.layout
    in
    case msg of
        Update u ->
            let
                model1 =
                    case u of
                        ButtonUp b ->
                            { model | layout = { layoutState | pressed = Set.remove b layoutState.pressed } }

                        ButtonDown b ->
                            { model | layout = { layoutState | pressed = Set.insert b layoutState.pressed } }

                        StickMove t p ->
                            { model | layout = { layoutState | stickPos = Dict.insert t p layoutState.stickPos } }

                        SliderMove t p ->
                            { model | layout = { layoutState | sliderPos = Dict.insert t p layoutState.sliderPos } }
            in
            ( model1, sendUpdate u )

        ServerUpdate u ->
            serverUpdate u model

        PointerDown pid callbacks ->
            ( { model
                | layout =
                    { layoutState
                        | pointerCallbacks = Dict.insert pid callbacks layoutState.pointerCallbacks
                    }
              }
            , Cmd.none
            )

        PointerUp pid ->
            ( { model | layout = { layoutState | pointerCallbacks = Dict.remove pid layoutState.pointerCallbacks } }
            , Cmd.none
            )

        Resized v ->
            ( { model | windowSize = v }, Cmd.none )

        GoFullscreen ->
            ( model, goFullscreen )

        FullscreenChange b ->
            ( { model | fullscreen = b }, Cmd.none )

        ConsoleLog s ->
            ( model, consoleLog s )


serverUpdate : ServerUpdate -> Model -> ( Model, Cmd Msgs )
serverUpdate u model =
    let
        layoutState =
            model.layout

        layout =
            layoutState.layout

        updateIndicator name f =
            { layout
                | elements =
                    layout.elements
                        |> List.map
                            (\fe ->
                                if fe.name == name then
                                    case fe.element of
                                        Element.Indicator ind ->
                                            { fe | element = Element.Indicator <| f ind }

                                        _ ->
                                            fe

                                else
                                    fe
                            )
            }
    in
    case u of
        SetImageURL image url ->
            ( { model
                | layout =
                    { layoutState
                        | layout =
                            { layout
                                | elements =
                                    layout.elements
                                        |> List.map
                                            (\e ->
                                                if e.name == image then
                                                    case e.element of
                                                        Element.Image img ->
                                                            { e | element = Element.Image { img | url = url } }

                                                        _ ->
                                                            e

                                                else
                                                    e
                                            )
                            }
                    }
              }
            , Cmd.none
            )

        SetText id text ->
            ( { model
                | layout =
                    { layoutState
                        | layout =
                            { layout
                                | elements =
                                    layout.elements
                                        |> List.map
                                            (\e ->
                                                if e.name == id then
                                                    case e.element of
                                                        Element.TextBox box ->
                                                            { e | element = Element.TextBox { box | text = text } }

                                                        _ ->
                                                            e

                                                else
                                                    e
                                            )
                            }
                    }
              }
            , Cmd.none
            )

        PlayAudioURL url ->
            ( model
            , playAudio url
            )

        Vibrate intervals ->
            ( model
            , vibrate intervals
            )

        SetLayout l ->
            ( { model | layout = loadLayout l }
            , Cmd.none
            )

        SwitchLayout id ->
            case Dict.get id model.layouts of
                Just l ->
                    ( { model | layout = l, layouts = Dict.insert model.layout.layout.name model.layout model.layouts }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, performCmd [ ConsoleLog <| "Unknown layout: " ++ id ] )

        AddElement e ->
            ( { model | layout = { layoutState | layout = { layout | elements = e :: layout.elements } } }
            , Cmd.none
            )

        RemoveElement e ->
            ( { model
                | layout =
                    { layoutState
                        | pressed = Set.remove e layoutState.pressed
                        , sliderPos = Dict.remove e layoutState.sliderPos
                        , stickPos = Dict.remove e layoutState.stickPos
                        , layout =
                            { layout
                                | elements =
                                    layout.elements
                                        |> List.filter
                                            (\x -> x.name /= e)
                            }
                    }
              }
            , Cmd.none
            )

        SetBackgroundColour c ->
            ( { model | layout = { layoutState | layout = { layout | backgroundColour = c } } }
            , Cmd.none
            )

        SetIndicatorHollowness name x ->
            ( { model | layout = { layoutState | layout = updateIndicator name (\e -> { e | hollowness = x }) } }
            , Cmd.none
            )

        SetIndicatorArcStart name x ->
            ( { model | layout = { layoutState | layout = updateIndicator name (\e -> { e | arcStart = x }) } }
            , Cmd.none
            )

        SetIndicatorArcEnd name x ->
            ( { model | layout = { layoutState | layout = updateIndicator name (\e -> { e | arcEnd = x }) } }
            , Cmd.none
            )

        SetIndicatorShape name x ->
            ( { model | layout = { layoutState | layout = updateIndicator name (\e -> { e | shape = x }) } }
            , Cmd.none
            )

        SetIndicatorCentre name x ->
            ( { model | layout = { layoutState | layout = updateIndicator name (\e -> { e | centre = x }) } }
            , Cmd.none
            )

        SetIndicatorColour name x ->
            ( { model | layout = { layoutState | layout = updateIndicator name (\e -> { e | colour = x }) } }
            , Cmd.none
            )

        SetSliderPosition name x ->
            ( { model | layout = { layoutState | sliderPos = Dict.insert name x layoutState.sliderPos } }
            , Cmd.none
            )

        ResetLayout x ->
            case x of
                StateReset ->
                    ( { model | layout = loadLayout layoutState.layout }
                    , Cmd.none
                    )

                FullReset ->
                    case Dict.get layoutState.layout.name model.initialLayouts of
                        Just l ->
                            ( { model | layout = loadLayout l }
                            , Cmd.none
                            )

                        Nothing ->
                            -- this really shouldn't happen, since we never remove anything from the dict
                            ( model, consoleLog <| "Not in initial layouts: " ++ layoutState.layout.name )



{- Util -}


styled1 : Colour -> Collage.Shape -> Collage msg
styled1 c =
    styled ( uniform <| fromRgba c, defaultLineStyle )


subLogErrors : String -> (a -> Msgs) -> Sub (Result JD.Error a) -> Sub Msgs
subLogErrors s f =
    Sub.map
        (either
            (\err -> [ ConsoleLog <| "Failed to decode " ++ s ++ ": " ++ JD.errorToString err ])
            f
        )


onPointerDown :
    (Pointer.Event -> Update)
    -> PointerCallbacks
    -> Collage Msgs
    -> Collage Msgs
onPointerDown f y =
    Collage.on "pointerdown"
        (Pointer.eventDecoder
            |> JD.map
                (\x ->
                    [ PointerDown x.pointerId y
                    , Update <| f x
                    ]
                )
        )
