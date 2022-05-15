module Main exposing (..)

import Auto.Button exposing (..)
import Auto.ClientUpdate exposing (..)
import Auto.Colour exposing (..)
import Auto.Element as Element
import Auto.ElmFlags exposing (..)
import Auto.Encoding exposing (..)
import Auto.FullElement exposing (..)
import Auto.Image exposing (..)
import Auto.Indicator exposing (..)
import Auto.Input exposing (..)
import Auto.InputType as InputType
import Auto.IntVec2 exposing (..)
import Auto.Layout exposing (..)
import Auto.ResetLayout exposing (..)
import Auto.ServerUpdate exposing (..)
import Auto.Shape exposing (..)
import Auto.Slider exposing (..)
import Auto.Stick exposing (..)
import Auto.TextBox exposing (..)
import Auto.TextStyle exposing (..)
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
import Color exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as H exposing (style)
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as JD
import List exposing (..)
import List.Extra exposing (..)
import Loadable
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Maybe exposing (..)
import Maybe.Extra as Maybe
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
        in
        [ svgExplicit
            [ viewBox x -(h + y) w h
            , Pointer.onMove <|
                \event ->
                    Dict.get event.pointerId model.layout.pointerCallbacks
                        |> maybe [] (\c -> c.onMove event)
            , Pointer.onLeave (singleton << PointerUp)
            , style "width" (String.fromInt model.windowSize.x ++ "px")
            , style "height" (String.fromInt model.windowSize.y ++ "px")
            , style "background-color" <| toCssString <| fromRgba model.layout.layout.backgroundColour
            ]
          <|
            (List.map (viewElement model) model.layout.layout.elements
                |> applyWhen (not model.fullscreen && model.supportsFullscreen)
                    (\es -> viewFullscreenButton model.layout.layout.viewBox :: es)
                |> stack
            )
        ]
    }


viewImage : Image -> Collage Msgs
viewImage img =
    html (both toFloat ( img.width, img.height )) [ style "pointer-events" "none" ] <|
        Html.img [ H.src img.url, H.width img.width, H.height img.height ] []


{-| We use a flexbox to center the text.
Rather than attempting to calculate the actual text size, we just set it to the maximum possible
(which is the size of the viewbox).
-}
viewText : ViewBox -> TextBox -> Collage Msgs
viewText vb x =
    html (both toFloat ( vb.w, vb.h )) [ style "pointer-events" "none" ] <|
        div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "height" "100%"
            , style "user-select" "none"
            ]
            [ pre
                (textStyle x.style)
                [ Html.text x.text ]
            ]


viewElement : Model -> FullElement -> Collage Msgs
viewElement model element =
    if element.hidden then
        stack []

    else
        let
            toOffset =
                let
                    w =
                        vec2FromIntRecord model.windowSize

                    v =
                        vec2 (toFloat model.layout.layout.viewBox.w) (toFloat model.layout.layout.viewBox.h)

                    -- how far we need to scale v down to fit within w
                    sf =
                        min (Vec2.getX w / Vec2.getX v) (Vec2.getY w / Vec2.getY v)

                    -- pagePos counts down from the top, whereas our other coordinate systems count up from the bottom
                    invertPagePos =
                        mapY negate >> Vec2.add (vec2 0 <| Vec2.getY w)

                    pageToSvg =
                        invertPagePos
                            >> Vec2.scale (1 / sf)
                            >> Vec2.add (Vec2.scale (1 / 2) <| Vec2.sub v (Vec2.scale (1 / sf) w))
                            >> Vec2.add (vec2FromIntRecord model.layout.layout.viewBox)
                in
                flip Vec2.sub (vec2FromIntRecord element.location) << pageToSvg
        in
        (case element.element of
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

            Element.Indicator x ->
                viewIndicator element.name x

            Element.Input x ->
                viewInput element.name x

            Element.Empty ->
                stack []
        )
            |> maybe identity (impose << viewImage) element.image
            |> maybe identity (impose << viewText model.layout.layout.viewBox) element.text
            |> shift ( Basics.toFloat element.location.x, Basics.toFloat element.location.y )


viewButton : String -> Button -> Bool -> Collage Msgs
viewButton name button pressed =
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
        |> onPointerDown (always <| ButtonDown name)
            { onMove = always []
            , onRelease = [ ClientUpdate <| ButtonUp name ]
            }


viewStick : String -> Stick -> (Vec2 -> Vec2) -> Vec2 -> Collage Msgs
viewStick name stick toOffset stickPos =
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
            if length == 0 then
                vec2 0 0
                -- (== v0)

            else
                Vec2.normalize v0 |> Vec2.scale (length / range)

        big =
            circle range |> styled1 stick.backgroundColour

        small =
            circle rad |> styled1 stick.stickColour
    in
    big
        |> impose (small |> shift (unVec2 <| Vec2.scale range stickPos))
        |> onPointerDown (StickMove name << getOffset)
            { onMove = \event -> [ ClientUpdate <| StickMove name <| getOffset event ]
            , onRelease = [ ClientUpdate <| StickMove name <| vec2 0 0 ]
            }


viewSlider : String -> Slider -> (Vec2 -> Vec2) -> Float -> Collage Msgs
viewSlider name slider toOffset pos =
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
    background
        |> impose (stick |> shift (unVec2 <| Vec2.scale pos v))
        |> onPointerDown (SliderMove name << getOffset)
            { onMove = \event -> [ ClientUpdate <| SliderMove name <| getOffset event ]
            , onRelease =
                if slider.resetOnRelease then
                    [ ClientUpdate <| SliderMove name <| slider.initialPosition ]

                else
                    []
            }


viewIndicator : String -> Indicator -> Collage Msgs
viewIndicator _ ind =
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


viewInput : String -> Input -> Collage Msgs
viewInput name inp =
    let
        {- TODO
           this is a bit of a hack, to allow us to reset the form
           do we really have to go by ID - can't Elm give us a handle to the element?
        -}
        id =
            "form-" ++ name

        padding ts =
            let
                size =
                    String.fromInt (ts.size // 2) ++ "px"
            in
            [ style "padding-left" size
            , style "padding-right" size
            ]
    in
    html (both toFloat ( inp.width, inp.height )) [] <|
        form
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "width" "100%"
            , style "height" "100%"
            , onSubmit [ ClientUpdate <| SubmitInput name, ResetForm id ]
            , H.id id
            ]
            [ input
                ([ H.type_ <|
                    case inp.inputType of
                        InputType.CheckBox () ->
                            "checkbox"

                        InputType.Number _ ->
                            "number"

                        InputType.Text _ ->
                            "text"
                 , style "flex" "auto"
                 , style "width" "100%"
                 , style "height" "100%"
                 , H.map List.singleton <|
                    case inp.inputType of
                        InputType.CheckBox () ->
                            onCheck (ClientUpdate << InputBool name)

                        InputType.Number _ ->
                            onInput <|
                                \s ->
                                    case String.toInt s of
                                        Just f ->
                                            ClientUpdate <| InputNumber name f

                                        Nothing ->
                                            ConsoleLog <| "Failed to decode number input: " ++ s

                        InputType.Text _ ->
                            onInput (ClientUpdate << InputText name)
                 ]
                    ++ (case inp.inputType of
                            InputType.CheckBox () ->
                                []

                            InputType.Number opts ->
                                H.step (Maybe.unwrap "any" String.fromFloat opts.step)
                                    :: padding opts.textStyle
                                    ++ textStyle opts.textStyle
                                    ++ Maybe.unwrap [] (List.singleton << H.min << String.fromFloat) opts.min
                                    ++ Maybe.unwrap [] (List.singleton << H.max << String.fromFloat) opts.max

                            InputType.Text opts ->
                                padding opts.textStyle
                                    ++ textStyle opts.textStyle
                                    ++ Maybe.unwrap [] (List.singleton << H.minlength) opts.minLength
                                    ++ Maybe.unwrap [] (List.singleton << H.maxlength) opts.maxLength
                       )
                )
                []
            ]


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
    , encoding : Encoding
    , supportsFullscreen : Bool
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
    = ClientUpdate ClientUpdate
    | ServerUpdate ServerUpdate
    | PointerDown Int PointerCallbacks
    | PointerUp Pointer.Event
    | Resized IntVec2
    | GoFullscreen
    | FullscreenChange Bool
    | ResetForm String
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
                                          , encoding = flags.encoding
                                          , supportsFullscreen = flags.supportsFullscreen
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
        ClientUpdate u ->
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

                        InputBool _ _ ->
                            model

                        InputNumber _ _ ->
                            model

                        InputText _ _ ->
                            model

                        SubmitInput _ ->
                            model

                        Pong _ ->
                            model
            in
            ( model1, sendUpdate model.encoding u )

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

        PointerUp event ->
            ( { model | layout = { layoutState | pointerCallbacks = Dict.remove event.pointerId layoutState.pointerCallbacks } }
            , Dict.get event.pointerId model.layout.pointerCallbacks
                |> maybe [] (\c -> c.onRelease)
                |> performCmd
            )

        Resized v ->
            ( { model | windowSize = v }, Cmd.none )

        GoFullscreen ->
            ( model, goFullscreen )

        FullscreenChange b ->
            ( { model | fullscreen = b }, Cmd.none )

        ResetForm id ->
            ( model, resetForm id )

        ConsoleLog s ->
            ( model, consoleLog s )


serverUpdate : ServerUpdate -> Model -> ( Model, Cmd Msgs )
serverUpdate u model =
    -- this needs to be equivalent to the same handling in the Haskell code
    let
        layoutState =
            model.layout

        layout =
            layoutState.layout

        updateElementFull name f =
            { model
                | layout =
                    { layoutState
                        | layout =
                            { layout
                                | elements =
                                    layout.elements
                                        |> List.map
                                            (\e ->
                                                if e.name == name then
                                                    f e

                                                else
                                                    e
                                            )
                            }
                    }
            }

        updateElement name f =
            updateElementFull name <| \fe -> { fe | element = f fe.element }

        updateIndicator name f =
            updateElement name <|
                \e ->
                    case e of
                        Element.Indicator ind ->
                            Element.Indicator <| f ind

                        _ ->
                            e

        updateButton name f =
            updateElement name <|
                \e ->
                    case e of
                        Element.Button x ->
                            Element.Button <| f x

                        _ ->
                            e

        updateTextStyle name f =
            updateElementFull name <|
                \e ->
                    { e | text = e.text |> Maybe.map (\x -> { x | style = f x.style }) }
    in
    case u of
        PlayAudioURL url ->
            ( model
            , playAudio url
            )

        Vibrate intervals ->
            ( model
            , vibrate intervals
            )

        SetImageURL name url ->
            ( updateElementFull name <|
                \e ->
                    { e | image = e.image |> Maybe.map (\x -> { x | url = url }) }
            , Cmd.none
            )

        AddImage name x ->
            ( updateElementFull name <|
                \e ->
                    { e | image = Just x }
            , Cmd.none
            )

        DeleteImage name ->
            ( updateElementFull name <|
                \e ->
                    { e | image = Nothing }
            , Cmd.none
            )

        SetText name text ->
            ( updateElementFull name <|
                \e ->
                    { e | text = e.text |> Maybe.map (\x -> { x | text = text }) }
            , Cmd.none
            )

        SetTextStyle name style ->
            ( updateElementFull name <|
                \e ->
                    { e | text = e.text |> Maybe.map (\x -> { x | style = style }) }
            , Cmd.none
            )

        SetTextSize name size ->
            ( updateTextStyle name <| \s -> { s | size = size }
            , Cmd.none
            )

        SetTextColour name colour ->
            ( updateTextStyle name <| \s -> { s | colour = colour }
            , Cmd.none
            )

        SetTextBold name bold ->
            ( updateTextStyle name <| \s -> { s | bold = bold }
            , Cmd.none
            )

        SetTextItalic name italic ->
            ( updateTextStyle name <| \s -> { s | italic = italic }
            , Cmd.none
            )

        SetTextUnderline name underline ->
            ( updateTextStyle name <| \s -> { s | underline = underline }
            , Cmd.none
            )

        SetTextShadow name shadow ->
            ( updateTextStyle name <| \s -> { s | shadow = shadow }
            , Cmd.none
            )

        SetTextFont name font ->
            ( updateTextStyle name <| \s -> { s | font = font }
            , Cmd.none
            )

        AddText name x ->
            ( updateElementFull name <|
                \e ->
                    { e | text = Just x }
            , Cmd.none
            )

        DeleteText name ->
            ( updateElementFull name <|
                \e ->
                    { e | text = Nothing }
            , Cmd.none
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

        HideElement name ->
            ( updateElementFull name <| \e -> { e | hidden = True }
            , Cmd.none
            )

        ShowElement name ->
            ( updateElementFull name <| \e -> { e | hidden = False }
            , Cmd.none
            )

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
            ( updateIndicator name <| \e -> { e | hollowness = x }
            , Cmd.none
            )

        SetIndicatorArcStart name x ->
            ( updateIndicator name <| \e -> { e | arcStart = x }
            , Cmd.none
            )

        SetIndicatorArcEnd name x ->
            ( updateIndicator name <| \e -> { e | arcEnd = x }
            , Cmd.none
            )

        SetIndicatorShape name x ->
            ( updateIndicator name <| \e -> { e | shape = x }
            , Cmd.none
            )

        SetIndicatorCentre name x ->
            ( updateIndicator name <| \e -> { e | centre = x }
            , Cmd.none
            )

        SetIndicatorColour name x ->
            ( updateIndicator name <| \e -> { e | colour = x }
            , Cmd.none
            )

        SetSliderPosition name x ->
            ( { model | layout = { layoutState | sliderPos = Dict.insert name x layoutState.sliderPos } }
            , Cmd.none
            )

        SetButtonColour name x ->
            ( updateButton name <| \e -> { e | colour = x }
            , Cmd.none
            )

        SetButtonPressed name x ->
            ( { model
                | layout =
                    { layoutState
                        | pressed =
                            if x then
                                Set.insert name layoutState.pressed

                            else
                                Set.remove name layoutState.pressed
                    }
              }
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

        Ping t ->
            ( model, performCmd [ ClientUpdate <| Pong t ] )



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
    (Pointer.Event -> ClientUpdate)
    -> PointerCallbacks
    -> Collage Msgs
    -> Collage Msgs
onPointerDown f y =
    Collage.on "pointerdown"
        (Pointer.eventDecoder
            |> JD.map
                (\x ->
                    [ PointerDown x.pointerId y
                    , ClientUpdate <| f x
                    ]
                )
        )


textStyle : TextStyle -> List (Attribute msg)
textStyle s =
    [ style "font-size" <| String.fromInt s.size ++ "px"
    , style "font-weight" <| bool "normal" "bold" s.bold
    , style "font-style" <| bool "normal" "italic" s.italic
    , style "text-decoration" <| bool "none" "underline" s.underline
    , style "color" <| Color.toCssString <| fromRgba s.colour
    , style "font-family" s.font
    , style "text-align" "center"
    ]
        ++ (if List.isEmpty s.shadow then
                []

            else
                [ style "text-shadow" <|
                    String.join ", " <|
                        List.map
                            (\shadow ->
                                String.join " "
                                    [ String.fromInt shadow.offset.x ++ "px"
                                    , String.fromInt -shadow.offset.y ++ "px"
                                    , String.fromInt shadow.blur ++ "px"
                                    , Color.toCssString <| fromRgba shadow.colour
                                    ]
                            )
                            s.shadow
                ]
           )
