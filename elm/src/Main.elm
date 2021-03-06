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
import Auto.ServerUpdate exposing (..)
import Auto.Shape exposing (..)
import Auto.Slider exposing (..)
import Auto.Stick exposing (..)
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
import Maybe.Extra as Maybe
import Platform.Cmd as Cmd
import Ports exposing (..)
import Set exposing (Set)
import Task
import Time exposing (..)
import Tuple exposing (..)
import Util exposing (..)


main : Loadable.Program JD.Value Model Msgs JD.Error
main =
    Loadable.application app


app :
    { load : JD.Value -> a -> b -> Task.Task JD.Error ( Model, Cmd Msgs )
    , update : Msgs -> Model -> ( Model, Cmd Msgs )
    , view : Model -> Document Msgs
    , subscriptions : Model -> Sub Msgs
    , failCmd : Maybe c
    , loadingView : Maybe d
    , errorView : Maybe (JD.Error -> { title : String, body : List (Html msg) })
    , onUrlRequest : e -> Msgs
    , onUrlChange : f -> Msgs
    }
app =
    { load =
        \f _ _ ->
            case JD.decodeValue Auto.ElmFlags.decode f of
                Err e ->
                    Task.fail e

                Ok flags ->
                    load flags
    , update =
        flip <|
            \model ->
                mapSecond Cmd.batch
                    << foldr
                        (\msg ( m, cs ) -> mapSecond (\c -> c :: cs) (update msg m))
                        ( model, [] )
    , view = view
    , subscriptions =
        always <|
            Sub.batch
                [ Sub.map
                    (either
                        (\err -> [ ConsoleLog <| "Failed to decode update message: " ++ JD.errorToString err ])
                        (List.map ServerUpdate)
                    )
                    receiveUpdates
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
                    , div [] []
                    , Html.text <| JD.errorToString e
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
                model.layout.viewBox

            fullscreenButton =
                let
                    -- how much of the screen to cover
                    scale =
                        1 / 10

                    size =
                        toFloat (min w h) * scale
                in
                rectangle size size
                    |> styled1 (toRgba orange)
                    |> shift ( toFloat x + size / 2, toFloat (h + y) - size / 2 )
                    |> Collage.on "pointerdown" (JD.succeed [ Fullscreen ])
        in
        [ div
            [ style "background-color" <| toCssString <| fromRgba model.layout.backgroundColour
            ]
            [ svgExplicit
                [ viewBox x -(h + y) w h
                , style "touch-action" "none"
                , Pointer.onMove <|
                    \event ->
                        maybe [] (\c -> c.onMove event) <|
                            Dict.get event.pointerId model.pointerCallbacks
                , Pointer.onLeave <|
                    \event ->
                        PointerUp event.pointerId
                            :: (maybe [] (\c -> c.onRelease) <| Dict.get event.pointerId model.pointerCallbacks)

                --TODO reading and reacting to changes seems ugly - is there no native API for "fill screen"?
                -- if there is, we may not need "overflow: hidden" in app.css
                , style "width" (String.fromInt model.windowSize.x ++ "px")
                , style "height" (String.fromInt model.windowSize.y ++ "px")
                ]
              <|
                stack <|
                    fullscreenButton
                        :: List.map (viewElement model) model.layout.elements
            ]
        ]
    }


showName : String -> Collage msg
showName name =
    Text.fromString name
        |> Text.size Text.large
        |> Text.color Color.darkRed
        |> Text.shape Text.Italic
        |> Collage.rendered


viewElement : Model -> FullElement -> Collage Msgs
viewElement model element =
    let
        toOffset =
            let
                bottomLeft =
                    vec2FromIntRecord model.layout.viewBox

                pageToSvg =
                    scaleVec2
                        { sfX = toFloat model.layout.viewBox.w / toFloat model.windowSize.x
                        , sfY = -(toFloat model.layout.viewBox.h / toFloat model.windowSize.y)
                        }
                        >> Vec2.add bottomLeft
                        >> Vec2.add (vec2 0 <| toFloat model.layout.viewBox.h)
            in
            flip Vec2.sub (vec2FromIntRecord element.location) << pageToSvg
    in
    shift ( Basics.toFloat element.location.x, Basics.toFloat element.location.y ) <|
        applyWhen element.showName (impose <| showName element.name) <|
            case element.element of
                Element.Button x ->
                    viewButton element.name x <| Set.member element.name model.pressed

                Element.Stick x ->
                    viewStick element.name x toOffset <|
                        withDefault zeroVec2 <|
                            Dict.get element.name model.stickPos

                Element.Slider x ->
                    viewSlider element.name x toOffset <|
                        withDefault x.initialPosition <|
                            Dict.get element.name model.sliderPos

                Element.Image x ->
                    viewImage element.name x <| withDefault x.url <| Dict.get element.name model.imageToUrl

                Element.Indicator x ->
                    viewIndicator element.name x


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
        |> Collage.on "pointerdown" (JD.succeed [ Update <| ButtonDown name ])
        |> Collage.on "pointerout" (JD.succeed [ Update <| ButtonUp name ])


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
            Vec2.normalize v0 |> Vec2.scale (length / range)

        big =
            circle range |> styled1 stick.backgroundColour

        small =
            circle rad |> styled1 stick.stickColour
    in
    stack [ small |> shift (unVec2 <| Vec2.scale range stickPos), big ]
        |> Collage.on "pointerdown"
            (Pointer.eventDecoder
                |> JD.map
                    (\x ->
                        [ PointerDown x.pointerId
                            { onMove = \event -> [ Update <| StickMove name <| getOffset event ]
                            , onRelease = [ Update <| StickMove name <| vec2 0 0 ]
                            }
                        , Update <| StickMove name <| getOffset x
                        ]
                    )
            )


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
            roundedRectangle width (Vec2.length v) (width / 2)
                |> styled1 slider.backgroundColour
                |> rotate (angleVec2 v - pi / 2)
                |> shift (unVec2 <| Vec2.scale (1 / 2) v)
    in
    stack [ stick |> shift (unVec2 <| Vec2.scale pos v), background ]
        |> Collage.on "pointerdown"
            (Pointer.eventDecoder
                |> JD.map
                    (\x ->
                        [ PointerDown x.pointerId
                            { onMove = \event -> [ Update <| SliderMove name <| getOffset event ]
                            , onRelease =
                                if slider.resetOnRelease then
                                    [ Update <| SliderMove name <| slider.initialPosition ]

                                else
                                    []
                            }
                        , Update <| SliderMove name <| getOffset x
                        ]
                    )
            )


viewImage : String -> Image -> String -> Collage Msgs
viewImage _ img url =
    image (both toFloat ( img.width, img.height )) url


viewIndicator : String -> Indicator -> Collage Msgs
viewIndicator _ ind =
    let
        a =
            ind.arcStart

        b =
            if ind.arcEnd < ind.arcStart then
                ind.arcEnd + 2 * pi

            else
                ind.arcEnd

        -- values in [0, 4pi) where we need a vertex
        angles =
            takeWhile (\x -> x < b) <|
                dropWhile (\x -> x < a) <|
                    case ind.shape of
                        Rectangle _ ->
                            range 0 7
                                |> List.map (\x -> (2 * toFloat x + 1) * pi / 4)

                        Circle _ ->
                            let
                                --TODO higher res? the whole thing is a hack really because of
                                -- https://github.com/timjs/elm-collage/issues/8#issuecomment-776603367
                                nPoints =
                                    256
                            in
                            range 0 (2 * nPoints - 1)
                                |> List.map (\x -> toFloat x * 2 * pi / nPoints)

        outer =
            (a :: angles ++ [ b ])
                |> List.map
                    (\t ->
                        case ind.shape of
                            Rectangle v ->
                                let
                                    mod1 x =
                                        x - toFloat (floor x)

                                    {- there's nothing I can tell you about this function that you can't get
                                        from typing in to Wolfram Alpha:
                                       min(1, max(-1, ((abs(mod(x / (2 * pi), 1) - 0.5) * 2) - 0.5) * 4))
                                    -}
                                    f x =
                                        limit ( -1, 1 ) <| ((abs (mod1 (x / (2 * pi)) - 0.5) * 2) - 0.5) * 4
                                in
                                ( toFloat v.x / 2 * f t, toFloat v.y / 2 * f (t - pi / 2) )

                            Circle r ->
                                let
                                    r1 =
                                        toFloat r
                                in
                                ( r1 * cos t, r1 * sin t )
                    )

        inner =
            outer
                |> List.map (both (\x -> x * ind.hollowness))
    in
    (reverse outer ++ inner)
        |> polygon
        |> styled1 ind.colour



{- Model -}


type alias Model =
    { username : String
    , layout : Layout
    , windowSize : IntVec2
    , stickPos : Dict String Vec2
    , pointerCallbacks : Dict Int { onMove : Pointer.Event -> Msgs, onRelease : Msgs } -- keyed by pointer id
    , pressed : Set String
    , sliderPos : Dict String Float
    , imageToUrl : Dict String String
    , startTime : Posix
    }


type alias Msgs =
    List Msg


type Msg
    = Update Update
    | ServerUpdate ServerUpdate
    | PointerDown Int { onMove : Pointer.Event -> Msgs, onRelease : Msgs }
    | PointerUp Int
    | Resized IntVec2
    | Fullscreen
    | ConsoleLog String


load : ElmFlags -> Task.Task JD.Error ( Model, Cmd Msgs )
load flags =
    now
        |> Task.andThen
            (\startTime ->
                getViewportSize
                    |> Task.andThen
                        (\viewport ->
                            Task.succeed <|
                                let
                                    imageToUrl =
                                        flags.layout.elements
                                            |> filterMap
                                                (\e ->
                                                    case e.element of
                                                        Element.Image img ->
                                                            Just ( e.name, img.url )

                                                        _ ->
                                                            Nothing
                                                )
                                            |> Dict.fromList
                                in
                                ( { username = flags.username
                                  , layout = flags.layout
                                  , windowSize = viewport
                                  , stickPos = Dict.empty
                                  , pointerCallbacks = Dict.empty
                                  , pressed = Set.empty
                                  , sliderPos = Dict.empty
                                  , imageToUrl = imageToUrl
                                  , startTime = startTime
                                  }
                                , Cmd.none
                                )
                        )
            )


update : Msg -> Model -> ( Model, Cmd Msgs )
update msg model =
    case msg of
        Update u ->
            let
                model1 =
                    case u of
                        ButtonUp b ->
                            { model | pressed = Set.remove b model.pressed }

                        ButtonDown b ->
                            { model | pressed = Set.insert b model.pressed }

                        StickMove t p ->
                            { model | stickPos = Dict.insert t p model.stickPos }

                        SliderMove t p ->
                            { model | sliderPos = Dict.insert t p model.sliderPos }
            in
            ( model1, sendUpdate u )

        ServerUpdate u ->
            ( serverUpdate u model, Cmd.none )

        PointerDown pid callbacks ->
            ( { model | pointerCallbacks = Dict.insert pid callbacks model.pointerCallbacks }, Cmd.none )

        PointerUp pid ->
            ( { model | pointerCallbacks = Dict.remove pid model.pointerCallbacks }, Cmd.none )

        Resized v ->
            ( { model | windowSize = v }, Cmd.none )

        Fullscreen ->
            ( model, toggleFullscreen )

        ConsoleLog s ->
            ( model, consoleLog s )


serverUpdate : ServerUpdate -> Model -> Model
serverUpdate u model =
    let
        layout =
            model.layout

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
            { model | imageToUrl = Dict.insert image url model.imageToUrl }

        SetLayout l ->
            { model | layout = l }

        AddElement e ->
            { model | layout = { layout | elements = e :: layout.elements } }

        RemoveElement e ->
            { username = model.username
            , windowSize = model.windowSize
            , stickPos = Dict.remove e model.stickPos
            , pointerCallbacks = model.pointerCallbacks
            , pressed = Set.remove e model.pressed
            , sliderPos = Dict.remove e model.sliderPos
            , imageToUrl = Dict.remove e model.imageToUrl
            , startTime = model.startTime
            , layout =
                { layout
                    | elements =
                        layout.elements
                            |> List.filterMap
                                (\e1 ->
                                    if e1.name == e then
                                        Nothing

                                    else
                                        Just e1
                                )
                }
            }

        SetBackgroundColour c ->
            { model
                | layout = { layout | backgroundColour = c }
            }

        SetIndicatorHollowness name x ->
            { model
                | layout = updateIndicator name (\e -> { e | hollowness = x })
            }

        SetIndicatorArcStart name x ->
            { model
                | layout = updateIndicator name (\e -> { e | arcStart = x })
            }

        SetIndicatorArcEnd name x ->
            { model
                | layout = updateIndicator name (\e -> { e | arcEnd = x })
            }

        SetIndicatorShape name x ->
            { model
                | layout = updateIndicator name (\e -> { e | shape = x })
            }


styled1 : Colour -> Collage.Shape -> Collage msg
styled1 c =
    styled ( uniform <| fromRgba c, defaultLineStyle )
