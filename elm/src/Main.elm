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
                [ Sub.map (maybe [] (List.map ServerUpdate)) receiveUpdates
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
        [ div
            [ style "background-color" <| toCssString <| fromRgba model.layout.backgroundColour
            ]
            [ svgExplicit
                [ let
                    { x, y, w, h } =
                        model.layout.viewBox
                  in
                  viewBox x y w h
                , style "touch-action" "none"
                , Pointer.onMove <|
                    \event -> Maybe.toList <| Maybe.map (\x -> x.onMove event) <| Dict.get event.pointerId model.stickId
                , Pointer.onLeave <|
                    \event ->
                        PointerUp event.pointerId
                            :: (Maybe.toList <| Maybe.map (\x -> x.onRelease) <| Dict.get event.pointerId model.stickId)
                ]
              <|
                stack <|
                    List.map (viewElement model) model.layout.elements
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
    shift ( Basics.toFloat element.location.x, Basics.toFloat element.location.y ) <|
        applyWhen element.showName (impose <| showName element.name) <|
            case element.element of
                Element.Button x ->
                    viewButton element.name x <| Set.member element.name model.pressed

                Element.Stick x ->
                    viewStick element.name x element.location model.windowSize <|
                        withDefault zeroVec2 <|
                            Dict.get element.name model.stickPos

                Element.Slider x ->
                    viewSlider element.name x <| withDefault 0 <| Dict.get element.name model.sliderPos

                Element.Image x ->
                    viewImage element.name x <| Maybe.withDefault x.url <| Dict.get element.name model.imageToUrl

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
            ( uniform <| applyWhen pressed darkColor <| Color.fromRgba button.colour
            , solid thick <| uniform black
            )
        |> Collage.on "pointerdown" (JD.succeed [ Update <| ButtonDown name ])
        |> Collage.on "pointerout" (JD.succeed [ Update <| ButtonUp name ])


viewStick : String -> Stick -> IntVec2 -> IntVec2 -> Vec2 -> Collage Msgs
viewStick name stick location windowSize stickPos =
    let
        range =
            toFloat stick.range

        rad =
            toFloat stick.radius

        rFront =
            range + rad

        getOffset event =
            let
                v0 =
                    flip Vec2.sub
                        (vec2
                            (toFloat windowSize.x + toFloat location.x)
                            (toFloat windowSize.y + toFloat location.y)
                        )
                    <|
                        Vec2.scale 2 <|
                            uncurry vec2 event.pointer.pagePos

                length =
                    min range <| Vec2.length v0
            in
            Vec2.normalize v0 |> Vec2.scale (length / range)

        big =
            circle range |> styled1 stick.backgroundColour

        small =
            circle rad |> styled1 stick.stickColour

        front =
            let
                decode =
                    Pointer.eventDecoder
                        |> JD.map
                            (\x ->
                                [ PointerDown x.pointerId
                                    { onMove = \event -> Update <| StickMove name <| getOffset event
                                    , onRelease = Update <| StickMove name <| vec2 0 0
                                    }
                                ]
                            )
            in
            -- invisible - area in which touches are registered
            -- used to extrude envelope to cover everywhere 'small' might go
            circle rFront
                |> filled (uniform <| hsla 0 0 0 0)
                |> Collage.on "pointerdown" decode
    in
    stack [ front, small |> shift (mapSecond negate <| unVec2 <| Vec2.scale range stickPos), big ]


viewSlider : String -> Slider -> Float -> Collage Msgs
viewSlider name slider pos =
    let
        length =
            toFloat slider.length

        width =
            toFloat slider.width

        rad =
            toFloat slider.radius

        ( getCoord, shiftSlider, ( sizeX, sizeY ) ) =
            if slider.vertical then
                ( second, shiftY, ( width, length ) )

            else
                ( first, shiftX, ( length, width ) )

        diam =
            2 * rad

        getOffset event =
            applyWhen slider.vertical negate <|
                limit ( 0, 1 ) ((getCoord event.pointer.offsetPos - rad) / length)
                    * 2
                    - 1

        stick =
            circle rad
                |> styled1 slider.sliderColour

        background =
            roundedRectangle sizeX sizeY (width / 2)
                |> styled1 slider.backgroundColour

        front =
            let
                decode =
                    JD.map (Update << SliderMove name << getOffset)
                        Pointer.eventDecoder
            in
            -- as with Stick, represents movement area
            rectangle (sizeX + diam) (sizeY + diam)
                |> filled (uniform <| hsla 0 0 0 0)
                |> Collage.on "pointermove" (JD.map List.singleton decode)
                |> Collage.on "pointerdown" (JD.map List.singleton decode)
                |> Collage.on "pointerout" (JD.succeed [ Update <| SliderMove name 0 ])
    in
    stack [ front, stick |> shiftSlider (pos * length / 2), background ]


viewImage : String -> Image -> String -> Collage Msgs
viewImage _ img url =
    image (both toFloat ( img.width, img.height )) url


viewIndicator : String -> Indicator -> Collage Msgs
viewIndicator _ ind =
    let
        --TODO higher res? lower for rectangle? the whole thing is a hack really because of
        -- https://github.com/timjs/elm-collage/issues/8#issuecomment-776603367
        nPoints =
            256

        a =
            ind.arcStart

        b =
            if ind.arcEnd < ind.arcStart then
                ind.arcEnd + 2 * pi

            else
                ind.arcEnd

        -- regular intervals in [0, 4pi)
        angles =
            range 0 (2 * nPoints - 1)
                |> List.map (\x -> toFloat x * 2 * pi / nPoints)
                |> dropWhile (\x -> x < a)
                |> (\x -> a :: x)
                |> takeWhile (\x -> x < b)
                |> (\x -> x ++ [ b ])

        outer =
            angles
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
    , stickId : Dict Int { onMove : Pointer.Event -> Msg, onRelease : Msg } -- pointer id to event callbacks
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
    | PointerDown Int { onMove : Pointer.Event -> Msg, onRelease : Msg }
    | PointerUp Int
    | Resized IntVec2


load : ElmFlags -> Task.Task JD.Error ( Model, Cmd Msgs )
load flags =
    now
        |> Task.andThen
            (\startTime ->
                getViewport
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
                                  , windowSize =
                                        { x = round viewport.viewport.width, y = round viewport.viewport.height }
                                  , stickPos = Dict.empty
                                  , stickId = Dict.empty
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
            ( { model | stickId = Dict.insert pid callbacks model.stickId }, Cmd.none )

        PointerUp pid ->
            ( { model | stickId = Dict.remove pid model.stickId }, Cmd.none )

        Resized v ->
            ( { model | windowSize = v }, Cmd.none )


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
            , stickId = model.stickId
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
    styled ( uniform <| Color.fromRgba c, defaultLineStyle )
