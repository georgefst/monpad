module Main exposing (main)

import Auto.Colour exposing (..)
import Auto.Element exposing (..)
import Auto.ElmFlags exposing (..)
import Auto.FullElement exposing (..)
import Auto.IntVec2 as IntVec2 exposing (IntVec2)
import Auto.Layout exposing (..)
import Auto.ServerUpdate exposing (..)
import Auto.Shape exposing (..)
import Auto.Update exposing (..)
import Basics exposing (..)
import Basics.Extra exposing (..)
import Browser exposing (..)
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
import Json.Encode as JE
import List exposing (..)
import List.Extra exposing (..)
import Loadable
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Maybe exposing (..)
import Ports exposing (..)
import Set exposing (Set)
import Svg.Attributes exposing (mode)
import Task
import Time exposing (..)
import Tuple exposing (..)
import Util exposing (..)


main : Loadable.Program JD.Value Model Msg JD.Error
main =
    Loadable.application
        { load = \f _ _ -> load f
        , update = update
        , view = view
        , subscriptions = always <| Sub.map (maybe EmptyMsg ServerUpdate) receiveUpdate
        , failCmd = Nothing
        , loadingView = Nothing
        , errorView =
            Just <|
                \e ->
                    { title = "Whoops"
                    , body = [ Html.text <| JD.errorToString e ]
                    }
        , onUrlRequest = always EmptyMsg
        , onUrlChange = always EmptyMsg
        }



{- View -}


view : Model -> Document Msg
view model =
    { title = "monpad"
    , body =
        [ svgExplicit
            [ let
                { x, y, w, h } =
                    model.layout.viewBox
              in
              viewBox x y w h
            , style "touch-action" "none"
            ]
          <|
            stack <|
                List.map (viewElement model) model.layout.elements
        ]
    }


showName : String -> Collage msg
showName name =
    Text.fromString name
        |> Text.size Text.large
        |> Text.color Color.darkRed
        |> Text.shape Text.Italic
        |> Collage.rendered


viewElement : Model -> FullElement -> Collage Msg
viewElement model element =
    shift ( Basics.toFloat element.location.x, Basics.toFloat element.location.y ) <|
        applyWhen element.showName (impose <| showName element.name) <|
            case element.element of
                Button b ->
                    let
                        shape =
                            case b.shape of
                                Circle r ->
                                    circle <| toFloat r

                                Rectangle v ->
                                    rectangle (Basics.toFloat v.x) (Basics.toFloat v.y)
                    in
                    shape
                        |> styled
                            ( uniform <|
                                applyWhen (Set.member element.name model.pressed) darkColor <|
                                    Color.fromRgba b.colour
                            , solid thick <| uniform black
                            )
                        |> Collage.on "pointerdown" (JD.succeed <| Update <| ButtonDown element.name)
                        |> Collage.on "pointerout" (JD.succeed <| Update <| ButtonUp element.name)

                Stick stick ->
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
                                    uncurry vec2 <| both (\t -> t - rFront) event.pointer.offsetPos

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
                                    JD.map (Update << StickMove element.name << getOffset)
                                        Pointer.eventDecoder
                            in
                            -- invisible - area in which touches are registered
                            -- used to extrude envelope to cover everywhere 'small' might go
                            circle rFront
                                |> filled (uniform <| hsla 0 0 0 0)
                                |> Collage.on "pointermove" decode
                                |> Collage.on "pointerdown" decode
                                |> Collage.on "pointerout" (JD.succeed <| Update <| StickMove element.name <| zeroVec2)

                        pos =
                            withDefault zeroVec2 <| Dict.get element.name model.stickPos
                    in
                    stack [ front, small |> shift (mapSecond negate <| unVec2 <| Vec2.scale range pos), big ]

                Slider s ->
                    let
                        length =
                            toFloat s.length

                        width =
                            toFloat s.width

                        rad =
                            toFloat s.radius

                        ( getCoord, shiftSlider, ( sizeX, sizeY ) ) =
                            if s.vertical then
                                ( second, shiftY, ( width, length ) )

                            else
                                ( first, shiftX, ( length, width ) )

                        diam =
                            2 * rad

                        getOffset event =
                            applyWhen s.vertical negate <|
                                limit ( 0, 1 ) ((getCoord event.pointer.offsetPos - rad) / length)
                                    * 2
                                    - 1

                        slider =
                            circle rad
                                |> styled1 s.sliderColour

                        background =
                            roundedRectangle sizeX sizeY (width / 2)
                                |> styled1 s.backgroundColour

                        front =
                            let
                                decode =
                                    JD.map (Update << SliderMove element.name << getOffset)
                                        Pointer.eventDecoder
                            in
                            -- as with Stick, represents movement area
                            rectangle (sizeX + diam) (sizeY + diam)
                                |> filled (uniform <| hsla 0 0 0 0)
                                |> Collage.on "pointermove" decode
                                |> Collage.on "pointerdown" decode
                                |> Collage.on "pointerout" (JD.succeed <| Update <| SliderMove element.name 0)

                        pos =
                            withDefault 0 <| Dict.get element.name model.sliderPos
                    in
                    stack [ front, slider |> shiftSlider (pos * length / 2), background ]

                Image img ->
                    image (both toFloat ( img.width, img.height )) <|
                        Maybe.withDefault img.url <|
                            Dict.get element.name model.imageToUrl

                Indicator ind ->
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

                        r =
                            toFloat ind.radius

                        -- regular intervals in [0, 4pi)
                        angles =
                            range 0 (2 * nPoints - 1)
                                |> List.map (\x -> toFloat x * 2 * pi / nPoints)
                                |> dropWhile (\x -> x < a)
                                |> (\x -> [ a ] ++ x)
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
                                                ( toFloat r * cos t, toFloat r * sin t )
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
    , stickPos : Dict String Vec2
    , pressed : Set String
    , sliderPos : Dict String Float
    , imageToUrl : Dict String String
    , startTime : Posix
    }


type Msg
    = Update Update
    | ServerUpdate ServerUpdate
    | EmptyMsg


load : JD.Value -> Task.Task JD.Error ( Model, Cmd Msg )
load jsonFlags =
    case JD.decodeValue Auto.ElmFlags.decode jsonFlags of
        Err e ->
            Task.fail e

        Ok flags ->
            now
                |> Task.andThen
                    (\startTime ->
                        Task.succeed <|
                            let
                                imageToUrl =
                                    flags.layout.elements
                                        |> filterMap
                                            (\e ->
                                                case e.element of
                                                    Image img ->
                                                        Just ( e.name, img.url )

                                                    _ ->
                                                        Nothing
                                            )
                                        |> Dict.fromList
                            in
                            ( { username = flags.username
                              , layout = flags.layout
                              , stickPos = Dict.empty
                              , pressed = Set.empty
                              , sliderPos = Dict.empty
                              , imageToUrl = imageToUrl
                              , startTime = startTime
                              }
                            , Cmd.none
                            )
                    )


update : Msg -> Model -> ( Model, Cmd Msg )
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
            ( model1
            , sendUpdate u
            )

        ServerUpdate u ->
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
                                                Indicator ind ->
                                                    { fe | element = Indicator <| f ind }

                                                _ ->
                                                    fe

                                        else
                                            fe
                                    )
                    }
            in
            case u of
                SetImageURL image url ->
                    ( { model | imageToUrl = Dict.insert image url model.imageToUrl }, Cmd.none )

                SetLayout l ->
                    ( { model | layout = l }
                    , Cmd.none
                    )

                AddElement e ->
                    ( { model | layout = { layout | elements = e :: layout.elements } }
                    , Cmd.none
                    )

                RemoveElement e ->
                    ( { username = model.username
                      , stickPos = Dict.remove e model.stickPos
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
                    , Cmd.none
                    )

                SetIndicatorHollowness name x ->
                    ( { model
                        | layout = updateIndicator name (\e -> { e | hollowness = x })
                      }
                    , Cmd.none
                    )

                SetIndicatorArcStart name x ->
                    ( { model
                        | layout = updateIndicator name (\e -> { e | arcStart = x })
                      }
                    , Cmd.none
                    )

                SetIndicatorArcEnd name x ->
                    ( { model
                        | layout = updateIndicator name (\e -> { e | arcEnd = x })
                      }
                    , Cmd.none
                    )

        EmptyMsg ->
            ( model, Cmd.none )


styled1 : Colour -> Collage.Shape -> Collage msg
styled1 c =
    styled ( uniform <| Color.fromRgba c, defaultLineStyle )
