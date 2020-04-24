module Main exposing (main)

import Auto.Button exposing (..)
import Auto.Endpoints exposing (..)
import Auto.Update exposing (..)
import Basics.Extra exposing (flip, uncurry)
import Browser exposing (..)
import Collage exposing (..)
import Collage.Events as C
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color
import Config exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Http
import Json.Decode as JD
import Json.Encode as JE
import List exposing (head)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Maybe exposing (withDefault)
import String exposing (words)
import Tuple exposing (first, mapSecond)
import Util exposing (..)
import Util.HorribleCrapThatMakesMeThinkMaybeElmIsIndeedWrong exposing (..)


main : Program () Model Msg
main =
    let
        init =
            case username of
                Nothing ->
                    initFull

                Just s ->
                    initSimple s
    in
    document { init = init, update = update, view = view, subscriptions = always Sub.none }



{- View -}


view : Model -> Document Msg
view model =
    case model of
        MainState m ->
            viewMain m

        FailedState s ->
            viewFail s

        InitialState m ->
            viewInitial m


viewFail : String -> Document Msg
viewFail s =
    { title = "Not good"
    , body = [ text s ]
    }



--TODO style better


viewInitial : InitModel -> Document Msg
viewInitial model =
    { title = "Enter details"
    , body =
        [ table
            []
            [ div []
                [ text "Username: "
                , input
                    [ onInput EnteredUserName
                    ]
                    []
                ]
            , div []
                [ button
                    [ onClick StartMain
                    ]
                    [ text "Go!"
                    ]
                ]
            , div []
                [ text model.error ]
            ]
        ]
    }


viewMain : MainModel -> Document Msg
viewMain model =
    { title = "Controller"
    , body =
        [ Html.node "meta"
            -- disables zooming, so that we can have sane multi-touch
            [ Attr.attribute "name" "viewport" --TODO 0.7 is totally arbitrary - designed for George's phone
            , Attr.attribute "content" "width=device-width, initial-scale=0.7, maximum-scale=0.7, user-scalable=0"
            ]
            []
        , div
            [ Attr.style "display" "flex"
            , Attr.style "width" "100%"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "space-evenly"
            , Attr.style "width" "100%"
            , Attr.style "height" "100vh"
            , Attr.style "background-color" "#cee7f7"
            ]
            [ viewLeft model, viewRight model ]
        ]
    }


viewLeft : MainModel -> Html Msg
viewLeft model =
    let
        rBack =
            rBig + rSmall

        getOffset event =
            let
                v0 =
                    -- invert y coord and convert tuple to vector
                    uncurry vec2 <| mapSecond negate <| both (\t -> t - rBack) event.pointer.offsetPos

                length =
                    min rBig <| Vec2.length v0

                v =
                    Vec2.normalize v0 |> Vec2.scale (length / rBig)
            in
            Update <| Stick v

        big =
            circle rBig
                |> filled (uniform Color.darkCharcoal)

        small =
            circle rSmall |> filled (uniform Color.lightPurple)

        front =
            -- invisible - area in which touches are registered
            circle rBack
                |> filled (uniform <| Color.hsla 0 0 0 0)
                |> C.on "pointermove" (JD.map getOffset Pointer.eventDecoder)

        -- used to extrude envelope to cover everywhere 'small' might go
        full =
            stack [ front, small |> shift (unVec2 <| Vec2.scale rBig model.stickPos), big ]
    in
    div []
        [ svg full ]


viewRight : MainModel -> Html Msg
viewRight model =
    let
        buts =
            [ ( ( 0, 0 ), Blue )
            , ( ( 1, 1 ), Yellow )
            , ( ( 2, 0 ), Red )
            , ( ( 1, -1 ), Green )
            ]

        diameter =
            2 * rButton

        buttonCol1 b =
            if memberListSet b model.pressed then
                buttonCol b |> darkColor

            else
                buttonCol b
    in
    List.map
        (\( ( x, y ), c ) ->
            circle rButton
                |> styled
                    ( uniform <| buttonCol1 c
                    , solid thick (uniform Color.black)
                    )
                |> C.on "pointerdown" (JD.succeed <| Update <| ButtonDown c)
                |> C.on "pointerout" (JD.succeed <| Update <| ButtonUp c)
                |> shift ( x * diameter, y * diameter )
        )
        buts
        |> group
        |> svg


buttonCol : Button -> Color.Color
buttonCol b =
    case b of
        Blue ->
            Color.blue

        Yellow ->
            Color.yellow

        Red ->
            Color.red

        Green ->
            Color.green



{- Model -}


type Model
    = MainState MainModel
    | InitialState InitModel
    | FailedState String


type alias MainModel =
    { username : String
    , stickPos : Vec2
    , pressed : ListSet Button
    }


type alias InitModel =
    { username : String
    , error : String
    }


{-| no startup screen - use the given username
-}
initFull : flags -> ( Model, Cmd Msg )
initFull _ =
    ( InitialState
        { username = ""
        , error = ""
        }
    , Cmd.none
    )


{-| no startup screen - use the given username
-}
initSimple : String -> flags -> ( Model, Cmd Msg )
initSimple u _ =
    ( MainState
        { username = u
        , stickPos = vec2 0 0
        , pressed = emptyListSet
        }
    , Cmd.none
    )


type Msg
    = Update Update
    | ServerResponse ServerResponse
    | EnteredUserName String
    | StartMain


type alias ServerResponse =
    Result
        ( Http.Error
        , Maybe
            { metadata : Http.Metadata
            , body : String
            }
        )
        ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        FailedState s ->
            ( FailedState s, Cmd.none )

        InitialState m ->
            ( updateInit msg m, Cmd.none )

        MainState m ->
            Tuple.mapFirst MainState <| updateMain msg m


updateInit : Msg -> InitModel -> Model
updateInit msg model =
    case msg of
        EnteredUserName s ->
            InitialState { model | username = s }

        --TODO validate (contact server, ask whether name is taken)
        StartMain ->
            case model.username of
                "" ->
                    InitialState { model | error = "please enter a username" }

                u ->
                    MainState
                        { username = u
                        , stickPos = vec2 0 0
                        , pressed = emptyListSet
                        }

        _ ->
            FailedState "multi-page"


updateMain : Msg -> MainModel -> ( MainModel, Cmd Msg )
updateMain msg model =
    case msg of
        ServerResponse _ ->
            --TODO do something with this (option to display on side for now?)
            ( model
            , Cmd.none
            )

        Update u ->
            let
                model1 =
                    case u of
                        ButtonUp b ->
                            { model | pressed = removeListSet b <| model.pressed }

                        ButtonDown b ->
                            { model | pressed = addListSet b <| model.pressed }

                        Stick p ->
                            { model | stickPos = p }
            in
            ( model1
            , Cmd.map ServerResponse <| postUpdateById model.username u
            )

        _ ->
            --TODO remove case when multi-page (use Browser.application ?)
            ( model, Cmd.none )
