port module Main exposing (main)

import Auto.Button exposing (..)
import Auto.Update exposing (..)
import Basics.Extra exposing (flip, uncurry)
import Browser exposing (..)
import Collage exposing (..)
import Collage.Events as C
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color
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


port sendUpdate :
    JE.Value
    -> Cmd msg --TODO type - update only


main : Program Flags Model Msg
main =
    document { init = init, update = update, view = view, subscriptions = always Sub.none }



{- View -}


view : Model -> Document Msg
view model =
    { title = "Gamepad"
    , body = [ viewLeft model, viewRight model ]
    }


viewLeft : Model -> Html Msg
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
                |> C.on "pointerout" (JD.succeed <| Update <| Stick <| vec2 0 0)

        -- used to extrude envelope to cover everywhere 'small' might go
        full =
            stack [ front, small |> shift (unVec2 <| Vec2.scale rBig model.stickPos), big ]
    in
    div []
        [ svg full ]


viewRight : Model -> Html Msg
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


type alias Model =
    { username : String
    , stickPos : Vec2
    , pressed : ListSet Button
    }


type Msg
    = Update Update


type alias Flags =
    --TODO JSON?
    String


init : Flags -> ( Model, Cmd Msg )
init username =
    ( { username = username
      , stickPos = vec2 0 0
      , pressed = emptyListSet
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update u ->
            let
                model1 =
                    case u of
                        ButtonUp b ->
                            { model | pressed = removeListSet b model.pressed }

                        ButtonDown b ->
                            { model | pressed = addListSet b model.pressed }

                        Stick p ->
                            { model | stickPos = p }
            in
            ( model1
            , sendUpdate <| Auto.Update.encode u
            )



-- for analog stick
--TODO make these three relative to screen size


rSmall : Float
rSmall =
    60


rBig : Float
rBig =
    175


rButton : Float
rButton =
    70