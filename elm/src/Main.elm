port module Main exposing (main)

import Auto.Button exposing (..)
import Auto.Colour exposing (..)
import Auto.ElmFlags exposing (..)
import Auto.Layout exposing (..)
import Auto.Update exposing (..)
import Basics.Extra exposing (..)
import Browser exposing (..)
import Collage exposing (..)
import Collage.Events as Collage
import Collage.Layout exposing (..)
import Collage.Render exposing (svgExplicit)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as JD
import Json.Encode as JE
import List exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Maybe exposing (..)
import String exposing (..)
import Tuple exposing (..)
import Util exposing (..)
import Util.HorribleCrapThatMakesMeThinkMaybeElmIsIndeedWrong exposing (..)


port sendUpdate :
    JE.Value
    -> Cmd msg --TODO type - update only


main : Program JD.Value FullModel Msg
main =
    document { init = fullInit, update = fullUpdate, view = fullView, subscriptions = always Sub.none }



{- Error handling -}


type alias FullModel =
    Result Error Model


type Error
    = FlagError JD.Error


showError : Error -> String
showError error =
    case error of
        FlagError e ->
            JD.errorToString e


fullInit : JD.Value -> ( FullModel, Cmd Msg )
fullInit flags =
    case JD.decodeValue Auto.ElmFlags.decode flags of
        Ok f ->
            mapFirst Ok <| init f

        Err e ->
            ( Err <| FlagError e, Cmd.none )


fullUpdate : Msg -> Result Error Model -> ( Result Error Model, Cmd Msg )
fullUpdate m r =
    case r of
        Ok x ->
            mapFirst Ok <| update m x

        Err e ->
            ( Err e, Cmd.none )


fullView : FullModel -> Document Msg
fullView model =
    case model of
        Ok m ->
            view m

        Err e ->
            { title = "Whoops"
            , body = [ text <| showError e ]
            }



{- View -}


view : Model -> Document Msg
view model =
    { title = "Gamepad"
    , body =
        [ svgExplicit
            [ let
                ( x, y ) =
                    ( sizes.viewBox.x, sizes.viewBox.y )
              in
              viewBox -(x / 2) -(y / 2) x y

            --TODO it would be preferable to apply this to the subcomponents instead,
            -- so that we could still pan and zoom in the gaps
            -- https://github.com/timjs/elm-collage/issues/22
            , style "touch-action" "none"
            ]
          <|
            center <|
                horizontal [ viewStick model, spacer sizes.space 0, viewColourButtons model ]
        ]
    }


viewStick : Model -> Collage Msg
viewStick model =
    let
        rBack =
            sizes.stickRange + sizes.stick

        getOffset event =
            let
                v0 =
                    -- invert y coord and convert tuple to vector
                    uncurry vec2 <| mapSecond negate <| both (\t -> t - rBack) event.pointer.offsetPos

                length =
                    min sizes.stickRange <| Vec2.length v0
            in
            Vec2.normalize v0 |> Vec2.scale (length / sizes.stickRange)

        big =
            circle sizes.stickRange
                |> filled (uniform darkCharcoal)

        small =
            circle sizes.stick |> filled (uniform lightPurple)

        front =
            -- invisible - area in which touches are registered
            -- used to extrude envelope to cover everywhere 'small' might go
            circle rBack
                |> filled (uniform <| hsla 0 0 0 0)
                |> Collage.on "pointermove" (JD.map (Update << Stick << getOffset) Pointer.eventDecoder)
                |> Collage.on "pointerout" (JD.succeed <| Update <| Stick <| vec2 0 0)
    in
    stack [ front, small |> shift (unVec2 <| Vec2.scale sizes.stickRange model.stickPos), big ]


viewColourButtons : Model -> Collage Msg
viewColourButtons model =
    let
        buts =
            [ ( ( 0, 0 ), Blue )
            , ( ( 1, 1 ), Yellow )
            , ( ( 2, 0 ), Red )
            , ( ( 1, -1 ), Green )
            ]

        diameter =
            2 * sizes.button
    in
    List.map
        (\( ( x, y ), b ) ->
            circle sizes.button
                |> viewButton model b
                |> shift ( x * diameter, y * diameter )
        )
        buts
        |> group


viewButton : Model -> Button -> Shape -> Collage Msg
viewButton model button shape =
    let
        col =
            if memberListSet button model.pressed then
                buttonColour model.layout button |> darkColor

            else
                buttonColour model.layout button
    in
    shape
        |> styled
            ( uniform col
            , solid thick <| uniform black
            )
        |> Collage.on "pointerdown" (JD.succeed <| Update <| ButtonDown button)
        |> Collage.on "pointerout" (JD.succeed <| Update <| ButtonUp button)


buttonColour : Layout -> Button -> Color
buttonColour l b =
    case b of
        Blue ->
            Color.fromRgba l.colourBlue

        Yellow ->
            Color.fromRgba l.colourYellow

        Red ->
            Color.fromRgba l.colourRed

        Green ->
            Color.fromRgba l.colourGreen



{- Model -}


type alias Model =
    { username : String
    , layout : Layout
    , stickPos : Vec2
    , pressed : ListSet Button
    }


type Msg
    = Update Update


init : ElmFlags -> ( Model, Cmd Msg )
init flags =
    ( { username = flags.username
      , layout = flags.layout
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


{-| All constants controlling the size of SVG components.
With viewBox (w,h), the rest can be seen as lengths on a (w x h) screen.
Line length or radius unless otherwise stated.
-}
sizes :
    --TODO 'Config' module?
    { viewBox : { x : Float, y : Float }
    , stick : Float
    , stickRange : Float
    , button : Float
    , space : Float
    }
sizes =
    { viewBox = { x = 2000, y = 1000 }
    , stick = 120
    , stickRange = 320
    , button = 120
    , space = 80
    }
