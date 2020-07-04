module Main exposing (main)

import Auto.Button exposing (..)
import Auto.Colour exposing (..)
import Auto.Element exposing (..)
import Auto.ElmFlags exposing (..)
import Auto.FullElement exposing (..)
import Auto.Layout exposing (..)
import Auto.Stick exposing (..)
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
import Ports exposing (..)
import String exposing (..)
import Tuple exposing (..)
import Util exposing (..)
import Util.HorribleCrapThatMakesMeThinkMaybeElmIsIndeedWrong.ListSet as ListSet
import Util.Prog exposing (..)


main : Program JD.Value (Result JD.Error Model) Msg
main =
    prog
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , decoder = Auto.ElmFlags.decode
        }



{- View -}


view : Model -> Document Msg
view model =
    { title = "Gamepad"
    , body =
        [ svgExplicit
            [ let
                ( x, y ) =
                    ( viewBoxSize.x, viewBoxSize.y )
              in
              viewBox -(x / 2) -(y / 2) x y

            --TODO it would be preferable to apply this to the subcomponents instead,
            -- so that we could still pan and zoom in the gaps
            -- https://github.com/timjs/elm-collage/issues/22
            , style "touch-action" "none"
            ]
          <|
            stack <|
                List.map (viewElement model) model.layout.elements
        ]
    }


viewElement : Model -> FullElement -> Collage Msg
viewElement model element =
    shift (unVec2 element.location) <|
        case element.element of
            ButtonElement b c ->
                let
                    shape =
                        case b of
                            CircleButton r ->
                                circle r

                            RectangleButton v ->
                                uncurry rectangle <| unVec2 v
                in
                shape
                    |> styled
                        ( uniform <| applyWhen (ListSet.member element.name model.pressed) darkColor <| Color.fromRgba c
                        , solid thick <| uniform black
                        )
                    |> Collage.on "pointerdown" (JD.succeed <| Update <| ButtonDown element.name)
                    |> Collage.on "pointerout" (JD.succeed <| Update <| ButtonUp element.name)

            StickElement stick ->
                viewStick model element.name stick


viewStick : Model -> String -> Stick -> Collage Msg
viewStick model name stick =
    let
        rBack =
            stick.range + stick.radius

        getOffset event =
            let
                v0 =
                    -- invert y coord and convert tuple to vector
                    uncurry vec2 <| mapSecond negate <| both (\t -> t - rBack) event.pointer.offsetPos

                length =
                    min stick.range <| Vec2.length v0
            in
            Vec2.normalize v0 |> Vec2.scale (length / stick.range)

        big =
            circle stick.range
                |> filled (uniform <| Color.fromRgba stick.backgroundColour)

        small =
            circle stick.radius |> filled (uniform <| Color.fromRgba stick.colour)

        front =
            -- invisible - area in which touches are registered
            -- used to extrude envelope to cover everywhere 'small' might go
            circle rBack
                |> filled (uniform <| hsla 0 0 0 0)
                |> Collage.on "pointermove" (JD.map (Update << StickMove name << getOffset) Pointer.eventDecoder)
                |> Collage.on "pointerout" (JD.succeed <| Update <| StickMove name <| vec2 0 0)
    in
    stack [ front, small |> shift (unVec2 <| Vec2.scale stick.range model.stickPos), big ]



{- Model -}


type alias Model =
    { username : String
    , layout : Layout
    , stickPos : Vec2
    , pressed : ListSet.Set String
    }


type Msg
    = Update Update


init : ElmFlags -> ( Model, Cmd Msg )
init flags =
    ( { username = flags.username
      , layout = flags.layout
      , stickPos = vec2 0 0
      , pressed = ListSet.empty
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
                            { model | pressed = ListSet.remove b model.pressed }

                        ButtonDown b ->
                            { model | pressed = ListSet.add b model.pressed }

                        StickMove t p ->
                            { model | stickPos = p }
            in
            ( model1
            , sendUpdate u
            )



--TODO make configurable?


{-| All constants controlling the size of SVG components.
With viewBox (w,h), the rest can be seen as lengths on a (w x h) screen.
Line length or radius unless otherwise stated.
-}
viewBoxSize : { x : Float, y : Float }
viewBoxSize =
    { x = 2000, y = 1000 }
