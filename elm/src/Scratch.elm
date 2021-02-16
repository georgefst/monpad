module Scratch exposing (..)

import Auto.Element exposing (..)
import Auto.ElmFlags exposing (..)
import Auto.ServerUpdate exposing (..)
import Auto.Shape exposing (..)
import Draggable exposing (subscriptions)
import Json.Decode as Json
import Loadable
import Main
import Time exposing (..)


main : Loadable.Program Json.Value Main.Model Main.Msg Json.Error
main =
    let
        app =
            Main.app
    in
    Loadable.application
        { app
            | load = \_ _ _ -> Main.load flags
            , subscriptions =
                \model ->
                    Sub.batch
                        [ sub (\x -> SetIndicatorArcStart "0" <| x * 2 * pi) model
                        , sub (\x -> SetIndicatorHollowness "0" x) model
                        ]
        }


{-| time in milliseconds to perform a full loop
-}
cycleLength : number
cycleLength =
    3000


{-| milliseconds per tick
-}
tick : number
tick =
    30


sub : (Float -> ServerUpdate) -> Main.Model -> Sub Main.Msg
sub f model =
    every tick <|
        \t ->
            let
                tDiff =
                    modBy cycleLength <| posixToMillis t - posixToMillis model.startTime
            in
            Main.ServerUpdate <|
                f <|
                    toFloat tDiff
                        / cycleLength


flags : ElmFlags
flags =
    { username = "GT"
    , layout =
        { elements =
            [ { location = { x = 0, y = 0 }
              , name = "0"
              , showName = False
              , element =
                    Indicator
                        { hollowness = 0.5
                        , arcStart = 0
                        , arcEnd = 4 * pi / 3
                        , colour = { red = 0.8, green = 0, blue = 0.5, alpha = 1 }
                        , shape = Circle 350
                        }
              }
            ]
        , viewBox = { x = -1000, y = -500, w = 2000, h = 1000 }
        }
    }
