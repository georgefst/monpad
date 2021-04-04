module Scratch exposing (..)

{-| Useful for playing with in `elm reactor`.
-}

import Auto.Element exposing (..)
import Auto.ElmFlags exposing (..)
import Auto.ResetLayout exposing (..)
import Auto.ServerUpdate exposing (..)
import Auto.Shape exposing (..)
import Basics.Extra exposing (..)
import Color exposing (..)
import Json.Decode as Json
import List.Extra exposing (..)
import Loadable
import Main
import Math.Vector2 exposing (..)
import Maybe exposing (..)
import Time exposing (..)
import Util exposing (..)


main : Loadable.Program Json.Value Main.Model Main.Msgs Main.Error
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
                    -- Sub.batch
                    withDefault Sub.none <| getAt 0
                        [ app.subscriptions model
                        , sub 3000 30 model <|
                            \x ->
                                List.map Main.ServerUpdate
                                    [ SetIndicatorArcStart "indicator" x
                                    , SetIndicatorHollowness "indicator" x
                                    ]
                        , sub 3000 30 model <|
                            \x ->
                                List.map Main.ServerUpdate
                                    [ SetIndicatorCentre "powerbar" <| vec2 0 (2 * x - 1)
                                    , SetIndicatorArcStart "powerbar" <| 5 / 8 - x / 4
                                    , SetIndicatorArcEnd "powerbar" <| 7 / 8 + x / 4
                                    , SetIndicatorColour "powerbar" <|
                                        let
                                            c =
                                                toRgba black
                                        in
                                        { c | alpha = x }
                                    ]
                        , sub 4000 2000 model <|
                            \x ->
                                if x < 0.5 then
                                    List.map Main.ServerUpdate
                                        [ SwitchLayout "A" ]

                                else
                                    List.map Main.ServerUpdate
                                        [ SwitchLayout "B"
                                        , SetBackgroundColour <| toRgba black
                                        , ResetLayout StateReset
                                        , SetSliderPosition "0" 0.7
                                        ]
                        , sub 4000 1000 model <|
                            \x ->
                                if x < 0.25 then
                                    List.map Main.ServerUpdate
                                        [ SetImageURL "image" "https://bit.ly/38MOJxe" ]

                                else if x < 0.5 then
                                    List.map Main.ServerUpdate
                                        [ ResetLayout FullReset ]

                                else
                                    []
                        ]
        }


sub : Int -> Int -> Main.Model -> (Float -> Main.Msgs) -> Sub Main.Msgs
sub cycleLength tickMs model f =
    every (toFloat tickMs) <|
        \t ->
            let
                tDiff =
                    modBy cycleLength <| posixToMillis t - posixToMillis model.startTime
            in
            f <|
                toFloat tDiff
                    / toFloat cycleLength


flags : ElmFlags
flags =
    { username = "GT"
    , layouts =
        [ { elements =
                [ { location = { x = -600, y = 0 }
                  , image = Nothing
                  , text = Nothing
                  , name = "indicator"
                  , element =
                        Indicator
                            { hollowness = 0.5
                            , arcStart = 0
                            , arcEnd = 2 / 3
                            , centre = vec2 0 0
                            , colour = { red = 0.8, green = 0, blue = 0.5, alpha = 1 }
                            , shape = Circle 300
                            }
                  }
                , { location = { x = 900, y = 0 }
                  , image = Nothing
                  , text = Nothing
                  , name = "powerbar"
                  , element =
                        Indicator
                            { hollowness = 0
                            , arcStart = 0.5
                            , arcEnd = 1
                            , centre = vec2 0 0
                            , colour = toRgba purple
                            , shape = Rectangle { x = 100, y = 800 }
                            }
                  }
                , { location = { x = 600, y = 0 }
                  , image = Nothing
                  , text = Nothing
                  , name = "stick"
                  , element =
                        Stick
                            { radius = 80
                            , range = 300
                            , backgroundColour = toRgba blue
                            , stickColour = toRgba white
                            , stickDataX = ()
                            , stickDataY = ()
                            }
                  }
                , { location = { x = -300, y = -200 }
                  , image = Nothing
                  , text = Nothing
                  , name = "slider"
                  , element =
                        Slider
                            { radius = 40
                            , width = 80
                            , offset = { x = 600, y = 400 }
                            , initialPosition = 0.2
                            , resetOnRelease = True
                            , backgroundColour = toRgba green
                            , sliderColour = toRgba white
                            , sliderData = ()
                            }
                  }
                , { location = { x = 0, y = 350 }
                  , image = Nothing
                  , name = "button"
                  , text =
                        Just
                            { style =
                                { bold = True
                                , italic = True
                                , underline = True
                                , colour = toRgba darkGray
                                , size = 60
                                , shadow =
                                    Just
                                        { offset = { x = 2, y = -1 }
                                        , blur = 2
                                        , colour = toRgba black
                                        }
                                , font = "sans-serif"
                                }
                            , text = "c'est un button"
                            }
                  , element =
                        Button
                            { shape = Rectangle { x = 300, y = 100 }
                            , colour = toRgba yellow
                            , buttonData = ()
                            }
                  }
                , { location = { x = -500, y = 0 }
                  , name = "image"
                  , element = Empty
                  , image =
                        Just
                            { width = 1000
                            , height = 1000
                            , url = "https://upload.wikimedia.org/wikipedia/commons/c/c2/Hieronymus_prag_a.jpg"
                            }
                  , text = Nothing
                  }
                ]
          , viewBox = { x = -1000, y = -500, w = 2000, h = 1000 }
          , backgroundColour = { red = 0.81, green = 0.91, blue = 0.97, alpha = 1.0 }
          , name = "A"
          }
        , { elements =
                [ { location = { x = -600, y = 0 }
                  , image = Nothing
                  , text = Nothing
                  , name = "0"
                  , element =
                        Slider
                            { radius = 200
                            , width = 200
                            , offset = { x = 600 * 2, y = 0 }
                            , backgroundColour = toRgba red
                            , sliderColour = toRgba white
                            , resetOnRelease = True
                            , initialPosition = 0
                            , sliderData = ()
                            }
                  }
                ]
          , viewBox = { x = -1000, y = -500, w = 2000, h = 1000 }
          , backgroundColour = toRgba white
          , name = "B"
          }
        ]
    }
