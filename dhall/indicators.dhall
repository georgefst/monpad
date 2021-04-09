let AllOS = ./lib/all-os.dhall

let Prelude = ./lib/Prelude.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let buttonData =
      { linux = AllOS.ButtonLinux.KeyUnknown, windows = {=}, mac = {=} }

let IncompleteElement =
      { element : monpad.Element, location : monpad.V2 Integer, name : Text }

in    { elements =
          Prelude.List.map
            IncompleteElement
            monpad.FullElement
            ( λ(e : IncompleteElement) →
                  e
                ∧ { image = None monpad.Image
                  , text = Some
                    { text = e.name, style = monpad.defaultTextStyle }
                  , hidden = False
                  }
            )
            [ { element =
                  monpad.Element.Indicator
                    { arcStart = 0.02
                    , arcEnd = 0.7
                    , colour = monpad.cols.white ⫽ { alpha = 0.4 }
                    , hollowness = 0.0
                    , centre = { x = 0.5, y = 0.15 }
                    , shape = monpad.Shape.Circle 100
                    }
              , location = { x = -800, y = +0 }
              , name = "5"
              }
            , { element =
                  monpad.Element.Button
                    { colour = monpad.cols.red
                    , buttonData
                    , shape = monpad.Shape.Circle 100
                    }
              , location = { x = -800, y = +0 }
              , name = "6"
              }
            , { element =
                  monpad.Element.Indicator
                    { arcStart = 0.0
                    , arcEnd = 0.5
                    , colour = monpad.cols.yellow
                    , hollowness = 0.2
                    , centre = { x = 0.0, y = 0.0 }
                    , shape = monpad.Shape.Rectangle { x = 100, y = 800 }
                    }
              , location = { x = -500, y = +0 }
              , name = "0"
              }
            , { element =
                  monpad.Element.Indicator
                    { arcStart = 0.7
                    , arcEnd = 1.3
                    , colour = monpad.cols.green
                    , hollowness = 0.95
                    , centre = { x = 0.9, y = 0.9 }
                    , shape = monpad.Shape.Circle 400
                    }
              , location = { x = +0, y = +0 }
              , name = "1"
              }
            , { element =
                  monpad.Element.Indicator
                    { arcStart = 0.7
                    , arcEnd = 1.3
                    , colour = monpad.cols.blue
                    , hollowness = 0.0
                    , centre = { x = 0.0, y = 0.0 }
                    , shape = monpad.Shape.Rectangle { x = 100, y = 800 }
                    }
              , location = { x = +500, y = +0 }
              , name = "2"
              }
            , { element =
                  monpad.Element.Indicator
                    { arcStart = 0.02
                    , arcEnd = 0.7
                    , colour = monpad.cols.white ⫽ { alpha = 0.4 }
                    , hollowness = 0.0
                    , centre = { x = 0.0, y = 0.0 }
                    , shape = monpad.Shape.Rectangle { x = 100, y = 800 }
                    }
              , location = { x = +800, y = +0 }
              , name = "3"
              }
            , { element =
                  monpad.Element.Button
                    { colour = monpad.cols.red
                    , buttonData
                    , shape = monpad.Shape.Rectangle { x = 100, y = 800 }
                    }
              , location = { x = +800, y = +0 }
              , name = "4"
              }
            ]
      , viewBox = { x = -1000, y = -500, w = 2000, h = 1000 }
      , backgroundColour = monpad.cols.pastelBlue
      , name = "indicators"
      }
    : monpad.Layout
