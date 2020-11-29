let Axis = {}

let Key = {}

let monpad = ./monpad.dhall Axis Key

let button =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(buttonData : Key) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        { element =
            monpad.Element.Button
              { buttonData, colour, shape = monpad.Shape.Circle 120 }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ button 1250 500 {=} "Blue" monpad.cols.blue
        , button 1500 250 {=} "Green" monpad.cols.green
        , button 1750 500 {=} "Red" monpad.cols.red
        , button 1500 750 {=} "Yellow" monpad.cols.yellow
        , { element =
              monpad.Element.Button
                { buttonData = {=}
                , colour = monpad.cols.white
                , shape = monpad.Shape.Circle 80
                }
          , location = { x = 1000, y = 200 }
          , name = "M"
          , showName = True
          }
        , { element =
              monpad.Element.Stick
                { radius = 120
                , range = 320
                , stickColour = monpad.cols.white
                , backgroundColour = monpad.cols.grey
                , stickDataX = {=}
                , stickDataY = {=}
                }
          , location = { x = 500, y = 500 }
          , name = "Stick"
          , showName = False
          }
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : monpad.Layout
