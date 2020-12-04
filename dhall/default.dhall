let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let ButtonL = AllOS.ButtonLinux

let ButtonW = {}

let AxisL = AllOS.AxisLinux

let button =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(linux : ButtonL) →
      λ(windows : ButtonW) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        { element =
            monpad.Element.Button
              { buttonData = { linux, windows }
              , colour
              , shape = monpad.Shape.Circle 120
              }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ button 1250 500 ButtonL.BtnWest {=} "Blue" monpad.cols.blue
        , button 1500 250 ButtonL.BtnSouth {=} "Green" monpad.cols.green
        , button 1750 500 ButtonL.BtnEast {=} "Red" monpad.cols.red
        , button 1500 750 ButtonL.BtnNorth {=} "Yellow" monpad.cols.yellow
        , { element =
              monpad.Element.Button
                { buttonData = { linux = ButtonL.BtnMode, windows = {=} }
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
                , stickDataX = { linux = AxisL.AbsX, windows = {=} }
                , stickDataY = { linux = AxisL.AbsY, windows = {=} }
                }
          , location = { x = 500, y = 500 }
          , name = "Stick"
          , showName = False
          }
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : monpad.Layout
