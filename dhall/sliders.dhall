let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let ButtonL = AllOS.ButtonLinux

let AxisL = AllOS.AxisLinux

let slider =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(sliderColour : monpad.Colour) →
      λ(linux : AxisL) →
        { element =
            monpad.Element.Slider
              { sliderData = { linux, windows = {=}, mac = {=} }
              , length = 700
              , width = 100
              , radius = 100
              , backgroundColour = monpad.cols.white
              , vertical = True
              , sliderColour
              }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ slider 500 500 "Left" monpad.cols.green AxisL.AbsX
        , slider 1500 500 "Right" monpad.cols.red AxisL.AbsRx
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : monpad.Layout
