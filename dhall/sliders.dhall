let Evdev = ./evdev.dhall

let Axis = Evdev.AbsAxis

let monpad = ./monpad.dhall Evdev.AbsAxis Evdev.Key

let slider =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(sliderColour : monpad.Colour) →
      λ(sliderData : Axis) →
        { element =
            monpad.Element.Slider
              { sliderData
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
        [ slider 500 500 "Left" monpad.cols.green Axis.AbsX
        , slider 1500 500 "Right" monpad.cols.red Axis.AbsX
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : monpad.Layout
