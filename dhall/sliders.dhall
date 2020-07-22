let monpad = ./monpad.dhall {} {}

let slider =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(sliderColour : monpad.Colour) →
        { element =
            monpad.Element.Slider
              { sliderData = {=}
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
        [ slider 500 500 "Left" monpad.cols.green
        , slider 1500 500 "Right" monpad.cols.red
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : monpad.Layout
