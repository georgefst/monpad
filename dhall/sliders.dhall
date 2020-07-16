let WG = ./WG.dhall {} {}

let slider =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(sliderColour : WG.Colour) →
        { element =
            WG.Element.Slider
              { sliderData = {=}
              , rangeX = 700
              , rangeY = 100
              , radius = 100
              , backgroundColour = WG.cols.white
              , sliderColour
              }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ slider 500 500 "Left" WG.cols.green
        , slider 1500 500 "Right" WG.cols.red
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : WG.Layout
