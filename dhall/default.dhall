let WG = ./WG.dhall

let button =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element =
            WG.Element.Button { button = WG.Button.Circle 120.0, colour }
        , location = { x, y }
        , name
        }

in    { elements =
        [ button +250 +0 "Blue" WG.cols.blue
        , button +500 -250 "Green" WG.cols.green
        , button +750 +0 "Red" WG.cols.red
        , button +500 +250 "Yellow" WG.cols.yellow
        , { element =
              WG.Element.Stick
                { radius = 120.0
                , range = 320.0
                , stickColour = WG.cols.white
                , backgroundColour = WG.cols.black
                }
          , location = { x = -500, y = +0 }
          , name = "Main"
          }
        ]
      }
    : WG.Layout
