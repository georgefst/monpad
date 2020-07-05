let WG = ./WG.dhall

let button =
      λ(_1 : Double) →
      λ(_2 : Double) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element =
            WG.Element.Button { button = WG.Button.Circle 120.0, colour }
        , location = { _1, _2 }
        , name
        }

in    { elements =
        [ button 250.0 0.0 "Blue" WG.cols.blue
        , button 500.0 -250.0 "Green" WG.cols.green
        , button 750.0 0.0 "Red" WG.cols.red
        , button 500.0 250.0 "Yellow" WG.cols.yellow
        , { element =
              WG.Element.Stick
                { radius = 120.0
                , range = 320.0
                , stickColour = WG.cols.white
                , backgroundColour = WG.cols.black
                }
          , location = { _1 = -500.0, _2 = 0.0 }
          , name = "Main"
          }
        ]
      }
    : WG.Layout
