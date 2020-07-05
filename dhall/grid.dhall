let WG = ./WG.dhall

let tile =
      λ(_1 : Double) →
      λ(_2 : Double) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element =
            WG.Element.Button
              { button = WG.Button.Rectangle { _1 = 1000.0, _2 = 500.0 }
              , colour
              }
        , location = { _1, _2 }
        , name
        }

in    { elements =
        [ tile -500.0 -250.0 "Bottom Left" WG.cols.blue
        , tile 500.0 -250.0 "Bottom Right" WG.cols.red
        , tile -500.0 250.0 "Top Left" WG.cols.green
        , tile 500.0 250.0 "Top Right" WG.cols.yellow
        ]
      }
    : WG.Layout
