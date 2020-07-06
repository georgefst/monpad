let WG = ./WG.dhall

let tile =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element =
            WG.Element.Button
              { button = WG.Button.Rectangle { x = 1000, y = 500 }, colour }
        , location = { x, y }
        , name
        }

in    { elements =
        [ tile 500 250 "Bottom Left" WG.cols.blue
        , tile 1500 250 "Bottom Right" WG.cols.red
        , tile 500 750 "Top Left" WG.cols.green
        , tile 1500 750 "Top Right" WG.cols.yellow
        ]
      }
    : WG.Layout
