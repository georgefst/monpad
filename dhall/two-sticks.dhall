let WG = ./WG.dhall

let stick =
      λ(name : Text) →
      λ(location : WG.Vec2) →
        { element =
            WG.Element.Stick
              { radius = 120
              , range = 320
              , stickColour = WG.cols.white
              , backgroundColour = WG.cols.black
              }
        , location
        , name
        , showName = False
        }

in    { elements =
        [ stick "Left" { x = 500, y = 500 }
        , stick "Right" { x = 1500, y = 500 }
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : WG.Layout
