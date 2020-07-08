let WG = ./WG.dhall

let button =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element = WG.Element.Button { button = WG.Button.Circle 120, colour }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ button 1250 500 "KeyB" WG.cols.blue
        , button 1500 250 "KeyG" WG.cols.green
        , button 1750 500 "KeyR" WG.cols.red
        , button 1500 750 "KeyY" WG.cols.yellow
        , { element =
              WG.Element.Stick
                { radius = 120
                , range = 320
                , stickColour = WG.cols.white
                , backgroundColour = WG.cols.black
                }
          , location = { x = 500, y = 500 }
          , name = "(AbsX, AbsY)"
          , showName = False
          }
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : WG.Layout
