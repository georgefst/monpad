let WG = ./WG.dhall

let Evdev = ./evdev.dhall

let Axis = Evdev.AbsAxis

let Key = Evdev.Key

let Element = WG.Element Axis Key

let Layout = WG.Layout Axis Key

let button =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(buttonData : Key) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element =
            Element.Button { buttonData, colour, button = WG.Button.Circle 120 }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ button 1250 500 Key.KeyB "Blue" WG.cols.blue
        , button 1500 250 Key.KeyG "Green" WG.cols.green
        , button 1750 500 Key.KeyR "Red" WG.cols.red
        , button 1500 750 Key.KeyY "Yellow" WG.cols.yellow
        , { element =
              Element.Stick
                { radius = 120
                , range = 320
                , stickColour = WG.cols.white
                , backgroundColour = WG.cols.black
                , stickDataX = Axis.AbsX
                , stickDataY = Axis.AbsY
                }
          , location = { x = 500, y = 500 }
          , name = "Stick"
          , showName = False
          }
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : Layout
