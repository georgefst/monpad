let Evdev = ./evdev.dhall

let Key = Evdev.Key

let WG = ./WG.dhall Evdev.AbsAxis Evdev.Key

let button =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(buttonData : Key) →
      λ(name : Text) →
        { element =
            WG.Element.Button
              { buttonData
              , colour = WG.cols.white
              , shape = WG.Shape.Rectangle { x = 200, y = 200 }
              }
        , location = { x, y }
        , name
        , showName = True
        }

in    { elements =
        [ button 400 100 Key.Key0 "0"
        , button 200 300 Key.Key1 "1"
        , button 400 300 Key.Key2 "2"
        , button 600 300 Key.Key3 "3"
        , button 200 500 Key.Key4 "4"
        , button 400 500 Key.Key5 "5"
        , button 600 500 Key.Key6 "6"
        , button 200 700 Key.Key7 "7"
        , button 400 700 Key.Key8 "8"
        , button 600 700 Key.Key9 "9"
        ]
      , grid = { x = 800, y = 800 }
      }
    : WG.Layout
