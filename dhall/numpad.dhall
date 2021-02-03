let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let ButtonL = AllOS.ButtonLinux

let button =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(linux : ButtonL) →
      λ(name : Text) →
        { element =
            monpad.Element.Button
              { buttonData = { linux, windows = {=}, mac = {=} }
              , colour = monpad.cols.white
              , shape = monpad.Shape.Rectangle { x = 200, y = 200 }
              }
        , location = { x, y }
        , name
        , showName = True
        }

in    { elements =
        [ button +400 +100 ButtonL.Key0 "0"
        , button +200 +300 ButtonL.Key1 "1"
        , button +400 +300 ButtonL.Key2 "2"
        , button +600 +300 ButtonL.Key3 "3"
        , button +200 +500 ButtonL.Key4 "4"
        , button +400 +500 ButtonL.Key5 "5"
        , button +600 +500 ButtonL.Key6 "6"
        , button +200 +700 ButtonL.Key7 "7"
        , button +400 +700 ButtonL.Key8 "8"
        , button +600 +700 ButtonL.Key9 "9"
        ]
      , viewBox = { x = +0, y = -800, w = +800, h = +800 }
      }
    : monpad.Layout
