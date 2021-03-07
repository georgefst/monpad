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
        [ button +300 +100 ButtonL.Key0 "0"
        , button +100 +300 ButtonL.Key1 "1"
        , button +300 +300 ButtonL.Key2 "2"
        , button +500 +300 ButtonL.Key3 "3"
        , button +100 +500 ButtonL.Key4 "4"
        , button +300 +500 ButtonL.Key5 "5"
        , button +500 +500 ButtonL.Key6 "6"
        , button +100 +700 ButtonL.Key7 "7"
        , button +300 +700 ButtonL.Key8 "8"
        , button +500 +700 ButtonL.Key9 "9"
        ]
      , viewBox = { x = +0, y = +0, w = +600, h = +800 }
      , backgroundColour = monpad.cols.pastelBlue
      , name = "numpad"
      }
    : monpad.Layout
