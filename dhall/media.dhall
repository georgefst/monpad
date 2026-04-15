let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let Evdev = ./lib/evdev.dhall

let ButtonL = AllOS.ButtonLinux

let ButtonW = {}

let button =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(linux : ButtonL) →
      λ(windows : ButtonW) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        monpad.Elem::{
        , element =
            monpad.Element.Button
              { buttonData = { linux, windows, mac = {=} }
              , colour
              , shape = monpad.Shape.Circle 340
              }
        , location = { x, y }
        , text = Some
          { alignX = monpad.PosX.Centre
          , alignY = monpad.PosY.Middle
          , style = monpad.noTextStyle ⫽ { size = 100 }
          , text = "1"
          }
        , name
        }

in    { elements =
        [ button +0 +0 ButtonL.KeyPlaypause {=} "Blue" monpad.cols.blue ]
      , viewBox = { x = -1000, y = -1000, w = 2000, h = 2000 }
      , backgroundColour = monpad.cols.pastelBlue
      , name = "gamepad"
      }
    : monpad.Layout
