let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let Evdev = ./lib/evdev.dhall

let ButtonL = Evdev.Key

let ButtonW = {}

let axis =
      λ(a : Evdev.RelAxis) →
      λ(m : Integer) →
        { linux = { axis = Evdev.Axis.Rel a, multiplier = m }
        , windows = {=}
        , mac = {=}
        }

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
              , shape = monpad.Shape.Circle 150
              }
        , location = { x, y }
        , name
        }

let layoutAll =
        { elements =
          [ button +1650 +300 ButtonL.BtnLeft {=} "Blue" monpad.cols.blue
          , button +1650 -300 ButtonL.BtnRight {=} "Red" monpad.cols.red
          , monpad.Elem::{
            , element =
                monpad.simpleSlider
                  { radius = 90
                  , length = 600
                  , width = 80
                  , sliderColour = monpad.cols.yellow
                  , backgroundColour = monpad.cols.white
                  , vertical = False
                  , sliderData = axis Evdev.RelAxis.RelWheel +5
                  }
            , location = { x = +1300, y = +0 }
            , name = "Slider"
            }
          , monpad.Elem::{
            , element =
                monpad.Element.Stick
                  { radius = 140
                  , range = 420
                  , stickColour = monpad.cols.white
                  , backgroundColour = monpad.cols.grey
                  , stickDataX = axis Evdev.RelAxis.RelX +15
                  , stickDataY = axis Evdev.RelAxis.RelY -15
                  }
            , location = { x = +650, y = +0 }
            , name = "Stick"
            }
          ]
        , viewBox = { x = +0, y = -500, w = 2000, h = 1000 }
        , backgroundColour = monpad.cols.pastelBlue
        , name = "mouse"
        }
      : monpad.Layout

in  layoutAll
