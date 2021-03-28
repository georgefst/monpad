let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let Evdev = ./lib/evdev.dhall

let ButtonL = Evdev.Key

let ButtonW = {}

let axis =
      λ(a : Evdev.RelAxis) →
      λ(m : Double) →
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
          [ button -300 +1650 ButtonL.BtnLeft {=} "Blue" monpad.cols.blue
          , button +300 +1650 ButtonL.BtnRight {=} "Red" monpad.cols.red
          , monpad.Elem::{
            , element =
                monpad.simpleSlider
                  { radius = 90
                  , length = 600
                  , width = 80
                  , sliderColour = monpad.cols.yellow
                  , backgroundColour = monpad.cols.white
                  , vertical = True
                  , sliderData = axis Evdev.RelAxis.RelWheel 5.0
                  }
            , location = { x = +0, y = +1300 }
            , name = "Slider"
            }
          , monpad.Elem::{
            , element =
                monpad.Element.Stick
                  { radius = 140
                  , range = 420
                  , stickColour = monpad.cols.white
                  , backgroundColour = monpad.cols.grey
                  , stickDataX = axis Evdev.RelAxis.RelX 15.0
                  , stickDataY = axis Evdev.RelAxis.RelY -15.0
                  }
            , location = { x = +0, y = +650 }
            , name = "Stick"
            }
          ]
        , viewBox = { x = -500, y = +0, w = +1000, h = +2000 }
        , backgroundColour = monpad.cols.pastelBlue
        , name = "mouse"
        }
      : monpad.Layout

in  layoutAll
