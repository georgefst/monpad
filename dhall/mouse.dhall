let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let Evdev = ./lib/evdev.dhall

let Windows = ./lib/windows.dhall

let axis =
      λ(la : Evdev.RelAxis) →
      λ(wa : Windows.Axis) →
      λ(wt : Windows.MouseEventType) →
      λ(multiplier : Double) →
        { linux = { axis = Evdev.Axis.Rel la, multiplier }
        , windows = { axis = wa, mouseType = wt, multiplier }
        , mac = {=}
        }

let button =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(linux : Evdev.Key) →
      λ(windows : Windows.Key) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        { element =
            monpad.Element.Button
              { buttonData = { linux, windows, mac = {=} }
              , colour
              , shape = monpad.Shape.Circle 150
              }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ button -300 +1650 Evdev.Key.BtnLeft 1 "Blue" monpad.cols.blue
        , button +300 +1650 Evdev.Key.BtnRight 2 "Red" monpad.cols.red
        , { element =
              monpad.Element.Slider
                { radius = 90
                , length = 500
                , width = 80
                , sliderColour = monpad.cols.yellow
                , backgroundColour = monpad.cols.white
                , vertical = True
                , sliderData =
                    axis
                      Evdev.RelAxis.RelWheel
                      Windows.Axis.Y
                      Windows.MouseEventType.Wheel
                      5.0
                }
          , location = { x = +0, y = +1650 }
          , name = "Slider"
          , showName = False
          }
        , { element =
              monpad.Element.Stick
                { radius = 140
                , range = 420
                , stickColour = monpad.cols.white
                , backgroundColour = monpad.cols.grey
                , stickDataX =
                    axis
                      Evdev.RelAxis.RelX
                      Windows.Axis.X
                      Windows.MouseEventType.Relative
                      15.0
                , stickDataY =
                    axis
                      Evdev.RelAxis.RelY
                      Windows.Axis.Y
                      Windows.MouseEventType.Relative
                      15.0
                }
          , location = { x = +0, y = +900 }
          , name = "Stick"
          , showName = False
          }
        ]
      , viewBox = { x = -500, y = -2000, w = +1000, h = +2000 }
      }
    : monpad.Layout
