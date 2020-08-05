let Evdev = ./evdev.dhall

let Axis = Evdev.AbsAxis

let Key = Evdev.Key

let monpad = ./monpad.dhall Evdev.AbsAxis Evdev.Key

let button =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(buttonData : Key) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        { element =
            monpad.Element.Button
              { buttonData, colour, shape = monpad.Shape.Circle 100 }
        , location = { x, y }
        , name
        , showName = False
        }

let button2 =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(buttonData : Key) →
      λ(name : Text) →
      λ(showName : Bool) →
        { element =
            monpad.Element.Button
              { buttonData
              , colour = monpad.cols.grey
              , shape = monpad.Shape.Rectangle { x = 150, y = 150 }
              }
        , location = { x, y }
        , name
        , showName
        }

let stick =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(stickDataX : Axis) →
      λ(stickDataY : Axis) →
        { element =
            monpad.Element.Stick
              { radius = 80
              , range = 200
              , stickColour = monpad.cols.white
              , backgroundColour = monpad.cols.grey
              , stickDataX
              , stickDataY
              }
        , location = { x, y }
        , name
        , showName = False
        }

let slider =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(sliderData : Axis) →
        { element =
            monpad.Element.Slider
              { length = 400
              , width = 90
              , radius = 80
              , backgroundColour = monpad.cols.white
              , sliderColour = monpad.cols.black
              , sliderData
              , vertical = True
              }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ button 1160 680 Key.BtnWest "Blue" monpad.cols.blue
        , button 1360 480 Key.BtnSouth "Green" monpad.cols.green
        , button 1560 680 Key.BtnEast "Red" monpad.cols.red
        , button 1360 880 Key.BtnNorth "Yellow" monpad.cols.yellow
        , button 440 680 Key.BtnDpadLeft "Left" monpad.cols.grey
        , button 640 480 Key.BtnDpadDown "Down" monpad.cols.grey
        , button 840 680 Key.BtnDpadRight "Right" monpad.cols.grey
        , button 640 880 Key.BtnDpadUp "Up" monpad.cols.grey
        , stick 300 300 "Left" Axis.AbsX Axis.AbsY
        , stick 1700 300 "Right" Axis.AbsRx Axis.AbsRy
        , slider 150 750 "LT" Axis.AbsZ
        , slider 1850 750 "RT" Axis.AbsRz
        , button2 1000 400 Key.BtnMode "Mode" True
        , button2 800 200 Key.BtnSelect "Select" True
        , button2 1200 200 Key.BtnStart "Start" True
        , button2 360 900 Key.BtnTl "LB" False
        , button2 1640 900 Key.BtnTr "RB" False
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : monpad.Layout
