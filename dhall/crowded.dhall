-- SDL_GAMECONTROLLERCONFIG="000000004d5000004d50000000000000,Monpad,platform:Linux,a:b0,b:b1,x:b3,y:b2,back:b6,guide:b8,start:b7,leftshoulder:b4,rightshoulder:b5,dpup:b9,dpdown:b10,dpleft:b11,dpright:b12,leftx:a0,lefty:a1,rightx:a3,righty:a4,lefttrigger:-a2,righttrigger:-a5,"

let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let Evdev = ./lib/evdev.dhall

let Abs = Evdev.AbsAxis

let ButtonL = AllOS.ButtonLinux

let AxisL = AllOS.AxisLinux

let axis =
      λ(a : Evdev.AbsAxis) → { axis = Evdev.Axis.Abs a, multiplier = 255.0 }

let button =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(linux : ButtonL) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        { element =
            monpad.Element.Button
              { buttonData = { linux, windows = {=}, mac = {=} }
              , colour
              , shape = monpad.Shape.Circle 100
              }
        , location = { x, y }
        , name
        , showName = False
        }

let button2 =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(linux : ButtonL) →
      λ(name : Text) →
      λ(showName : Bool) →
        { element =
            monpad.Element.Button
              { buttonData = { linux, windows = {=}, mac = {=} }
              , colour = monpad.cols.grey
              , shape = monpad.Shape.Rectangle { x = 150, y = 150 }
              }
        , location = { x, y }
        , name
        , showName
        }

let stick =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(name : Text) →
      λ(linuxX : AxisL) →
      λ(linuxY : AxisL) →
        { element =
            monpad.Element.Stick
              { radius = 80
              , range = 200
              , stickColour = monpad.cols.white
              , backgroundColour = monpad.cols.grey
              , stickDataX = { linux = linuxX, windows = {=}, mac = {=} }
              , stickDataY = { linux = linuxY, windows = {=}, mac = {=} }
              }
        , location = { x, y }
        , name
        , showName = False
        }

let slider =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(name : Text) →
      λ(linux : AxisL) →
        { element =
            monpad.Element.Slider
              { length = 400
              , width = 90
              , radius = 80
              , backgroundColour = monpad.cols.white
              , sliderColour = monpad.cols.black
              , sliderData = { linux, windows = {=}, mac = {=} }
              , vertical = True
              }
        , location = { x, y }
        , name
        , showName = False
        }

in    { elements =
        [ button +160 +180 ButtonL.BtnWest "Blue" monpad.cols.blue
        , button +360 -20 ButtonL.BtnSouth "Green" monpad.cols.green
        , button +560 +180 ButtonL.BtnEast "Red" monpad.cols.red
        , button +360 +380 ButtonL.BtnNorth "Yellow" monpad.cols.yellow
        , button -560 +180 ButtonL.BtnDpadLeft "Left" monpad.cols.grey
        , button -360 -20 ButtonL.BtnDpadDown "Down" monpad.cols.grey
        , button -160 +180 ButtonL.BtnDpadRight "Right" monpad.cols.grey
        , button -360 +380 ButtonL.BtnDpadUp "Up" monpad.cols.grey
        , stick -700 -200 "Left" (axis Abs.AbsX) (axis Abs.AbsY)
        , stick +700 -200 "Right" (axis Abs.AbsRx) (axis Abs.AbsRy)
        , slider -850 +250 "LT" (axis Abs.AbsZ)
        , slider +850 +250 "RT" (axis Abs.AbsRz)
        , button2 +0 -100 ButtonL.BtnMode "Mode" True
        , button2 -200 -300 ButtonL.BtnSelect "Select" True
        , button2 +200 -300 ButtonL.BtnStart "Start" True
        , button2 -640 +400 ButtonL.BtnTl "LB" False
        , button2 +640 +400 ButtonL.BtnTr "RB" False
        ]
      , viewBox = { x = -1000, y = -500, w = +2000, h = +1000 }
      }
    : monpad.Layout
