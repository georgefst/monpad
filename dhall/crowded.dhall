-- SDL_GAMECONTROLLERCONFIG="000000004d5000004d50000000000000,Monpad,platform:Linux,a:b0,b:b1,x:b3,y:b2,back:b6,guide:b8,start:b7,leftshoulder:b4,rightshoulder:b5,dpup:b9,dpdown:b10,dpleft:b11,dpright:b12,leftx:a0,lefty:a1,rightx:a3,righty:a4,lefttrigger:-a2,righttrigger:-a5,"

let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let ButtonL = AllOS.ButtonLinux

let AxisL = AllOS.AxisLinux

let button =
      λ(x : Natural) →
      λ(y : Natural) →
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
      λ(x : Natural) →
      λ(y : Natural) →
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
      λ(x : Natural) →
      λ(y : Natural) →
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
      λ(x : Natural) →
      λ(y : Natural) →
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
        [ button 1160 680 ButtonL.BtnWest "Blue" monpad.cols.blue
        , button 1360 480 ButtonL.BtnSouth "Green" monpad.cols.green
        , button 1560 680 ButtonL.BtnEast "Red" monpad.cols.red
        , button 1360 880 ButtonL.BtnNorth "Yellow" monpad.cols.yellow
        , button 440 680 ButtonL.BtnDpadLeft "Left" monpad.cols.grey
        , button 640 480 ButtonL.BtnDpadDown "Down" monpad.cols.grey
        , button 840 680 ButtonL.BtnDpadRight "Right" monpad.cols.grey
        , button 640 880 ButtonL.BtnDpadUp "Up" monpad.cols.grey
        , stick 300 300 "Left" AxisL.AbsX AxisL.AbsY
        , stick 1700 300 "Right" AxisL.AbsRx AxisL.AbsRy
        , slider 150 750 "LT" AxisL.AbsZ
        , slider 1850 750 "RT" AxisL.AbsRz
        , button2 1000 400 ButtonL.BtnMode "Mode" True
        , button2 800 200 ButtonL.BtnSelect "Select" True
        , button2 1200 200 ButtonL.BtnStart "Start" True
        , button2 360 900 ButtonL.BtnTl "LB" False
        , button2 1640 900 ButtonL.BtnTr "RB" False
        ]
      , grid = { x = 2000, y = 1000 }
      }
    : monpad.Layout
