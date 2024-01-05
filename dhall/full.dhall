let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let Evdev = ./lib/evdev.dhall

let Abs = Evdev.AbsAxis

let ButtonL = AllOS.ButtonLinux

let AxisL = AllOS.AxisLinux

let axis =
      λ(a : Evdev.AbsAxis) →
      λ(multiplier : Integer) →
        { axis = Evdev.Axis.Abs a, multiplier }

let button =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(linux : ButtonL) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        monpad.Elem::{
        , element =
            monpad.Element.Button
              { buttonData = { linux, windows = {=}, mac = {=} }
              , colour
              , shape = monpad.Shape.Circle 100
              }
        , location = { x, y }
        , name
        }

let button2 =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(linux : ButtonL) →
      λ(name : Text) →
      λ(showName : Bool) →
        monpad.Elem::{
        , element =
            monpad.Element.Button
              { buttonData = { linux, windows = {=}, mac = {=} }
              , colour = monpad.cols.white
              , shape = monpad.Shape.Rectangle { x = 150, y = 150 }
              }
        , location = { x, y }
        , name
        , text =
            if    showName
            then  Some
                    { style = monpad.defaultTextStyle
                    , text = name
                    , alignX = monpad.PosX.Centre
                    , alignY = monpad.PosY.Middle
                    }
            else  None monpad.TextBox
        }

let stick =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(name : Text) →
      λ(linuxX : AxisL) →
      λ(linuxY : AxisL) →
        monpad.Elem::{
        , element =
            monpad.Element.Stick
              { radius = 80
              , range = 220
              , stickColour = monpad.cols.white
              , backgroundColour = monpad.cols.grey
              , stickDataX = { linux = linuxX, windows = {=}, mac = {=} }
              , stickDataY = { linux = linuxY, windows = {=}, mac = {=} }
              }
        , location = { x, y }
        , name
        }

let slider =
      λ(x : Integer) →
      λ(y : Integer) →
      λ(name : Text) →
      λ(linux : AxisL) →
        monpad.Elem::{
        , element =
            monpad.Element.Slider
              { width = 130
              , radius = 80
              , backgroundColour = monpad.cols.white
              , sliderColour = monpad.cols.black
              , sliderData = { linux, windows = {=}, mac = {=} }
              , offset = { x = +0, y = +300 }
              , initialPosition = 0.0
              , resetOnRelease = True
              }
        , location = { x, y }
        , name
        }

in    { elements =
        [ button +160 +140 ButtonL.BtnY "Blue" monpad.cols.blue
        , button +360 -60 ButtonL.BtnA "Green" monpad.cols.green
        , button +560 +140 ButtonL.BtnB "Red" monpad.cols.red
        , button +360 +340 ButtonL.BtnX "Yellow" monpad.cols.yellow
        , button -560 +140 ButtonL.BtnDpadLeft "Left" monpad.cols.grey
        , button -360 -60 ButtonL.BtnDpadDown "Down" monpad.cols.grey
        , button -160 +140 ButtonL.BtnDpadRight "Right" monpad.cols.grey
        , button -360 +340 ButtonL.BtnDpadUp "Up" monpad.cols.grey
        , stick -710 -210 "StickL" (axis Abs.AbsX +255) (axis Abs.AbsY -255)
        , stick +710 -210 "StickR" (axis Abs.AbsRx +255) (axis Abs.AbsRy -255)
        , slider -850 +100 "LT" (axis Abs.AbsZ +255)
        , slider +850 +100 "RT" (axis Abs.AbsRz +255)
        , button2 +0 -150 ButtonL.BtnMode "Mode" True
        , button2 -200 -350 ButtonL.BtnSelect "Select" True
        , button2 +200 -350 ButtonL.BtnStart "Start" True
        , button2 -640 +400 ButtonL.BtnTl "LB" False
        , button2 +640 +400 ButtonL.BtnTr "RB" False
        ]
      , viewBox = { x = -1000, y = -500, w = 2000, h = 1000 }
      , backgroundColour = monpad.cols.pastelBlue
      , name = "gamepad"
      }
    : monpad.Layout
