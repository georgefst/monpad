let Evdev = ./evdev.dhall

let Windows = ./windows.dhall

let Mac = ./mac.dhall

in  { Button = { linux : Evdev.Key, windows : Windows.Button, mac : Mac.Button }
    , Axis = { linux : Evdev.AxisInfo, windows : Windows.Axis, mac : Mac.Axis }
    , AxisLinux = Evdev.AxisInfo
    , ButtonLinux = Evdev.Key
    , AxisWindows = Windows.Axis
    , ButtonWindows = Windows.Button
    , AxisMac = Mac.Axis
    , ButtonMac = Mac.Button
    }
