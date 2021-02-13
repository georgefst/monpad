let Evdev = ./evdev.dhall

let Windows = ./windows.dhall

let Mac = ./mac.dhall

in  { Button = { linux : Evdev.Key, windows : Windows.Key, mac : Mac.Button }
    , Axis =
        { linux : Evdev.AxisInfo, windows : Windows.MouseEvent, mac : Mac.Axis }
    , AxisLinux = Evdev.AxisInfo
    , ButtonLinux = Evdev.Key
    , AxisWindows = Windows.MouseEvent
    , ButtonWindows = Windows.Key
    , AxisMac = Mac.Axis
    , ButtonMac = Mac.Button
    }
