let Evdev = ./evdev.dhall

let Windows = ./windows.dhall

in  { Button = { linux : Evdev.Key, windows : Windows.Button }
    , Axis = { linux : Evdev.AbsAxis, windows : Windows.Axis }
    , AxisLinux = Evdev.AbsAxis
    , ButtonLinux = Evdev.Key
    , AxisWindows = Windows.Axis
    , ButtonWindows = Windows.Button
    }
