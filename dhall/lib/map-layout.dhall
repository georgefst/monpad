let AllOS = ./all-os.dhall

let monpad = ./monpad.dhall

let monpadAll = monpad AllOS.Axis AllOS.Button

let toLinux =
      monpadAll.mapLayout
        AllOS.AxisLinux
        AllOS.ButtonLinux
        (λ(a : AllOS.Axis) → a.linux)
        (λ(b : AllOS.Button) → b.linux)

let toWindows =
      monpadAll.mapLayout
        AllOS.AxisWindows
        AllOS.ButtonWindows
        (λ(a : AllOS.Axis) → a.windows)
        (λ(b : AllOS.Button) → b.windows)

let toMac =
      monpadAll.mapLayout
        AllOS.AxisMac
        AllOS.ButtonMac
        (λ(a : AllOS.Axis) → a.mac)
        (λ(b : AllOS.Button) → b.mac)

let void =
      monpadAll.mapLayout
        {}
        {}
        (λ(_ : AllOS.Axis) → {=})
        (λ(_ : AllOS.Button) → {=})

in  { void, toLinux, toWindows, toMac }
