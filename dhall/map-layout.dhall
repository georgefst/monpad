--TODO this is a workaround until we have something like https://github.com/dhall-lang/dhall-haskell/issues/1521

let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall

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

let void =
      monpadAll.mapLayout
        {}
        {}
        (λ(_ : AllOS.Axis) → {=})
        (λ(_ : AllOS.Button) → {=})

in  { void, toLinux, toWindows }
