let Prelude = ./lib/Prelude.dhall

let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let ButtonL = AllOS.ButtonLinux

let grid = { x = 4, y = 4 }

let halfRect = { x = 50, y = 25 }

let rect = { x = halfRect.x * 2, y = halfRect.y * 2 }

let tile =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(colour : monpad.Colour) →
        { element =
            monpad.Element.Button
              { shape = monpad.Shape.Rectangle rect
              , colour
              , buttonData =
                { linux = ButtonL.BtnMisc, windows = {=}, mac = {=} }
              }
        , location = { x = Natural/toInteger x, y = Natural/toInteger y }
        , name
        , showName = True
        }

let colour =
      λ(v : monpad.V2 Natural) →
        if    Natural/even v.x != Natural/even v.y
        then  monpad.cols.black
        else  monpad.cols.white

let row =
      λ(y : Natural) →
        Prelude.List.generate
          grid.x
          monpad.FullElement
          ( λ(x : Natural) →
              tile
                (x * rect.x + halfRect.x)
                (y * rect.y + halfRect.y)
                "( ${Natural/show x} , ${Natural/show y} )"
                (colour { x, y })
          )

in    { elements =
          Prelude.List.concat
            monpad.FullElement
            (Prelude.List.generate grid.y (List monpad.FullElement) row)
      , grid = { x = grid.x, y = grid.y }
      }
    : monpad.Layout
