let Prelude = ./Prelude.dhall

let WG = ./WG.dhall

let Element = WG.Element {} {}

let FullElement = WG.FullElement {} {}

let Layout = WG.Layout {} {}

let grid = { x = 4, y = 4 }

let halfRect = { x = 50, y = 25 }

let rect = { x = halfRect.x * 2, y = halfRect.y * 2 }

let tile =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element =
            Element.Button
              { button = WG.Button.Rectangle rect, colour, buttonData = {=} }
        , location = { x, y }
        , name
        , showName = True
        }

let colour =
      λ(v : WG.Vec2) →
        if    Natural/even v.x != Natural/even v.y
        then  WG.cols.black
        else  WG.cols.white

let row =
      λ(y : Natural) →
        Prelude.List.generate
          grid.x
          FullElement
          ( λ(x : Natural) →
              tile
                (x * rect.x + halfRect.x)
                (y * rect.y + halfRect.y)
                "( ${Natural/show x} , ${Natural/show y} )"
                (colour { x, y })
          )

in    { elements =
          Prelude.List.concat
            FullElement
            (Prelude.List.generate grid.y (List FullElement) row)
      , grid = { x = grid.x * rect.x, y = grid.y * rect.y }
      }
    : Layout
