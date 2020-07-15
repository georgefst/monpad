let Prelude = ./Prelude.dhall

let WG = ./WG.dhall {} {}

let grid = { x = 4, y = 4 }

let halfRect = { x = 50, y = 25 }

let rect = { x = halfRect.x * 2, y = halfRect.y * 2 }

let tile =
      λ(x : Natural) →
      λ(y : Natural) →
      λ(name : Text) →
      λ(colour : WG.Colour) →
        { element =
            WG.Element.Button
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
          WG.FullElement
          ( λ(x : Natural) →
              tile
                (x * rect.x + halfRect.x)
                (y * rect.y + halfRect.y)
                "( ${Natural/show x} , ${Natural/show y} )"
                (colour { x, y })
          )

in    { elements =
          Prelude.List.concat
            WG.FullElement
            (Prelude.List.generate grid.y (List WG.FullElement) row)
      , grid = { x = grid.x * rect.x, y = grid.y * rect.y }
      }
    : WG.Layout
