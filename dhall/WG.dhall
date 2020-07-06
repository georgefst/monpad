let Colour = { red : Double, green : Double, blue : Double, alpha : Double }

let Vec2 = { x : Natural, y : Natural }

let Button = < Circle : Double | Rectangle : Vec2 >

let Element =
      < Button : { button : Button, colour : Colour }
      | Stick :
          { radius : Double
          , range : Double
          , stickColour : Colour
          , backgroundColour : Colour
          }
      >

let Layout =
      { elements : List { element : Element, location : Vec2, name : Text }
      , grid : Vec2
      }

let col =
      λ(r : Double) →
      λ(g : Double) →
      λ(b : Double) →
      λ(a : Double) →
        { red = r, green = g, blue = b, alpha = a }

let cols =
      { red = col 0.85 0.28 0.28 1.0
      , green = col 0.20 0.72 0.20 1.0
      , blue = col 0.28 0.28 0.85 1.0
      , yellow = col 0.94 0.95 0.33 1.0
      , black = col 0.0 0.0 0.0 1.0
      , white = col 1.0 1.0 1.0 1.0
      }

in  { Colour, Vec2, Button, Element, Layout, cols }
