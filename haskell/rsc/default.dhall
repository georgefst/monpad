let Colour = { red : Double, green : Double, blue : Double, alpha : Double }

let T =
      { colourBlue : Colour
      , colourYellow : Colour
      , colourRed : Colour
      , colourGreen : Colour
      }

let col =
      λ(r : Double) →
      λ(g : Double) →
      λ(b : Double) →
      λ(a : Double) →
        { red = r, green = g, blue = b, alpha = a }

in    { colourBlue = col 0.28 0.28 0.85 1.0
      , colourYellow = col 0.94 0.95 0.33 1.0
      , colourRed = col 0.85 0.28 0.28 1.0
      , colourGreen = col 0.20 0.72 0.20 1.0
      }
    : T
