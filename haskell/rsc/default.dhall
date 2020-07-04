let Colour = { red : Double, green : Double, blue : Double, alpha : Double }

let Button = < Circle : Double | Rectangle : { _1 : Double, _2 : Double } >

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
      { elements :
          List
            { element : Element
            , location : { _1 : Double, _2 : Double }
            , name : Text
            }
      }

let col =
      λ(r : Double) →
      λ(g : Double) →
      λ(b : Double) →
      λ(a : Double) →
        { red = r, green = g, blue = b, alpha = a }

let cols =
      { blue = col 0.28 0.28 0.85 1.0
      , yellow = col 0.94 0.95 0.33 1.0
      , red = col 0.85 0.28 0.28 1.0
      , green = col 0.20 0.72 0.20 1.0
      , black = col 0.0 0.0 0.0 1.0
      , white = col 1.0 1.0 1.0 1.0
      }

in    { elements =
        [ { element =
              Element.Button
                { button = Button.Circle 120.0, colour = cols.blue }
          , location = { _1 = 250.0, _2 = 0.0 }
          , name = "Blue"
          }
        , { element =
              Element.Button
                { button = Button.Circle 120.0, colour = cols.green }
          , location = { _1 = 500.0, _2 = -250.0 }
          , name = "Green"
          }
        , { element =
              Element.Button { button = Button.Circle 120.0, colour = cols.red }
          , location = { _1 = 750.0, _2 = 0.0 }
          , name = "Red"
          }
        , { element =
              Element.Button
                { button = Button.Circle 120.0, colour = cols.yellow }
          , location = { _1 = 500.0, _2 = 250.0 }
          , name = "Yellow"
          }
        , { element =
              Element.Stick
                { radius = 120.0
                , range = 320.0
                , stickColour = cols.white
                , backgroundColour = cols.black
                }
          , location = { _1 = -500.0, _2 = 0.0 }
          , name = "Main"
          }
        ]
      }
    : Layout
