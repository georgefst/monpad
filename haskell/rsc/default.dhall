let Colour = { red : Double, green : Double, blue : Double, alpha : Double }

let T2 =
      < CircleButton : Double | RectangleButton : { _1 : Double, _2 : Double } >

let T1 =
      < Button :
          { _1 : T2
          , _2 : Colour
          }
      | Stick :
          { _1 : Double
          , _2 : Double
          , _3 : Colour
          , _4 : Colour
          }
      >

let T =
      { elements :
          List
            { element : T1
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
        [ { element = T1.Button { _1 = T2.CircleButton 120.0, _2 = cols.blue }
          , location = { _1 = 250.0, _2 = 0.0 }
          , name = "Blue"
          }
        , { element = T1.Button { _1 = T2.CircleButton 120.0, _2 = cols.green }
          , location = { _1 = 500.0, _2 = -250.0 }
          , name = "Green"
          }
        , { element = T1.Button { _1 = T2.CircleButton 120.0, _2 = cols.red }
          , location = { _1 = 750.0, _2 = 0.0 }
          , name = "Red"
          }
        , { element = T1.Button { _1 = T2.CircleButton 120.0, _2 = cols.yellow }
          , location = { _1 = 500.0, _2 = 250.0 }
          , name = "Yellow"
          }
        , { element =
              T1.Stick
                { _1 = 120.0, _2 = 320.0, _3 = cols.white, _4 = cols.black }
          , location = { _1 = -500.0, _2 = 0.0 }
          , name = "Main"
          }
        ]
      }
    : T
