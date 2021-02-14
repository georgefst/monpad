let Prelude = ./Prelude.dhall

let Colour = { red : Double, green : Double, blue : Double, alpha : Double }

let ViewBox = { x : Integer, y : Integer, w : Integer, h : Integer }

let V2 = λ(a : Type) → { x : a, y : a }

let Shape = < Circle : Natural | Rectangle : V2 Natural >

let Button = λ(b : Type) → { shape : Shape, colour : Colour, buttonData : b }

let Stick =
      λ(a : Type) →
        { radius : Natural
        , range : Natural
        , stickColour : Colour
        , backgroundColour : Colour
        , stickDataX : a
        , stickDataY : a
        }

let Slider =
      λ(a : Type) →
        { radius : Natural
        , length : Natural
        , width : Natural
        , sliderColour : Colour
        , backgroundColour : Colour
        , vertical : Bool
        , sliderData : a
        }

let Image = { width : Natural, height : Natural, url : Text }

let Indicator =
      { hollowness : Double
      , arcStart : Double
      , arcEnd : Double
      , colour : Colour
      , radius : Natural
      }

let Element =
      λ(a : Type) →
      λ(b : Type) →
        < Button : Button b
        | Stick : Stick a
        | Slider : Slider a
        | Image : Image
        | Indicator : Indicator
        >

let FullElement =
      λ(a : Type) →
      λ(b : Type) →
        { element : Element a b
        , location : V2 Integer
        , name : Text
        , showName : Bool
        }

let Layout =
      λ(a : Type) →
      λ(b : Type) →
        { elements : List (FullElement a b), viewBox : ViewBox }

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
      , grey = col 0.35 0.35 0.4 1.0
      , white = col 1.0 1.0 1.0 1.0
      }

let mapLayout
    : ∀(a0 : Type) →
      ∀(b0 : Type) →
      ∀(a1 : Type) →
      ∀(b1 : Type) →
      ∀(fa : a0 → a1) →
      ∀(fb : b0 → b1) →
      Layout a0 b0 →
        Layout a1 b1
    = λ(a0 : Type) →
      λ(b0 : Type) →
      λ(a1 : Type) →
      λ(b1 : Type) →
      λ(fa : a0 → a1) →
      λ(fb : b0 → b1) →
      λ(e : Layout a0 b0) →
          e
        ⫽ { elements =
              Prelude.List.map
                (FullElement a0 b0)
                (FullElement a1 b1)
                ( λ(fe : FullElement a0 b0) →
                      fe
                    ⫽ { element =
                          merge
                            { Button =
                                λ(button : Button b0) →
                                  (Element a1 b1).Button
                                    (   button
                                      ⫽ { buttonData = fb button.buttonData }
                                    )
                            , Stick =
                                λ(stick : Stick a0) →
                                  (Element a1 b1).Stick
                                    (   stick
                                      ⫽ { stickDataX = fa stick.stickDataX
                                        , stickDataY = fa stick.stickDataY
                                        }
                                    )
                            , Slider =
                                λ(slider : Slider a0) →
                                  (Element a1 b1).Slider
                                    (   slider
                                      ⫽ { sliderData = fa slider.sliderData }
                                    )
                            , Image = (Element a1 b1).Image
                            , Indicator = (Element a1 b1).Indicator
                            }
                            fe.element
                      }
                )
                e.elements
          }

in  λ(a : Type) →
    λ(b : Type) →
      { Colour
      , V2
      , Shape
      , cols
      , Element = Element a b
      , FullElement = FullElement a b
      , Layout = Layout a b
      , mapLayout = mapLayout a b
      }
