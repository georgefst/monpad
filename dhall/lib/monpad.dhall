let Prelude = ./Prelude.dhall

let Colour = { red : Double, green : Double, blue : Double, alpha : Double }

let V2 = λ(a : Type) → { x : a, y : a }

let TextShadow = { offset : V2 Integer, blur : Natural, colour : Colour }

let PosX = < Left | Centre | Right >

let PosY = < Top | Middle | Bottom >

let TextStyle =
      { size : Natural
      , colour : Colour
      , bold : Bool
      , italic : Bool
      , underline : Bool
      , shadow : List TextShadow
      , rotation : Double
      , align : PosX
      , font : Text
      }

let ViewBox = { x : Integer, y : Integer, w : Natural, h : Natural }

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
        , offset : V2 Integer
        , width : Natural
        , initialPosition : Double
        , resetOnRelease : Bool
        , sliderColour : Colour
        , backgroundColour : Colour
        , sliderData : a
        }

let Image = { url : Text }

let TextBox = { text : Text, style : TextStyle, alignX : PosX, alignY : PosY }

let Indicator =
      { hollowness : Double
      , arcStart : Double
      , arcEnd : Double
      , centre : V2 Double
      , colour : Colour
      , shape : Shape
      }

let TextInput =
      { textStyle : TextStyle
      , minLength : Optional Natural
      , maxLength : Optional Natural
      }

let NumberInput =
      { textStyle : TextStyle
      , min : Optional Double
      , max : Optional Double
      , step : Optional Double
      }

let InputType = < CheckBox | Number : NumberInput | Text : TextInput >

let Input = { width : Natural, height : Natural, inputType : InputType }

let Element =
      λ(a : Type) →
      λ(b : Type) →
        < Button : Button b
        | Stick : Stick a
        | Slider : Slider a
        | Indicator : Indicator
        | Input : Input
        | Empty : V2 Natural
        >

let FullElement =
      λ(a : Type) →
      λ(b : Type) →
        { element : Element a b
        , location : V2 Integer
        , name : Text
        , text : Optional TextBox
        , image : Optional Image
        , hidden : Bool
        }

let Layout =
      λ(a : Type) →
      λ(b : Type) →
        { elements : List (FullElement a b)
        , viewBox : ViewBox
        , backgroundColour : Colour
        , name : Text
        }

let col =
      λ(r : Double) →
      λ(g : Double) →
      λ(b : Double) →
      λ(a : Double) →
        { red = r, green = g, blue = b, alpha = a }

let cols =
      { red = col 0.9 0.2 0.2 1.0
      , green = col 0.2 0.8 0.2 1.0
      , blue = col 0.2 0.2 0.9 1.0
      , pastelBlue = col 0.81 0.91 0.97 1.0
      , yellow = col 1.0 0.8 0.0 1.0
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
                            , Indicator = (Element a1 b1).Indicator
                            , Input = (Element a1 b1).Input
                            , Empty = (Element a1 b1).Empty
                            }
                            fe.element
                      }
                )
                e.elements
          }

let simpleSlider =
      λ(a : Type) →
      λ(b : Type) →
      λ ( s
        : { radius : Natural
          , length : Natural
          , width : Natural
          , sliderColour : Colour
          , backgroundColour : Colour
          , vertical : Bool
          , sliderData : a
          }
        ) →
          (Element a b).Slider
            (   s
              ∧ { offset =
                    if    s.vertical
                    then  { x = +0, y = Natural/toInteger s.length }
                    else  { x = Natural/toInteger s.length, y = +0 }
                , initialPosition = 0.5
                , resetOnRelease = True
                }
            ).(Slider a)
        : Element a b

let noTextStyle =
        { size = 30
        , colour = cols.black
        , bold = False
        , italic = False
        , underline = False
        , shadow = [] : List TextShadow
        , rotation = 0.0
        , align = PosX.Centre
        , font = "sans-serif"
        }
      : TextStyle

let defaultTextStyle =
      noTextStyle ⫽ { colour = cols.red, italic = True, bold = True }

let Elem =
      λ(a : Type) →
      λ(b : Type) →
        { Type = FullElement a b
        , default = { text = None TextBox, image = None Image, hidden = False }
        }

in  λ(a : Type) →
    λ(b : Type) →
      { Colour
      , TextStyle
      , TextShadow
      , noTextStyle
      , defaultTextStyle
      , V2
      , Shape
      , cols
      , Element = Element a b
      , FullElement = FullElement a b
      , Elem = Elem a b
      , Layout = Layout a b
      , mapLayout = mapLayout a b
      , simpleSlider = simpleSlider a b
      , ViewBox
      , Button = Button b
      , Stick = Stick a
      , Slider = Slider a
      , Image
      , PosX
      , PosY
      , TextBox
      , Indicator
      , Input
      , InputType
      , NumberInput
      , TextInput
      }
