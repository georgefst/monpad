{-
This rotate function is a bit wonky, but it's hard because Dhall doesn't let us do arbitrary arithmetic.
In particular, just swapping (and negating) x and y coords only works because the viewbox is centered at (0,0).
-}
let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let Prelude = ./lib/Prelude.dhall

let rotate =
      λ(l : monpad.Layout) →
          l
        ⫽ { elements =
              Prelude.List.map
                monpad.FullElement
                monpad.FullElement
                ( λ(fe : monpad.FullElement) →
                      fe
                    ⫽ { location =
                        { x = fe.location.y, y = Integer/negate fe.location.x }
                      , element =
                          merge
                            { Button = monpad.Element.Button
                            , Stick = monpad.Element.Stick
                            , Slider =
                                λ(s : monpad.Slider) →
                                  monpad.Element.Slider
                                    (   s
                                      ⫽ { offset =
                                          { x = s.offset.y, y = s.offset.x }
                                        }
                                    )
                            , Indicator = monpad.Element.Indicator
                            , Input = monpad.Element.Input
                            , Empty = monpad.Element.Empty
                            }
                            fe.element
                      }
                )
                l.elements
          , viewBox =
            { x = l.viewBox.y
            , y = l.viewBox.x
            , w = l.viewBox.h
            , h = l.viewBox.w
            }
          }

in  rotate ./mouse.dhall : monpad.Layout
