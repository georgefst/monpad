let WG = ./WG.dhall

let Prelude = ./Prelude.dhall

in    ( λ(a : Type) →
        λ(b : Type) →
        λ(e : WG.Layout a b) →
            e
          ⫽ { elements =
                Prelude.List.map
                  (WG.FullElement a b)
                  (WG.FullElement {} {})
                  ( λ(fe : WG.FullElement a b) →
                        fe
                      ⫽ { element =
                            merge
                              { Button =
                                  λ ( b
                                    : { button : WG.Button
                                      , colour : WG.Colour
                                      , buttonData : b
                                      }
                                    ) →
                                    (WG.Element {} {}).Button
                                      (b ⫽ { buttonData = {=} })
                              , Stick =
                                  λ ( s
                                    : { radius : Natural
                                      , range : Natural
                                      , stickColour : WG.Colour
                                      , backgroundColour : WG.Colour
                                      , stickDataX : a
                                      , stickDataY : a
                                      }
                                    ) →
                                    (WG.Element {} {}).Stick
                                      (   s
                                        ⫽ { stickDataX = {=}, stickDataY = {=} }
                                      )
                              }
                              fe.element
                        }
                  )
                  e.elements
            }
      )
    : ∀(a : Type) → ∀(b : Type) → WG.Layout a b → WG.Layout {} {}
