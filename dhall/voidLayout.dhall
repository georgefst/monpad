let E = ./../dhall/evdev.dhall

let A = E.AbsAxis

let B = E.Key

in  (./../dhall/monpad.dhall A B).mapLayout
      {}
      {}
      (λ(_ : A) → {=})
      (λ(_ : B) → {=})
