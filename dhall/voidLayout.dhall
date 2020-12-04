--TODO this is a workaround until we have something like https://github.com/dhall-lang/dhall-haskell/issues/1521

let E = ./../dhall/evdev.dhall

let A = E.AbsAxis

let B = E.Key

in  (./../dhall/monpad.dhall A B).mapLayout
      {}
      {}
      (λ(_ : A) → {=})
      (λ(_ : B) → {=})
