let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

in    { elements = [] : List monpad.FullElement
      , viewBox = { x = -1000, y = -500, w = +2000, h = +1000 }
      }
    : monpad.Layout
