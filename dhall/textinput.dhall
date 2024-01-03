let AllOS = ./lib/all-os.dhall

let monpad = ./lib/monpad.dhall AllOS.Axis AllOS.Button

let basic = ./default.dhall

in        basic
      //  { elements =
                basic.elements
              # [ monpad.Elem::{
                  , element =
                      monpad.Element.Input
                        { width = 400
                        , height = 100
                        , inputType =
                            monpad.InputType.Text
                              { textStyle = monpad.noTextStyle // { size = 32 }
                              , minLength = None Natural
                              , maxLength = Some 16
                              }
                        }
                  , location = { x = +0, y = +350 }
                  , name = "Text"
                  }
                ]
          }
    : monpad.Layout
