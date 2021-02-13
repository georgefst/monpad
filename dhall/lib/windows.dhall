let Key = Natural

let Axis = < X | Y >

let MouseEventType = < Relative | Absolute | Wheel >

let MouseEvent =
      { axis : Axis, mouseType : MouseEventType, multiplier : Double }

in  { Key, MouseEvent, Axis, MouseEventType }
