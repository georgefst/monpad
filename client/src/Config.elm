module Config exposing (..)

urlBase : String
urlBase = "http://localhost:8001"

username : Maybe String
username = Nothing

-- for analog stick
--TODO make these relative to screen size
rSmall : Float
rSmall = 60
rBig : Float
rBig = 175
