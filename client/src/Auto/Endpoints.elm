module Auto.Endpoints exposing (..)

import Auto.Update
import Config
import Http


postUpdateById : String -> Auto.Update.Update -> Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) ())
postUpdateById a b =
    Http.request { method = "POST"
    , headers = []
    , url = String.join "/" [Config.urlBase, "update", a]
    , body = Http.jsonBody (Auto.Update.encode b)
    , expect = Http.expectStringResponse identity (\c -> case c of
        Http.BadUrl_ d ->
            Err (Http.BadUrl d , Nothing)
        
        Http.Timeout_ ->
            Err (Http.Timeout , Nothing)
        
        Http.NetworkError_ ->
            Err (Http.NetworkError , Nothing)
        
        Http.BadStatus_ d e ->
            Err (Http.BadStatus d.statusCode , Just { metadata = d, body = e })
        
        Http.GoodStatus_ d e ->
            if e == "" then
                Ok ()
            
            else
                Err (Http.BadBody "Expected the response body to be empty" , Just { metadata = d
                , body = e }))
    , timeout = Nothing
    , tracker = Nothing }