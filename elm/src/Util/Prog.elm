module Util.Prog exposing (..)

import Browser exposing (Document, document)
import Html exposing (Html)
import Json.Decode as JD
import Tuple


{-| A slight modification of 'Browser.document',
for programs which can fail only if passed flags of the wrong type at startup.
-}
prog :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , decoder : JD.Decoder flags
    }
    -> Program JD.Value (Result JD.Error model) msg
prog c =
    document
        { init = wrapInit c.decoder c.init
        , view = wrapView (Html.text << JD.errorToString) c.view
        , update = wrapUpdate c.update
        , subscriptions = Result.withDefault Sub.none << Result.map c.subscriptions
        }


wrapInit :
    JD.Decoder flags
    -> (flags -> ( model, Cmd msg ))
    -> JD.Value
    -> ( Result JD.Error model, Cmd msg )
wrapInit decode init =
    transformResultPair Cmd.none init << JD.decodeValue decode


wrapView :
    (error -> Html msg)
    -> (model -> Document msg)
    -> Result error model
    -> Document msg
wrapView viewFail view model =
    case model of
        Ok m ->
            view m

        Err e ->
            { title = "Whoops"
            , body = [ viewFail e ]
            }


wrapUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> msg
    -> Result error model
    -> ( Result error model, Cmd msg )
wrapUpdate update =
    transformResultPair Cmd.none << update


transformResultPair : c -> (a -> ( b, c )) -> Result e a -> ( Result e b, c )
transformResultPair default f r =
    case r of
        Ok x ->
            Tuple.mapFirst Ok <| f x

        Err e ->
            ( Err e, default )
