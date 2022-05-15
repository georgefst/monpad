module Util.EncodeBytes exposing (bytesToList)

import Auto.ClientUpdate exposing (ClientUpdate(..))
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BD
import List exposing (reverse)


{-| Encode a `Bytes` value as a `Uint8Array`.
It is odd that Elm doesn't allow `Bytes` to be sent through a port directly.
This is adapted from <https://discourse.elm-lang.org/t/bytes-ports-and-uint8array/3569/2>.
Really, this should be separated in to a library. Maybe using `Array` instead of `List`.
-}
bytesToList : Bytes -> List Int
bytesToList bs =
    let
        listDecode =
            bytesListDecode BD.unsignedInt8 <| Bytes.width bs

        bytesListDecode decoder len =
            BD.loop ( len, [] ) (listStep decoder)

        listStep decoder ( n, xs ) =
            if n <= 0 then
                BD.succeed <| BD.Done xs

            else
                BD.map (\x -> BD.Loop ( n - 1, x :: xs )) decoder
    in
    reverse <| Maybe.withDefault [] (BD.decode listDecode bs)
