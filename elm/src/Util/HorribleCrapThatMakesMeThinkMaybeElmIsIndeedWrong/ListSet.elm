module Util.HorribleCrapThatMakesMeThinkMaybeElmIsIndeedWrong.ListSet exposing (..)

{-| out of site, out of mind
<https://reasonablypolymorphic.com/blog/elm-is-wrong/>
-}


{-| <https://www.reddit.com/r/elm/comments/g7bjxp/sets_of_a_custom_type/>
-}
type Set a
    = Set (List a)


empty : Set a
empty =
    Set []


add : a -> Set a -> Set a
add x (Set xs) =
    Set <|
        if List.member x xs then
            xs

        else
            x :: xs


{-| same as filter . (/=), but more efficient, due to the no-duplicates invariant
-}
remove : a -> Set a -> Set a
remove x =
    let
        f xs =
            case xs of
                [] ->
                    -- it was never there
                    []

                y :: ys ->
                    if x == y then
                        -- here it is
                        ys

                    else
                        -- keep looking
                        y :: f ys
    in
    toList >> f >> fromList


member : a -> Set a -> Bool
member x (Set xs) =
    List.member x xs


toList : Set a -> List a
toList (Set xs) =
    xs


fromList : List a -> Set a
fromList =
    Set
