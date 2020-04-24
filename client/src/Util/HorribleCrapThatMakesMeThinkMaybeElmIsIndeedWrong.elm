module Util.HorribleCrapThatMakesMeThinkMaybeElmIsIndeedWrong exposing (..)

{-| out of site, out of mind
<https://reasonablypolymorphic.com/blog/elm-is-wrong/>
-}


{-| <https://www.reddit.com/r/elm/comments/g7bjxp/sets_of_a_custom_type/>
-}
type ListSet a
    = ListSet (List a)


emptyListSet : ListSet a
emptyListSet =
    ListSet []


addListSet : a -> ListSet a -> ListSet a
addListSet x (ListSet xs) =
    ListSet <|
        if List.member x xs then
            xs

        else
            x :: xs


{-| same as filter . (/=), but more efficient, due to the no-duplicates invariant
-}
removeListSet : a -> ListSet a -> ListSet a
removeListSet x =
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
    toListListSet >> f >> fromListListSet


memberListSet : a -> ListSet a -> Bool
memberListSet x (ListSet xs) =
    List.member x xs


toListListSet : ListSet a -> List a
toListListSet (ListSet xs) =
    xs


fromListListSet : List a -> ListSet a
fromListListSet =
    ListSet
