module Internal.SubId exposing
    ( SubId
    , init
    , inc
    )

{-|

@docs SubId
@docs init
@docs inc

-}

import Internal.SafeInt as SafeInt exposing (SafeInt)


{-| -}
type SubId
    = SubId (List SafeInt)


{-| Initial value.
-}
init : SubId
init =
    SubId [ SafeInt.minBound ]


{-| -}
inc : SubId -> SubId
inc (SubId ls) =
    case incList ls of
        ( True, new ) ->
            SubId <| SafeInt.minBound :: new

        ( False, new ) ->
            SubId new


incList : List SafeInt -> ( Bool, List SafeInt )
incList =
    List.foldr
        (\a ( carry, ls ) ->
            if carry then
                SafeInt.inc a
                    |> Tuple.mapSecond (\new -> new :: ls)

            else
                ( False, a :: ls )
        )
        ( True, [] )
