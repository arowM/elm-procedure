module Procedure.Modifier exposing
    ( Modifier
    , root
    , dig
    , maybeItem
    , listItem
    )

{-|


# Modifier

@docs Modifier
@docs root
@docs dig
@docs maybeItem
@docs listItem

-}

import Internal.Channel exposing (Channel)
import Internal.Modifier as Modifier


{-| -}
type alias Modifier m m1 =
    Modifier.Modifier m m1


{-| Root `Modifier`, from which you can dig up some interesting `Modifier`s.
-}
root : Modifier m m
root =
    Modifier.root


{-| Dig up a child `Modifier`.
-}
dig :
    { get : m1 -> m2
    , set : m2 -> m1 -> m1
    }
    -> Modifier m m1
    -> Modifier m m2
dig =
    Modifier.dig


{-| Dig up a child `Modifier` for `Maybe` item.
-}
maybeItem :
    { get : m1 -> Maybe ( Channel, m2 )
    , set : ( Channel, m2 ) -> m1 -> m1
    }
    -> Channel
    -> Modifier m m1
    -> Modifier m m2
maybeItem =
    Modifier.maybeItem


{-| Dig up a child `Modifier` for `List` item.
-}
listItem :
    { get : m1 -> List ( Channel, m2 )
    , set : List ( Channel, m2 ) -> m1 -> m1
    }
    -> Channel
    -> Modifier m m1
    -> Modifier m m2
listItem =
    Modifier.listItem
