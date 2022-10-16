module Procedure.Channel exposing
    ( Channel
    , toString
    )

{-|

@docs Channel
@docs toString

-}

import Internal.Channel as Internal


{-| Identifier for Layers.
-}
type alias Channel =
    Internal.Channel


{-| Convert a Channel into a unique string.

You can use this value as a key for [`Html.Keyed`](https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed) nodes.
-}
toString : Channel -> String
toString =
    Internal.toString
