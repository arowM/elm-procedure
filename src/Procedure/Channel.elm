module Procedure.Channel exposing
    ( Channel
    , toString
    )

{-|

@docs Channel
@docs toString

-}

import Internal.Channel as Internal


{-| -}
type alias Channel =
    Internal.Channel


{-| Convert a Channel into the unique string.
-}
toString : Channel -> String
toString =
    Internal.toString
