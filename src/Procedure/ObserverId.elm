module Procedure.ObserverId exposing
    ( ObserverId
    , child
    , decoder
    , toString
    , toValue
    )

{-|

@docs ObserverId
@docs child
@docs decoder
@docs toString
@docs toValue

-}

import Internal.ObserverId as Internal
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| -}
type alias ObserverId e =
    Internal.ObserverId e


{-| Make Observer ID to observe child Events.
-}
child : (e1 -> e0) -> ObserverId e0 -> ObserverId e1
child _ = Internal.coerce

{-| Convert into `String`.
Different `ObserverId`s are supposed to be converted to different strings, and the same `ObserverId`s to the same string.
-}
toString : ObserverId e -> String
toString = Internal.toString


{-| JSON decoder.
-}
decoder : Decoder (ObserverId e)
decoder = Internal.decoder


{-| Convert into JSON `Value`.
-}
toValue : (ObserverId e) -> Value
toValue = Internal.toValue
