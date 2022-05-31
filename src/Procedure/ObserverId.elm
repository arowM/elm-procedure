module Procedure.ObserverId exposing
    ( ObserverId
    , decoder
    , toString
    , toValue
    )

{-|

@docs ObserverId
@docs decoder
@docs toString
@docs toValue

-}

import Internal exposing (ObserverId)
import Internal.SafeInt as SafeInt
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| ID to determine `Observer`.
-}
type alias ObserverId =
    Internal.ObserverId


{-| Convert into `String`.
Different `ObserverId`s are supposed to be converted to different strings, and the same `ObserverId`s to the same string.
-}
toString : ObserverId -> String
toString (Internal.ObserverId ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "tid_" ++ str)


{-| JSON decoder.
-}
decoder : Decoder ObserverId
decoder =
    JD.list SafeInt.decoder
        |> JD.map Internal.ObserverId


{-| Convert into JSON `Value`.
-}
toValue : ObserverId -> Value
toValue (Internal.ObserverId ls) =
    JE.list SafeInt.toValue ls
