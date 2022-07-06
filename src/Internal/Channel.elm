module Internal.Channel exposing
    ( Channel
    , init
    , inc
    , decoder
    , toString
    , toValue
    )

{-|

@docs Channel
@docs init
@docs inc
@docs decoder
@docs toString
@docs toValue

-}

import Internal.SafeInt as SafeInt exposing (SafeInt)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| -}
type Channel
    = Channel (List SafeInt)


{-| Initial value.
-}
init : Channel
init =
    Channel [ SafeInt.minBound ]


{-| -}
inc : Channel -> Channel
inc (Channel ls) =
    case incList ls of
        ( True, new ) ->
            Channel <| SafeInt.minBound :: new

        ( False, new ) ->
            Channel new


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


{-| -}
toString : Channel -> String
toString (Channel ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "tid_" ++ str)


{-| -}
decoder : Decoder Channel
decoder =
    JD.list SafeInt.decoder
        |> JD.map Channel


{-| -}
toValue : Channel -> Value
toValue (Channel ls) =
    JE.list SafeInt.toValue ls
