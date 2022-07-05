module Internal.ObserverId exposing
    ( ObserverId
    , init
    , inc
    , decoder
    , toString
    , toValue
    )


{-|

@docs ObserverId
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
type ObserverId
    = ObserverId (List SafeInt)


{-| Initial value.
-}
init : ObserverId
init =
    ObserverId [ SafeInt.minBound ]


{-| -}
inc : ObserverId -> ObserverId
inc (ObserverId ls) =
    case incList ls of
        ( True, new ) ->
            ObserverId <| SafeInt.minBound :: new

        ( False, new ) ->
            ObserverId new


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
toString : ObserverId -> String
toString (ObserverId ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "tid_" ++ str)


{-| -}
decoder : Decoder ObserverId
decoder =
    JD.list SafeInt.decoder
        |> JD.map ObserverId


{-| -}
toValue : ObserverId -> Value
toValue (ObserverId ls) =
    JE.list SafeInt.toValue ls
