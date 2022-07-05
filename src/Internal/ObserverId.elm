module Internal.ObserverId exposing
    ( ObserverId
    , init
    , inc
    , coerce
    , decoder
    , toString
    , toValue
    )


{-|

@docs ObserverId
@docs init
@docs inc
@docs coerce
@docs decoder
@docs toString
@docs toValue

-}


import Internal.SafeInt as SafeInt exposing (SafeInt)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| -}
type ObserverId e
    = ObserverId (List SafeInt)


{-| -}
coerce : ObserverId e1 -> ObserverId e0
coerce (ObserverId ls) =
    ObserverId ls


{-| Initial value.
-}
init : ObserverId e
init =
    ObserverId [ SafeInt.minBound ]


{-| -}
inc : ObserverId e -> ObserverId e
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
toString : ObserverId e -> String
toString (ObserverId ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "tid_" ++ str)


{-| -}
decoder : Decoder (ObserverId e)
decoder =
    JD.list SafeInt.decoder
        |> JD.map ObserverId


{-| -}
toValue : (ObserverId e) -> Value
toValue (ObserverId ls) =
    JE.list SafeInt.toValue ls
