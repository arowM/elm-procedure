module Internal.PortId exposing
    ( PortId
    , init
    , inc
    , decoder
    , toValue
    )

{-|

@docs PortId
@docs init
@docs inc
@docs decoder
@docs toValue

-}

import Internal.SafeInt as SafeInt exposing (SafeInt)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| -}
type PortId
    = PortId (List SafeInt)


{-| Initial value.
-}
init : PortId
init =
    PortId [ SafeInt.minBound ]


{-| -}
inc : PortId -> PortId
inc (PortId ls) =
    case incList ls of
        ( True, new ) ->
            PortId <| SafeInt.minBound :: new

        ( False, new ) ->
            PortId new


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
toString : PortId -> String
toString (PortId ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "tid_" ++ str)


{-| -}
decoder : Decoder PortId
decoder =
    JD.list SafeInt.decoder
        |> JD.map PortId


{-| -}
toValue : PortId -> Value
toValue pid =
    JE.string (toString pid)
