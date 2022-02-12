module Procedure.Wrapper exposing
    ( Wrapper
    , andThen
    )

{-| Helper module for building SPA.

@docs Wrapper
@docs andThen

-}


{-| -}
type alias Wrapper a b =
    { unwrap : a -> Maybe b
    , wrap : b -> a
    }


{-| -}
andThen : Wrapper b c -> Wrapper a b -> Wrapper a c
andThen w2 w1 =
    { unwrap = w1.unwrap >> Maybe.andThen w2.unwrap
    , wrap = w2.wrap >> w1.wrap
    }
