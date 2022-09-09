module Procedure.Pointer exposing
    ( Pointer
    , root
    )


{-|
@docs Pointer
@docs root

-}

{-| -}
type alias Pointer m0 m1 =
    { wrap : m1 -> m0
    , unwrap : m0 -> Maybe m1
    }


{-| -}
root : Pointer a a
root =
    { wrap = identity
    , unwrap = Just
    }
