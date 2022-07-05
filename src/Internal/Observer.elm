module Internal.Observer exposing
    ( Observer(..)
    , mget
    , set
    , expected
    , setExpect
    , fromRecord
    )


{-|

@docs Observer(..)
@docs mget
@docs set
@docs expected
@docs setExpect
@docs fromRecord

-}


import Internal.ObserverId exposing (ObserverId)

{-| -}
type Observer m e1 m1
    = Observer
        { mget : m -> Maybe (ObserverId e1, m1)
        , set : ( ObserverId e1, m1) -> m -> m
        , expected : ObserverId e1
        }


{-| -}
mget : Observer m e1 m1 -> m -> Maybe (ObserverId e1, m1)
mget (Observer o) m0 =
    o.mget m0
        |> Maybe.andThen
            (\(oid1, m1) ->
                if oid1 == o.expected then
                    Just (oid1, m1)
                else
                    Nothing
            )


{-| -}
set : Observer m e1 m1 -> (ObserverId e1, m1) -> m -> m
set (Observer o) (oid1, m1) m0 =
    if oid1 == o.expected then
        o.set (oid1, m1) m0
    else
        m0


{-| -}
expected : Observer m e1 m1 -> ObserverId e1
expected (Observer o) =
    o.expected


{-| -}
setExpect : ObserverId e1 -> Observer m e1 m1 -> Observer m e1 m1
setExpect oid1 (Observer o) =
    Observer { o | expected = oid1 }


{-| -}
fromRecord :
    { mget : m -> Maybe (ObserverId e1, m1)
    , set : ( ObserverId e1, m1) -> m -> m
    , expected : ObserverId e1
    }
    -> Observer m e1 m1
fromRecord = Observer
