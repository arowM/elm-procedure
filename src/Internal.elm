module Internal exposing
    ( Msg(..)
    , Observer(..)
    , ObserverId(..)
    , initObserverId
    )

{-| Internal module.
-}

import Internal.SafeInt as SafeInt exposing (SafeInt)


type Observer m0 e0 m1 e1
    = Observer
        { mget : m0 -> Maybe m1
        , set : m1 -> m0 -> m0
        , unwrap : e0 -> Maybe e1
        , wrap : e1 -> e0
        , id : ObserverId
        }


type ObserverId
    = ObserverId (List SafeInt)


{-| Initial value for `ObserverId`.
-}
initObserverId : ObserverId
initObserverId =
    ObserverId [ SafeInt.minBound ]


type Msg event
    = Msg ObserverId event
