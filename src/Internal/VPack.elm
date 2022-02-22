module Internal.VPack exposing
    ( VPack(..)
    , global
    )

import Internal.ObserverId exposing (ObserverId)


{-| -}
type VPack e0 e1 m1
    = VPack (VPack_ e0 e1 m1)


type alias VPack_ e0 e1 m1 =
    { observerId : ObserverId
    , memory : m1
    , wrap : e1 -> e0
    }


global : m -> VPack e e m
global m =
    VPack
        { observerId = Internal.ObserverId.init
        , memory = m
        , wrap = identity
        }
