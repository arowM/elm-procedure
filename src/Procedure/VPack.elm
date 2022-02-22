module Procedure.VPack exposing
    ( VPack
    , global
    , issue
    , memory
    , child
    )

{-| Helper module for building SPA.
The `VPack` is useful for building hierarchical Views and Subscriptions.

@docs VPack
@docs global
@docs issue
@docs memory
@docs child

-}

import Internal.ObserverId
import Procedure exposing (Msg)
import Procedure.ObserverId exposing (ObserverId)


{-| -}
type VPack e0 e1 m1
    = VPack (VPack_ e0 e1 m1)


type alias VPack_ e0 e1 m1 =
    { observerId : ObserverId
    , memory : m1
    , wrap : e1 -> e0
    }


{-| -}
memory : VPack e0 e1 m1 -> m1
memory (VPack r) =
    r.memory


{-| -}
issue : VPack e0 e1 m1 -> e1 -> Msg e0
issue (VPack r) e1 =
    Procedure.issue r.observerId (r.wrap e1)


{-| Global `VPack`.
-}
global : m -> VPack e e m
global m =
    VPack
        { observerId = Internal.ObserverId.init
        , memory = m
        , wrap = identity
        }


{-| -}
child :
    VPack e0 e1 m1
    -> (e2 -> e1)
    -> ( ObserverId, m2 )
    -> (VPack e0 e2 m2 -> view)
    -> view
child (VPack p1) wrap ( oid, m2 ) f =
    f <|
        VPack
            { observerId = oid
            , wrap = p1.wrap << wrap
            , memory = m2
            }
