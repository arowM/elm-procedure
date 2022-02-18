module Procedure.VPack exposing
    ( VPack
    , issue
    , child
    )

{-| Helper module for building SPA.
The `VPack` is useful for building hierarchical Views and Subscriptions.

@docs VPack
@docs issue
@docs child

-}

import Procedure exposing (Msg)
import Procedure.ObserverId exposing (ObserverId)


{-| -}
type alias VPack e0 e1 m1 =
    { observerId : ObserverId
    , memory : m1
    , wrap : e1 -> e0
    }


{-| -}
issue : VPack e0 e1 m1 -> e1 -> Msg e0
issue p1 e1 =
    Procedure.issue p1.observerId (p1.wrap e1)


{-| -}
child :
    VPack e0 e1 m1
    -> (e2 -> e1)
    -> ( ObserverId, m2 )
    -> (VPack e0 e2 m2 -> view)
    -> view
child p1 wrap ( oid, m2 ) f =
    f
        { observerId = oid
        , wrap = p1.wrap << wrap
        , memory = m2
        }
