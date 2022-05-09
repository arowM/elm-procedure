module Procedure.VPack exposing
    ( VPack
    , global
    , issue
    , memory
    , dig
    , child
    )

{-| `VPack` is used for building hierarchical Views and Subscriptions.

@docs VPack
@docs global
@docs issue
@docs memory
@docs dig
@docs child

-}

import Internal.VPack exposing (VPack(..))
import Procedure exposing (Msg)
import Procedure.ObserverId exposing (ObserverId)


{-| -}
type alias VPack e0 e1 m1 =
    Internal.VPack.VPack e0 e1 m1


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
global =
    Internal.VPack.global


{-| -}
dig :
    { get : m1 -> m2
    , wrap : e2 -> e1
    }
    -> VPack e0 e1 m1
    -> VPack e0 e2 m2
dig o (VPack r) =
    VPack
        { observerId = r.observerId
        , memory = o.get r.memory
        , wrap = o.wrap >> r.wrap
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
