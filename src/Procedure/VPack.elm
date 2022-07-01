module Procedure.VPack exposing
    ( VPack
    , root
    , issue
    , memory
    , inherit
    , child
    )

{-| `VPack` is used for building hierarchical Views and Subscriptions.

@docs VPack
@docs root
@docs issue
@docs memory
@docs inherit
@docs child

-}

import Internal exposing (Msg(..), ObserverId, initObserverId)


{-| -}
type VPack e m1 e1
    = VPack
        { observerId : ObserverId
        , memory : m1
        , wrap : e1 -> e
        }


{-| -}
memory : VPack e m1 e1 -> m1
memory (VPack r) =
    r.memory


{-| Issue event to the given `VPack`.
-}
issue : VPack e m1 e1 -> e1 -> Msg e
issue (VPack r) e1 =
    Msg r.observerId (r.wrap e1)


{-| Root `VPack`, which is for your application root.
-}
root : m -> VPack e m e
root m =
    VPack
        { observerId = initObserverId
        , memory = m
        , wrap = identity
        }


{-| Build a new `VPack` that inherits the parent `ObserverId`. See [`Page.Home` in `spa-sample`](https://github.com/arowM/elm-procedure-architecture/blob/main/spa-sample/src/Page/Home.elm) for real use case.
-}
inherit :
    (e2 -> e1)
    -> m2
    -> VPack e m1 e1
    -> VPack e m2 e2
inherit wrap m2 (VPack p) =
    VPack
        { observerId = p.observerId
        , memory = m2
        , wrap = p.wrap << wrap
        }


{-| -}
child :
    (e2 -> e1)
    -> ( ObserverId, m2 )
    -> VPack e m1 e1
    -> VPack e m2 e2
child wrap ( oid, m2 ) (VPack p1) =
    VPack
        { observerId = oid
        , wrap = p1.wrap << wrap
        , memory = m2
        }
