module Procedure.VPack exposing
    ( VPack
    , issue
    , childView
    )

{-| Helper module for building SPA.

@docs VPack
@docs issue
@docs childView

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
childView :
    VPack e0 e1 m1
    -> (e2 -> e1)
    -> ( ObserverId, m2 )
    -> (VPack e0 e2 m2 -> view)
    -> view
childView p1 wrap ( oid, m2 ) f =
    f
        { observerId = oid
        , wrap = p1.wrap << wrap
        , memory = m2
        }



{-
   childView : VPack e0 e1 m1
       -> (e2 -> e1)
       -> (m1 -> m2)
       -> (VPack e0 e1 m1 -> VPack e0 e2 m2 -> view)
       -> view
   childView p1 wrap get f =
       f p1
           { observerId = oid
           , wrap = p1.wrap << wrap
           , memory = get p1.memory
           }
-}
