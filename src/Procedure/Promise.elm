module Procedure.Promise exposing (Promise)

{-|

@docs Promise

-}

import Internal.Core as Core


{-| -}
type alias Promise cmd memory event a =
    Core.Promise cmd memory event a
