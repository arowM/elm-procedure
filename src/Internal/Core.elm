module Internal.Core exposing
    ( Model(..)
    , ThreadState
    , Msg(..)
    )


import Internal.Channel exposing (Channel)
import Internal.PortId exposing (PortId)
import Internal.SubId exposing (SubId)


type Model cmd memory event
    = Thread
        -- New thread state after the evaluation.
        { newState : ThreadState memory event

        -- Side effects caused by the evaluation.
        , cmds : List cmd

        -- New thread to evaluate next time.
        , next : Msg event -> ThreadState memory event -> Model cmd memory event
        }


{-| State to evaluate a thread.
-}
type alias ThreadState memory event =
    { memory : memory
    , nextChannel : Channel
    , subs : List ( SubId, Sub (Msg event) )
    , nextSubId : SubId
    , nextPortId : PortId
    }


type Msg event
    = Msg (Maybe SubId) Channel event
    | NoOp
