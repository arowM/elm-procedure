module Internal.Core exposing
    ( Model(..)
    , Context
    , Procedure(..)
    , none, batch
    , Msg(..)
    , modify, push, asyncOn
    , init, update
    )


import Internal.Channel as Channel exposing (Channel)
import Internal.RequestId as RequestId exposing (RequestId)
import Internal.SubId as SubId exposing (SubId)


type Model cmd memory event
    = Model (Model_ cmd memory event)


type alias Model_ c m e =
        -- New context after the evaluation.
        { context : Context m e

        -- Side effects caused by the evaluation.
        , cmds : List c

        -- New state to evaluate next time.
        , next : Msg e -> Context m e -> Model c m e
        }


{-| Execution time context for procedures
-}
type alias Context memory event =
    { state : memory
    , channel : Channel
    , subs : List ( SubId, Sub (Msg event) )
    , nextSubId : SubId
    , nextRequestId : RequestId
    , nextChannel : Channel
    }


type Msg event
    = Msg (Maybe SubId) Channel event
    | PortEvent Channel Value
    | NoOp


type Procedure c m e
    = Modify
        { modify : m -> m
        , next : Procedure c m e
        }
    | Push
        { push : m -> List c
        , next : Procedure c m e
        }
    | Await
        { await : Channel -> e -> Maybe (Procedure c m e)
        }
    | RunRequest
        { runRequest : Request (Procedure c m e)
        }

    | Async
        { async : Procedure c m e
        , next : Procedure c m e
        }
    | AsyncOn
        { asyncOn : Channel -> Procedure c m e
        , channel : m -> Maybe Channel
        , next : Procedure c m e
        }
    | AddEventListener
        { addEventListener : e -> Procedure c m e
        , subscription : Sub e
        , next : Procedure c m e
        }
    | WithNewChannel
        { withNewChannel :Channel -> Procedure c m e
        , next : Procedure c m e
        }
    | WithNewRequestId
        { WithNewRequestId : RequestId -> Procedure c m e
        , next : Procedure c m e
        }
    | Nil


none : Procedure c m e
none = Nil


batch : List (Procedure c m e) -> Procedure c m e
batch = List.foldr mappend Nil


mappend : Procedure c m e -> Procedure c m e -> Procedure c m e
mappend p1 p2 =
    case p1 of
        Modify r ->
            Modify
            { r | next = mappend r.next p2 }
        Push r ->
            Push
            { r | next = mappend r.next p2 }
        Await r ->
            Await
                { r
                    | await = \e ->
                        r.await e
                            |> Maybe.map
                                (\resolved ->
                                    mappend resolved p2
                                )
                }
        Async r ->
            Async
            { r | next = mappend r.next p2 }
        AsyncOn r ->
            AsyncOn
            { r | next = mappend r.next p2 }
        WithNewChannel r ->
            WithNewChannel
            { r | next = mappend r.next p2 }
        Nil ->
            p2



modify : (m -> m) -> Procedure c m e
modify f =
    Modify
        { modify = f
        , next = Nil
        }

push : (m -> cmd) -> Procedure cmd m e
push f =
    Push
        { push = \m -> [f m]
        , next = Nil
        }

asyncOn :
    { get : m -> Maybe (Channel, m1)
    , set : (Channel, m1) -> m -> m
    }
    -> (Channel -> Procedure c m1 e)
    -> Procedure c m e
asyncOn o f =
    AsyncOn
        { asyncOn = \c -> liftMemory o (f c)
        , channel = \m ->
            o.get m
                |> Maybe.map Tuple.first
        , next = Nil
        }

runRequest : Request a -> (a -> Procedure cmd m e) -> Procedure cmd m e
runRequest req f =
    RunRequest
        { runRequest = Request.map f req
        }


portRequest :
    { request : RequestId -> c
    , response : (Value -> Msg e) -> Sub (Msg e)
    , responseBody : Value -> Maybe a
    , requestId : Value -> Maybe RequestId
    }
    -> (a -> Procedure c m e)
portRequest =
    Debug.log ""


Request a = Set (Msg e) ->
    { result : Maybe a
    , deps : Set (Msg e)
    }

liftMemory :
   { get : m -> Maybe (Channel, m1)
   , set : (Channel, m1) -> m -> m
   }
   -> Procedure c m1 e -> Procedure c m e
liftMemory o proc =
    case proc of
        Modify r ->
            Modify
                { modify = \m ->
                    case o.get m of
                        Just (c, m1) ->
                            o.set (c, r.modify m1) m
                        Nothing ->
                            m
                , next = liftMemory o r.next
                }
        Push r ->
            Push
                { push = \m ->
                    case o.get m of
                        Just (_, m1) ->
                            r.push m1
                        Nothing ->
                            []
                , next = liftMemory o r.next
                }

        Await r ->
            Await
                { await = \e ->
                    r.await e
                        |> Maybe.map (liftMemory o)
                }
        Async r ->
            Async
                { async = liftMemory o r.async
                , next = liftMemory o r.next
                }

        AsyncOn r ->
            AsyncOn
                { asyncOn = \c -> liftMemory o (r.asyncOn c)
                , channel = o.get >> Maybe.map Tuple.first
                , next = liftMemory o r.next
                }
        WithNewChannel r ->
            WithNewChannel
                { withNewChannel = \c ->
                    liftMemory o (r.withNewChannel c)
                , next = liftMemory o r.next
                }
        Nil ->
            Nil


init :
    memory
    -> List (Procedure cmd memory event)
    -> ( Model cmd memory event, List cmd)
init m procs =
    let
        model = toModel_ (initContext m) (batch procs)
    in
    ( Model model, model.cmds)

initContext : m -> Context m e
initContext memory =
    { state = memory
    , channel = Channel.init
    , subs = []
    , nextSubId = SubId.init
    , nextRequestId = RequestId.init
    , nextChannel = Channel.inc Channel.init
    }

toModel_ : Context m e -> Procedure c m e -> Model_ c m e
toModel_ context proc =
    case proc of
        Modify r ->
            toModel_
                { context
                    | state = r.modify context.state
                }
                r.next
        Push r ->
            let
                model = toModel_ context r.next
            in
            { model
                | cmds = r.push context.state ++ model.cmds
            }
        Await r ->
            let
                next : Msg e -> Context m e -> Model c m e
                next msg nextContext =
                    let
                        awaitAgain =
                            Model
                            { context = nextContext
                            , cmds = []
                            , next = next
                            }
                    in
                    case msg of
                        Msg _ channel e ->
                            if nextContext.channel == channel then
                                case r.await e of
                                    Nothing ->
                                        awaitAgain
                                    Just nextProc ->
                                        Model <|
                                        toModel_ nextContext nextProc
                            else
                                awaitAgain
                        NoOp ->
                            awaitAgain
            in
            { context = context
            , cmds = []
            , next = next
            }
        Async r ->
            toModel_ context (concurrent r.async r.next)
        AsyncOn r ->
            case r.channel context.state of
                Nothing ->
                    toModel_ context r.next
                Just c ->
                    toModel_ context (concurrent (r.asyncOn c) r.next)
        WithNewChannel r ->
            let
                newChannel = context.nextChannel
                newContext =
                    { context
                        | nextChannel = Channel.inc context.nextChannel
                        , channel = newChannel
                    }
                newProc = r.withNewChannel newChannel
            in
            toModel_ newContext (mappend newProc r.next)
        Nil ->
            let
                next : Context m e -> Model c m e
                next newContext =
                    Model
                    { context = newContext
                    , cmds = []
                    , next = \_ -> next
                    }
            in
            { context = context
            , cmds = []
            , next = \_ -> next
            }


concurrent : Procedure c m e -> Procedure c m e -> Procedure c m e
concurrent p1 p2 =
    let
        concurrentAwait r1 r2 =
            Await
                { await = \e ->
                    case (r1.await e, r2.await e) of
                        (Nothing, Nothing) ->
                            Nothing
                        (Just resolved1, Nothing) ->
                            Just <| concurrent resolved1 p2
                        (Nothing, Just resolved2) ->
                            Just <| concurrent p1 resolved2
                        (Just resolved1, Just resolved2) ->
                            Just <| concurrent resolved1 resolved2
                }
    in
    case (p1, p2) of
        (Await r1, _) ->
            case p2 of
                Modify r2 ->
                    Modify
                        { r2
                            | next = concurrent p1 r2.next
                        }
                Push r2 ->
                    Push
                        { r2
                            | next = concurrent p1 r2.next
                        }
                Await r2 ->
                    concurrentAwait r1 r2
                Async r2 ->
                    Async
                        { r2
                            | next = concurrent p1 r2.next
                        }
                AsyncOn r2 ->
                    AsyncOn
                        { r2
                            | next = concurrent p1 r2.next
                        }

                WithNewChannel r2 ->
                    WithNewChannel
                        { r2
                            | next = concurrent p1 r2.next
                        }
                Nil ->
                    p1

        (_, Await r2) ->
            case p1 of
                Modify r1 ->
                    Modify
                        { r1
                            | next = concurrent r1.next p2
                        }

                Push r1 ->
                    Push
                        { r1
                            | next = concurrent r1.next p2
                        }

                Await r1 ->
                    concurrentAwait r1 r2
                Async r1 ->
                    Async
                        { r1
                            | next = concurrent r1.next p2
                        }

                AsyncOn r1 ->
                    AsyncOn
                        { r1
                            | next = concurrent r1.next p2
                        }

                WithNewChannel r1 ->
                    WithNewChannel
                        { r1
                            | next = concurrent r1.next p2
                        }

                Nil ->
                    p2

        _ ->
            mappend p1 p2


update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, [] )

        Msg msid _ _ ->
            let
                (Model model2) =
                    case msid of
                        Just sid ->
                            model.next msg
                                { newContext
                                    | subs =
                                        List.filter
                                            (\( sid_, _ ) -> sid_ /= sid)
                                            newContext.subs
                                }

                        Nothing ->
                            model.next msg newContext

                newContext =
                    model.context
            in
            ( Model model2, model2.cmds )
