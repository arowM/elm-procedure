module Internal.Core exposing
    ( Model(..)
    , Context
    , Procedure(..)
    , none, batch
    , Msg(..)
    , modify, push, asyncOn, await
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

        -- New state to evaluate next time.
        , next : Msg e -> Context m e -> (Model c m e, List c)
        }


{-| Execution time context for procedures
-}
type alias Context memory event =
    { state : memory
    , channel : Channel
    , subs : List (SubId, memory -> Maybe (Sub (Msg event) ))
    , nextSubId : SubId
    , nextRequestId : RequestId
    , nextChannel : Channel
    }


type Msg event
    = ChannelMsg
        { channel : Channel
        , event : event
        }
    | ResponseMsg
        { requestId : RequestId
        , event : event
        , closeSub : Maybe SubId
        }
    | SubMsg
        { subId : SubId
        , event : event
        }
    | NoOp


-- EventHandler

type EventHandler e a
    = EventHandler (Msg e -> Result (EventHandler e a) a)

mapEventHandler : (a -> b) -> EventHandler e a -> EventHandler e b
mapEventHandler f (EventHandler handler) =
    EventHandler <|
        \msg ->
            case handler msg of
                Ok a ->
                    Ok <| f a
                Err nextHandler ->
                    Err <| mapEventHandler f nextHandler

succeedEventHandler : a -> EventHandler e a
succeedEventHandler a =
    EventHandler <| \_ -> Ok a


andAsyncEventHandler : EventHandler e a -> EventHandler e (a -> b) -> EventHandler e b
andAsyncEventHandler (EventHandler handlerA) (EventHandler handlerF) =
    EventHandler <| \msg ->
        case (handlerF msg, handlerA msg) of
            (Ok f, Ok a) ->
                Ok <| f a
            (Ok f, Err handlerA2) ->
                Err <| mapEventHandler f handlerA2
            ( Err handlerF2, Ok a) ->
                Err <| mapEventHandler (\f -> f a) handlerF2
            ( Err handlerF2, Err handlerA2) ->
                Err <| andAsyncEventHandler handlerA2 handlerF2


andRaceEventHandler : EventHandler e a -> EventHandler e a -> EventHandler e a
andRaceEventHandler (EventHandler handler1) (EventHandler handler2) =
    EventHandler <| \msg ->
        case (handler1 msg, handler2 msg) of
            (Ok a1, _) ->
                Ok <| a1
            (Err _, Ok a2) ->
                Ok <| a2
            (Err next1, Err next2) ->
                Err <| andRaceEventHandler next1 next2

-- Promise


type Promise cmd e a = Promise
    { handler : EventHandler e a
    , cmds : List (RequestId -> cmd)
    }

succeedPromise : a -> Promise c e a
succeedPromise a =
    Promise
        { handler = succeedEventHandler a
        , cmds = []
        }

mapPromise : (a -> b) -> Promise c e a -> Promise c e b
mapPromise f (Promise prom) =
    Promise
        { handler = mapEventHandler f prom.handler
        , cmds = prom.cmds
        }


andAsyncPromise : Promise c e a -> Promise c e (a -> b) -> Promise c e b
andAsyncPromise (Promise promA) (Promise promF) =
    Promise
        { handler = andAsyncEventHandler promA.handler promF.handler
        , cmds = promA.cmds ++ promF.cmds
        }


andRacePromise : Promise c e a -> Promise c e a -> Promise c e a
andRacePromise (Promise prom1) (Promise prom2) =
    Promise
        { handler = andRaceEventHandler prom1.handler prom2.handler
        , cmds = prom1.cmds ++ prom2.cmds
        }


-- Procedure

type Procedure c m e
    = Modify
        { modify : m -> m
        , next : m -> Procedure c m e
        }
    | Push
        { push : m -> List c
        , next : m -> Procedure c m e
        }
    | RunPromise
        { runPromise : Promise c e (Procedure c m e)
        }

    | Async
        { async : Procedure c m e
        , next : m -> Procedure c m e
        }
    | AsyncOn
        { asyncOn : Channel -> Procedure c m e
        , channel : m -> Maybe Channel
        , next : m -> Procedure c m e
        }
    | AddListener
        { addListener : e -> Procedure c m e
        , subscription : m -> Maybe (Sub e)
        , next : m -> Procedure c m e
        }
    | WithNewChannel
        { withNewChannel :Channel -> Procedure c m e
        , next : m -> Procedure c m e
        }
    -- | WithNewRequestId
    --     { withNewRequestId : RequestId -> Procedure c m e
    --     , next : m -> Procedure c m e
    --     }
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
        RunPromise r ->
            RunPromise
                { r
                    | runPromise =
                        mapPromise
                            (\resolved ->
                                mappend resolved p2
                            )
                            r.runPromise
                }
        Async r ->
            Async
            { r | next = mappend r.next p2 }
        AsyncOn r ->
            AsyncOn
            { r | next = mappend r.next p2 }
        AddListener r ->
            AddListener
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

await : Promise cmd e a -> (a -> Procedure cmd m e) -> Procedure cmd m e
await prom f =
    RunPromise
        { runPromise = mapPromise f prom
        }


-- portRequest :
--     { request : RequestId -> c
--     , response : (Value -> Msg e) -> Sub (Msg e)
--     , responseBody : Value -> Maybe a
--     , requestId : Value -> Maybe RequestId
--     }
--     -> (a -> Procedure c m e)
-- portRequest =
--     Debug.todo ""


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

        RunPromise r ->
            RunPromise
                { runPromise =
                    mapPromise (liftMemory o) r.runPromise
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

        AddListener r ->
            AddListener
                { addListener = \e ->
                    liftMemory o (r.addListener e)
                , subscription = \m ->
                    o.get m
                        |> Maybe.map Tuple.second
                        |> Maybe.andThen r.subscription
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
        (model, cmds) = toModel_ (initContext m) (batch procs)
    in
    ( Model model, cmds)

initContext : m -> Context m e
initContext memory =
    { state = memory
    , channel = Channel.init
    , subs = \_ -> []
    , nextSubId = SubId.init
    , nextRequestId = RequestId.init
    , nextChannel = Channel.inc Channel.init
    }

toModel_ : Context m e -> Procedure c m e -> (Model_ c m e, List c)
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
                (model, cmds) = toModel_ context r.next
            in
            (model, r.push context.state ++ cmds)

        RunPromise r ->
            let
                (Promise prom) = r.runPromise
                (EventHandler handler) = prom.handler

                next : Msg e -> Context m e -> (Model c m e, List c)
                next msg nextContext =
                    case handler msg of
                        Err nextHandler ->
                            Tuple.mapFirst Model <| toModel_ nextContext <|
                                RunPromise
                                    { runPromise =
                                        Promise
                                            { handler = nextHandler
                                            , cmds = []
                                            }
                                    }
                        Ok nextProc ->
                            Tuple.mapFirst Model <| toModel_ nextContext nextProc
                (cmds, nextRequestId) =
                    List.foldr
                        (\toCmd (accCmds, accNextReqId) ->
                            ( toCmd accNextReqId :: accCmds
                            , RequestId.inc accNextReqId
                            )
                        )
                        ([], context.nextRequestId)
                        prom.cmds
            in
            ( { context = { context | nextRequestId = nextRequestId }
              , next = next
              }
            , cmds
            )

        Async r ->
            toModel_ context (concurrent r.async r.next)
        AsyncOn r ->
            case r.channel context.state of
                Nothing ->
                    toModel_ context r.next
                Just c ->
                    toModel_ context (concurrent (r.asyncOn c) r.next)

        AddListener r ->
            let
                mySubId = context.nextSubId
                newContext =
                    { context
                        | subs = \m ->
                            case r.subscription m of
                                Nothing ->
                                    context.subs m
                                Just sub ->
                                    (mySubId
                                    , Sub.map
                                        (\e ->
                                            SubMsg
                                                { subId = mySubId
                                                , event = e
                                                }
                                        )
                                        sub
                                    ) :: context.subs m
                        , nextSubId = SubId.inc context.nextSubId
                    }

                handler msg nextContext =
                    case msg of
                        SubMsg o ->
                            if (o.subId == mySubId) then
                                toModel_ nextContext (r.addListener o.event)
                                    |> Tuple.mapFirst Model
                            else
                                ( Model
                                    { context = nextContext
                                    , next = handler
                                    }
                                , []
                                )
                        _ ->
                            ( Model
                                { context = nextContext
                                , next = handler
                                }
                            , []
                            )
                (nextModel, nextCmds) = toModel_ newContext r.next
            in
            ( { nextModel
                | next = concurrentNext handler nextModel.next
              }
            , nextCmds
            )

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
                next : Context m e -> (Model c m e, List c)
                next newContext =
                    ( Model
                        { context = newContext
                        , next = \_ -> next
                        }
                    , []
                    )
            in
            ( { context = context
              , next = \_ -> next
              }
            , []
            )


concurrent : Procedure c m e -> Procedure c m e -> Procedure c m e
concurrent p1 p2 =
    let
        concurrentRequest r1 r2 =
            RunPromise
                { runPromise =
                    mapPromise concurrent r1.runPromise
                        |> andAsyncPromise r2.runPromise
                }
    in
    case (p1, p2) of
        (RunPromise r1, _) ->
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
                RunPromise r2 ->
                    concurrentRequest r1 r2
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

                AddListener r2 ->
                    AddListener
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

        (_, RunPromise r2) ->
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

                RunPromise r1 ->
                    concurrentRequest r1 r2

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

                AddListener r1 ->
                    AddListener
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


concurrentNext :
    (Msg e -> Context m e -> (Model c m e, List c))
    -> (Msg e -> Context m e -> (Model c m e, List c))
    -> Msg e ->Context m e -> (Model c m e, List c)
concurrentNext next1 next2 msg context =
    let
        (Model model1, cmds1) = next1 msg context
        (Model model2, cmds2) = next2 msg model1.context
    in
    ( Model
        { context = model2.context
        , next = concurrentNext model1.next model2.next
        }
    , cmds1 ++ cmds2
    )

update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, [] )

        ChannelMsg _ ->
            model.next msg model.context

        SubMsg _ ->
            model.next msg model.context

        ResponseMsg r ->
            let
                newContext =
                    case r.closeSub of
                        Just subId ->
                            let
                                context = model.context
                            in
                            { context |
                                subs = \m ->
                                    List.filter
                                        (\(sid, _) ->
                                            sid /= subId
                                        )
                                        (context.subs m)
                            }
                        Nothing ->
                            model.context
            in
            model.next msg newContext
