module Internal.Core exposing
    ( Model(..)
    , Msg(..)
    , Promise(..)
    , succeedPromise
    , mapPromise
    , andAsyncPromise
    , andRacePromise
    , liftPromiseMemory
    , portRequest
    , Procedure(..)
    , none, batch
    , modify, push, await, awaitMsg, async, addListener
    , putLayer, onLayer
    , init, update
    )

{-|


# Core

@docs Model
@docs Msg


# Promise

@docs Promise
@docs succeedPromise
@docs mapPromise
@docs andAsyncPromise
@docs andRacePromise
@docs liftPromiseMemory
@docs portRequest


# Procedure

@docs Procedure
@docs none, batch


# Primitive Procedures

@docs modify, push, await, awaitMsg, async, addListener


# Layer

@docs putLayer, onLayer


# TEA

@docs init, update

-}

import Internal.Channel as Channel exposing (Channel)
import Internal.RequestId as RequestId exposing (RequestId)
import Internal.SubId as SubId exposing (SubId)
import Json.Encode exposing (Value)



-- Model


type Model cmd memory event
    = OnGoing (OnGoing_ cmd memory event)
    | EndOfProcess


type alias OnGoing_ c m e =
    -- New context after the evaluation.
    { context : Context m e

    -- New state to evaluate next time.
    , next : Msg e -> Context m e -> ( Model c m e, List c )
    }


{-| Execution time context for procedures
-}
type alias Context memory event =
    { state : memory
    , subs : List ( Channel, SubId, Sub (Msg event) )
    , layerChannel : LayerChannel memory
    , nextSubId : SubId
    , nextRequestId : RequestId
    , nextChannel : Channel
    }


mapContextMemory :
    Layer_ m m1
    -> Context m e
    -> Maybe (Context m1 e)
mapContextMemory layer context =
    layer.get context.state
        |> Maybe.map
            (\m1 ->
                { state = m1
                , subs = context.subs
                , layerChannel = layer.channel
                , nextSubId = context.nextSubId
                , nextRequestId = context.nextRequestId
                , nextChannel = context.nextChannel
                }
            )


setContext :
    Layer_ m m1
    -> Context m1 e
    -> Context m e
    -> Context m e
setContext layer context1 context =
    { context
        | state =
            layer.set context1.state context.state
    }



-- Msg


type Msg event
    = ChannelMsg
        { channel : Channel
        , event : event
        }
    | ResponseMsg
        { requestId : RequestId
        , response : Value
        , closeSub : Maybe SubId
        }
    | SubMsg
        { subId : SubId
        , event : event
        }
    | NoOp



-- Promise


type Promise c m e a
    = Promise (Context m e -> PromiseEffect c m e a)


type alias PromiseEffect c m e a =
    { newContext : Context m e
    , cmds : List c
    , handler : m -> Msg e -> PromiseResult c m e a
    }


type PromiseResult c m e a
    = Resolved a
    | AwaitAgain (Promise c m e a)
    | Reject


mapPromise : (a -> b) -> Promise c m e a -> Promise c m e b
mapPromise f (Promise prom) =
    Promise <|
        \context ->
            let
                effA =
                    prom context
            in
            { newContext = effA.newContext
            , cmds = effA.cmds
            , handler =
                \m msg ->
                    case effA.handler m msg of
                        Resolved a ->
                            Resolved <| f a

                        AwaitAgain nextProm ->
                            AwaitAgain <|
                                mapPromise f nextProm

                        Reject ->
                            Reject
            }


succeedPromise : a -> Promise c m e a
succeedPromise a =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , handler =
                \_ _ -> Resolved a
            }


andAsyncPromise : Promise c m e a -> Promise c m e (a -> b) -> Promise c m e b
andAsyncPromise (Promise promA) (Promise promF) =
    Promise <|
        \context ->
            let
                effF =
                    promF context

                effA =
                    promA effF.newContext
            in
            { newContext = effA.newContext
            , cmds = effF.cmds ++ effA.cmds
            , handler =
                \m msg ->
                    case ( effF.handler m msg, effA.handler m msg ) of
                        ( Resolved f, Resolved a ) ->
                            Resolved <| f a

                        ( Resolved f, AwaitAgain nextPromA ) ->
                            AwaitAgain <|
                                mapPromise f nextPromA

                        ( AwaitAgain nextPromF, Resolved a ) ->
                            AwaitAgain <|
                                mapPromise (\f -> f a) nextPromF

                        ( AwaitAgain nextPromF, AwaitAgain nextPromA ) ->
                            AwaitAgain <|
                                andAsyncPromise nextPromA nextPromF

                        ( Reject, _ ) ->
                            Reject

                        ( _, Reject ) ->
                            Reject
            }


andRacePromise : Promise c m e a -> Promise c m e a -> Promise c m e a
andRacePromise (Promise prom2) (Promise prom1) =
    Promise <|
        \context ->
            let
                eff1 =
                    prom1 context

                eff2 =
                    prom2 eff1.newContext
            in
            { newContext = eff2.newContext
            , cmds = eff1.cmds ++ eff2.cmds
            , handler =
                \m msg ->
                    case ( eff1.handler m msg, eff2.handler m msg ) of
                        ( Resolved a1, _ ) ->
                            Resolved a1

                        ( _, Resolved a2 ) ->
                            Resolved a2

                        ( AwaitAgain nextProm1, AwaitAgain nextProm2 ) ->
                            AwaitAgain <|
                                andRacePromise nextProm2 nextProm1

                        ( Reject, res2 ) ->
                            res2

                        ( res1, Reject ) ->
                            res1
            }


liftPromiseMemory :
    Layer_ m m1
    -> Promise c m1 e a
    -> Promise c m e a
liftPromiseMemory o (Promise prom1) =
    Promise <|
        \context ->
            case mapContextMemory o context of
                Nothing ->
                    let
                        (LayerChannel closedChannel) =
                            o.channel
                    in
                    { newContext =
                        { context
                            | subs =
                                List.filter
                                    (\( c, _, _ ) ->
                                        c /= closedChannel
                                    )
                                    context.subs
                        }
                    , cmds = []
                    , handler = \_ _ -> Reject
                    }

                Just context1 ->
                    let
                        eff1 =
                            prom1 context1
                    in
                    { newContext = setContext o eff1.newContext context
                    , cmds = eff1.cmds
                    , handler =
                        \m msg ->
                            case o.get m of
                                Nothing ->
                                    Reject

                                Just m1 ->
                                    case eff1.handler m1 msg of
                                        Resolved a ->
                                            Resolved a

                                        AwaitAgain next1 ->
                                            AwaitAgain <| liftPromiseMemory o next1

                                        Reject ->
                                            Reject
                    }


awaitOnce : (m -> Msg e -> List (Procedure c m e)) -> Promise c m e (Procedure c m e)
awaitOnce f =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , handler =
                \m msg ->
                    case f m msg of
                        [] ->
                            AwaitAgain <|
                                awaitOnce f

                        procs ->
                            Resolved <| batch procs
            }



-- Procedure


type Procedure c m e
    = Modify
        { modify : m -> m
        , next : Procedure c m e
        }
    | Push
        -- Supply `m` parameter not to issue commands on expired Layers.
        { push : m -> List c
        , next : Procedure c m e
        }
    | RunPromise
        { promise : Promise c m e (Procedure c m e)
        }
    | Async
        { async : Procedure c m e
        , next : Procedure c m e
        }
    | WithNewChannel
        -- Supply `m` parameter not to assign new channel on expired Layers.
        { withNewChannel : m -> Channel -> Procedure c m e
        , next : Procedure c m e
        }
    | Nil


none : Procedure c m e
none =
    Nil


batch : List (Procedure c m e) -> Procedure c m e
batch =
    List.foldr mappend Nil


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
                    | promise =
                        mapPromise
                            (\resolved ->
                                mappend resolved p2
                            )
                            r.promise
                }

        Async r ->
            Async
                { r | next = mappend r.next p2 }

        WithNewChannel r ->
            WithNewChannel
                { r | next = mappend r.next p2 }

        Nil ->
            p2


type LayerChannel m
    = LayerChannel Channel


liftMemory_ :
    Layer_ m m1
    -> Procedure c m1 e
    -> Procedure c m e
liftMemory_ o proc =
    case proc of
        Modify r ->
            Modify
                { modify =
                    \m ->
                        case o.get m of
                            Just m1 ->
                                o.set (r.modify m1) m

                            Nothing ->
                                m
                , next = liftMemory_ o r.next
                }

        Push r ->
            Push
                { push =
                    \m ->
                        case o.get m of
                            Just m1 ->
                                r.push m1

                            Nothing ->
                                []
                , next = liftMemory_ o r.next
                }

        RunPromise r ->
            RunPromise
                { promise =
                    mapPromise (liftMemory_ o) r.promise
                        |> liftPromiseMemory o
                }

        Async r ->
            Async
                { async = liftMemory_ o r.async
                , next = liftMemory_ o r.next
                }

        WithNewChannel r ->
            WithNewChannel
                { withNewChannel =
                    \m c ->
                        case o.get m of
                            Nothing ->
                                Nil

                            Just m1 ->
                                liftMemory_ o (r.withNewChannel m1 c)
                , next = liftMemory_ o r.next
                }

        Nil ->
            Nil



-- Primitive Procedures


modify : (m -> m) -> Procedure c m e
modify f =
    Modify
        { modify = f
        , next = Nil
        }


push : (m -> cmd) -> Procedure cmd m e
push f =
    Push
        { push = \m -> [ f m ]
        , next = Nil
        }


runPromise : Promise c m e (Procedure c m e) -> Procedure c m e
runPromise prom =
    RunPromise
        { promise = prom
        }


await : Promise c m e a -> (a -> List (Procedure c m e)) -> Procedure c m e
await prom f =
    mapPromise (f >> batch) prom
        |> runPromise


awaitMsg : (m -> Msg e -> List (Procedure c m e)) -> Procedure c m e
awaitMsg f =
    runPromise <| awaitOnce f


async : List (Procedure c m e) -> Procedure c m e
async procs =
    Async
        { async = batch procs
        , next = Nil
        }


withNewChannel : (Channel -> List (Procedure c m e)) -> Procedure c m e
withNewChannel f =
    WithNewChannel
        { withNewChannel = \_ -> f >> batch
        , next = Nil
        }


type Layer m m1
    = Layer (Layer_ m m1)


type alias Layer_ m m1 =
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    , channel : LayerChannel m1
    }


putLayer :
    { get : m -> Maybe ( Channel, m1 )
    , set : ( Channel, m1 ) -> m -> m
    , init : Channel -> m -> m
    }
    -> (Layer m m1 -> List (Procedure c m e))
    -> Procedure c m e
putLayer o f =
    withNewChannel <|
        \c ->
            [ modify (o.init c)
            , batch <|
                f <|
                    Layer
                        { get =
                            \m ->
                                o.get m
                                    |> Maybe.andThen
                                        (\( c_, m1 ) ->
                                            if c_ == c then
                                                Just m1

                                            else
                                                Nothing
                                        )
                        , set = \m1 -> o.set ( c, m1 )
                        , channel = LayerChannel c
                        }
            ]


onLayer : Layer m m1 -> List (Procedure c m1 e) -> Procedure c m e
onLayer (Layer layer) procs =
    liftMemory_ layer <| batch procs


addListener : (m -> Sub e) -> (e -> List (Procedure c m e)) -> Procedure c m e
addListener sub handler =
    async
        [ runPromise <|
            Promise <|
                \context ->
                    let
                        mySubId =
                            context.nextSubId

                        (LayerChannel layerChannel) =
                            context.layerChannel

                        newContext =
                            { context
                                | nextSubId = SubId.inc context.nextSubId
                                , subs =
                                    ( layerChannel
                                    , mySubId
                                    , sub context.state |> Sub.map toSubMsg
                                    )
                                        :: context.subs
                            }

                        toSubMsg e =
                            SubMsg
                                { subId = mySubId
                                , event = e
                                }

                        awaitForever m msg =
                            case msg of
                                SubMsg subMsg ->
                                    if subMsg.subId == mySubId then
                                        Resolved <|
                                            batch
                                                [ async
                                                    [ runPromise <| listenerPromise m
                                                    ]
                                                , handler subMsg.event
                                                    |> batch
                                                ]

                                    else
                                        AwaitAgain <| listenerPromise m

                                _ ->
                                    AwaitAgain <| listenerPromise m

                        listenerPromise _ =
                            Promise <|
                                \nextContext ->
                                    { newContext = nextContext
                                    , cmds = []
                                    , handler = awaitForever
                                    }
                    in
                    { newContext = newContext
                    , cmds = []
                    , handler = awaitForever
                    }
        ]


portRequest :
    { request : m -> RequestId -> c
    , receiver : (Value -> Msg e) -> Sub (Msg e)
    , requestId : Value -> Maybe RequestId
    , responseBody : Value -> resp
    }
    -> Promise c m e resp
portRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                mySubId =
                    context.nextSubId

                (LayerChannel layerChannel) =
                    context.layerChannel

                handler : m -> Msg e -> PromiseResult c m e resp
                handler _ msg =
                    case msg of
                        ResponseMsg respMsg ->
                            if respMsg.requestId == myRequestId then
                                Resolved <| o.responseBody respMsg.response

                            else
                                AwaitAgain <|
                                    Promise <|
                                        \nextContext ->
                                            { newContext = nextContext
                                            , cmds = []
                                            , handler = handler
                                            }

                        _ ->
                            AwaitAgain <|
                                Promise <|
                                    \nextContext ->
                                        { newContext = nextContext
                                        , cmds = []
                                        , handler = handler
                                        }
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                    , nextSubId = SubId.inc context.nextSubId
                    , subs =
                        ( layerChannel
                        , mySubId
                        , o.receiver
                            (\resp ->
                                case o.requestId resp of
                                    Nothing ->
                                        NoOp

                                    Just requestId ->
                                        if requestId == myRequestId then
                                            ResponseMsg
                                                { requestId = myRequestId
                                                , response = resp
                                                , closeSub = Just mySubId
                                                }

                                        else
                                            NoOp
                            )
                        )
                            :: context.subs
                }
            , cmds =
                [ o.request
                    context.state
                    myRequestId
                ]
            , handler = handler
            }



-- TEA


init :
    memory
    -> List (Procedure cmd memory event)
    -> ( Model cmd memory event, List cmd )
init m procs =
    toModel (initContext m) (batch procs)


initContext : m -> Context m e
initContext memory =
    { state = memory
    , subs = []
    , layerChannel = LayerChannel Channel.init
    , nextSubId = SubId.init
    , nextRequestId = RequestId.init
    , nextChannel = Channel.inc Channel.init
    }


toModel : Context m e -> Procedure c m e -> ( Model c m e, List c )
toModel context proc =
    case proc of
        Modify r ->
            toModel
                { context
                    | state = r.modify context.state
                }
                r.next

        Push r ->
            let
                ( model, cmds ) =
                    toModel context r.next
            in
            ( model, r.push context.state ++ cmds )

        RunPromise r ->
            let
                (Promise prom) =
                    r.promise

                eff =
                    prom context

                next : Msg e -> Context m e -> ( Model c m e, List c )
                next msg nextContext =
                    case eff.handler nextContext.state msg of
                        AwaitAgain nextPromise ->
                            toModel nextContext <|
                                RunPromise
                                    { promise = nextPromise }

                        Resolved nextProc ->
                            toModel nextContext nextProc

                        Reject ->
                            ( EndOfProcess, [] )
            in
            ( OnGoing
                { context = eff.newContext
                , next = next
                }
            , eff.cmds
            )

        Async r ->
            toModel context (concurrent r.async r.next)

        WithNewChannel r ->
            let
                newChannel =
                    context.nextChannel

                newContext =
                    { context
                        | nextChannel = Channel.inc context.nextChannel
                    }

                newProc =
                    r.withNewChannel context.state newChannel
            in
            toModel newContext (mappend newProc r.next)

        Nil ->
            ( EndOfProcess
            , []
            )


concurrent : Procedure c m e -> Procedure c m e -> Procedure c m e
concurrent p1 p2 =
    let
        concurrentRequest r1 r2 =
            RunPromise
                { promise =
                    mapPromise concurrent r1.promise
                        |> andAsyncPromise r2.promise
                }
    in
    case ( p1, p2 ) of
        ( RunPromise r1, _ ) ->
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

                WithNewChannel r2 ->
                    WithNewChannel
                        { r2
                            | next = concurrent p1 r2.next
                        }

                Nil ->
                    p1

        ( _, RunPromise r2 ) ->
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
update msg model =
    case model of
        EndOfProcess ->
            ( EndOfProcess, [] )

        OnGoing onGoing ->
            let
                context =
                    onGoing.context
            in
            case msg of
                NoOp ->
                    ( model, [] )

                ChannelMsg _ ->
                    onGoing.next msg context

                SubMsg _ ->
                    onGoing.next msg context

                ResponseMsg r ->
                    let
                        newContext =
                            case r.closeSub of
                                Just subId ->
                                    { context
                                        | subs =
                                            List.filter
                                                (\( _, sid, _ ) ->
                                                    sid /= subId
                                                )
                                                context.subs
                                    }

                                Nothing ->
                                    context
                    in
                    onGoing.next msg newContext
