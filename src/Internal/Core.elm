module Internal.Core exposing
    ( Model(..)
    , Msg(..)
    , Key(..)
    , Promise(..)
    , succeedPromise
    , mapPromise
    , andAsyncPromise
    , andRacePromise
    , andThenPromise
    , liftPromiseMemory
    , portRequest, customRequest
    , push
    , layerEvent
    , Procedure(..)
    , none, concat
    , mapCmd
    , modify, await, async, withMemory, jump, addListener
    , putLayer, onLayer
    , init, update
    , Context, Layer(..), LayerChannel(..), Pointer, requestChannelEvent, runNavCmd
    )

{-|


# Core

@docs Model
@docs Msg


# Key

@docs Key


# Promise

@docs Promise
@docs succeedPromise
@docs mapPromise
@docs andAsyncPromise
@docs andRacePromise
@docs andThenPromise
@docs liftPromiseMemory
@docs portRequest, customRequest
@docs push
@docs layerEvent


# Procedure

@docs Procedure
@docs none, concat
@docs mapCmd


# Primitive Procedures

@docs modify, await, async, withMemory, jump, addListener


# Layer

@docs putLayer, onLayer


# TEA

@docs init, update

-}

import Browser.Navigation as Nav
import Internal.Channel as Channel exposing (Channel)
import Internal.RequestId as RequestId exposing (RequestId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)



-- Model


type Model cmd memory event
    = OnGoing (OnGoing_ cmd memory event)
    | EndOfProcess (EndOfProcess_ memory)


type alias OnGoing_ c m e =
    -- New context after the evaluation.
    { context : Context m e

    -- New state to evaluate next time.
    , next : Msg e -> Context m e -> ( Model c m e, List c )
    }


type alias EndOfProcess_ m =
    { lastState : m
    }


{-| Execution time context for procedures
-}
type alias Context memory event =
    { state : memory
    , listeners : List (Listener event)
    , layerChannel : LayerChannel memory
    , nextRequestId : RequestId
    , nextChannel : Channel
    }


type alias Listener event =
    { channel : Channel
    , requestId : RequestId
    , name : String
    , sub : Sub (Msg event)
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
                , listeners = context.listeners
                , layerChannel = layer.channel
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
        , event : event
        }
    | PortResponseMsg
        { requestId : RequestId
        , response : Value
        }
    | ListenerMsg
        { requestId : RequestId
        , event : event
        }
    | NoOp



-- Key


type Key
    = RealKey Nav.Key
    | SimKey


runNavCmd : (Nav.Key -> Cmd msg) -> Key -> Cmd msg
runNavCmd f key =
    case key of
        RealKey k ->
            f k

        _ ->
            Cmd.none



-- Promise


type Promise c m e a
    = Promise (Context m e -> PromiseEffect c m e a)


type alias PromiseEffect c m e a =
    { newContext : Context m e
    , cmds : List c
    , handler : PromiseHandler c m e a
    }


type PromiseHandler c m e a
    = Resolved a
    | Rejected
    | AwaitMsg (Msg e -> Promise c m e a)


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
                case effA.handler of
                    Resolved a ->
                        Resolved <| f a

                    Rejected ->
                        Rejected

                    AwaitMsg next ->
                        AwaitMsg <|
                            \msg ->
                                mapPromise f (next msg)
            }


succeedPromise : a -> Promise c m e a
succeedPromise a =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , handler = Resolved a
            }


justAwaitPromise : (Msg e -> Promise c m e a) -> Promise c m e a
justAwaitPromise f =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , handler = AwaitMsg f
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
                case ( effF.handler, effA.handler ) of
                    ( Resolved f, Resolved a ) ->
                        Resolved <| f a

                    ( Resolved f, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg ->
                                mapPromise f (nextPromA msg)

                    ( AwaitMsg nextPromF, Resolved a ) ->
                        AwaitMsg <|
                            \msg ->
                                mapPromise (\f -> f a) (nextPromF msg)

                    ( AwaitMsg nextPromF, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg ->
                                andAsyncPromise (nextPromA msg) (nextPromF msg)

                    ( Rejected, _ ) ->
                        Rejected

                    ( _, Rejected ) ->
                        Rejected
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
                case ( eff1.handler, eff2.handler ) of
                    ( Resolved a1, _ ) ->
                        Resolved a1

                    ( _, Resolved a2 ) ->
                        Resolved a2

                    ( Rejected, res2 ) ->
                        res2

                    ( res1, Rejected ) ->
                        res1

                    ( AwaitMsg nextProm1, AwaitMsg nextProm2 ) ->
                        AwaitMsg <|
                            \msg ->
                                andRacePromise
                                    (nextProm2 msg)
                                    (nextProm1 msg)
            }


andThenPromise : (a -> Promise c m e b) -> Promise c m e a -> Promise c m e b
andThenPromise f (Promise promA) =
    Promise <|
        \context ->
            let
                effA =
                    promA context
            in
            case effA.handler of
                Resolved a ->
                    let
                        (Promise promB) =
                            f a

                        effB =
                            promB effA.newContext
                    in
                    { newContext = effB.newContext
                    , cmds = effA.cmds ++ effB.cmds
                    , handler = effB.handler
                    }

                Rejected ->
                    { newContext = effA.newContext
                    , cmds = effA.cmds
                    , handler = Rejected
                    }

                AwaitMsg promNextA ->
                    { newContext = effA.newContext
                    , cmds = effA.cmds
                    , handler =
                        AwaitMsg <|
                            \msg ->
                                promNextA msg
                                    |> andThenPromise f
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
                            | listeners =
                                List.filter
                                    (\sub ->
                                        sub.channel /= closedChannel
                                    )
                                    context.listeners
                        }
                    , cmds = []
                    , handler = Rejected
                    }

                Just context1 ->
                    let
                        eff1 =
                            prom1 context1
                    in
                    { newContext =
                        setContext o eff1.newContext context
                    , cmds = eff1.cmds
                    , handler =
                        case eff1.handler of
                            Resolved a ->
                                Resolved a

                            Rejected ->
                                Rejected

                            AwaitMsg nextProm ->
                                AwaitMsg <|
                                    \msg ->
                                        liftPromiseMemory o (nextProm msg)
                    }


mapPromiseCmd : (c -> cmd) -> Promise c m e a -> Promise cmd m e a
mapPromiseCmd f (Promise prom) =
    Promise <|
        \context ->
            let
                eff =
                    prom context
            in
            { newContext = eff.newContext
            , cmds = List.map f eff.cmds
            , handler =
                case eff.handler of
                    Resolved a ->
                        Resolved a

                    Rejected ->
                        Rejected

                    AwaitMsg nextProm ->
                        AwaitMsg <|
                            \msg ->
                                mapPromiseCmd f (nextProm msg)
            }


requestChannelEvent : (e -> Maybe a) -> Promise c m e a
requestChannelEvent f =
    Promise <|
        \context ->
            let
                (LayerChannel thisChannel) =
                    context.layerChannel

                handler : Msg e -> Promise c m e a
                handler msg =
                    case msg of
                        ChannelMsg r ->
                            if r.channel /= thisChannel then
                                justAwaitPromise handler

                            else
                                case f r.event of
                                    Nothing ->
                                        justAwaitPromise handler

                                    Just a ->
                                        succeedPromise a

                        _ ->
                            justAwaitPromise handler
            in
            { newContext = context
            , cmds = []
            , handler = AwaitMsg handler
            }



-- Procedure


type Procedure c m e
    = Modify
        { modify : m -> m
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
    | WithMemory
        { next : m -> Procedure c m e
        }
    | Jump
        { jumpTo : Procedure c m e
        }
    | Nil


none : Procedure c m e
none =
    Nil


concat : List (Procedure c m e) -> Procedure c m e
concat =
    List.foldr mappend Nil


mappend : Procedure c m e -> Procedure c m e -> Procedure c m e
mappend p1 p2 =
    case p1 of
        Modify r ->
            Modify
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

        WithMemory r ->
            WithMemory
                { r | next = \m -> mappend (r.next m) p2 }

        Jump r ->
            Jump r

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

        WithMemory r ->
            WithMemory
                { next =
                    \m ->
                        case o.get m of
                            Nothing ->
                                Nil

                            Just m1 ->
                                liftMemory_ o (r.next m1)
                }

        Jump r ->
            Jump
                { jumpTo = liftMemory_ o r.jumpTo
                }

        Nil ->
            Nil


mapCmd : (c -> cmd) -> Procedure c m e -> Procedure cmd m e
mapCmd f proc =
    case proc of
        Modify r ->
            Modify
                { modify = r.modify
                , next = mapCmd f r.next
                }

        RunPromise r ->
            RunPromise
                { promise =
                    mapPromiseCmd f r.promise
                        |> mapPromise (mapCmd f)
                }

        Async r ->
            Async
                { async = mapCmd f r.async
                , next = mapCmd f r.next
                }

        WithNewChannel r ->
            WithNewChannel
                { withNewChannel =
                    \m c ->
                        mapCmd f (r.withNewChannel m c)
                , next = mapCmd f r.next
                }

        WithMemory r ->
            WithMemory
                { next = \m -> mapCmd f (r.next m)
                }

        Jump r ->
            Jump
                { jumpTo = mapCmd f r.jumpTo }

        Nil ->
            Nil



-- Primitive Procedures


modify : (m -> m) -> Procedure c m e
modify f =
    Modify
        { modify = f
        , next = Nil
        }


runPromise : Promise c m e (Procedure c m e) -> Procedure c m e
runPromise prom =
    RunPromise
        { promise = prom
        }


await : Promise c m e a -> (a -> List (Procedure c m e)) -> Procedure c m e
await prom f =
    mapPromise (f >> concat) prom
        |> runPromise


async : List (Procedure c m e) -> Procedure c m e
async procs =
    Async
        { async = concat procs
        , next = Nil
        }


jump : (() -> List (Procedure c m e)) -> Procedure c m e
jump f =
    Jump
        { jumpTo = concat <| f ()
        }


withMemory : (m -> List (Procedure c m e)) -> Procedure c m e
withMemory f =
    WithMemory
        { next = \m -> concat (f m)
        }


type Layer m m1
    = Layer (Layer_ m m1)


type alias Layer_ m m1 =
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    , channel : LayerChannel m1
    }


type alias Pointer m m1 =
    { get : m -> Maybe ( Channel, m1 )
    , set : ( Channel, m1 ) -> m -> m
    }


withNewChannel : (Channel -> List (Procedure c m e)) -> Procedure c m e
withNewChannel f =
    WithNewChannel
        { withNewChannel = \_ -> f >> concat
        , next = Nil
        }


putLayer :
    { pointer : Pointer m m1
    , init : Channel -> m -> m
    }
    -> (Layer m m1 -> List (Procedure c m e))
    -> Procedure c m e
putLayer o f =
    withNewChannel <|
        \c ->
            [ modify (o.init c)
            , concat <|
                f <|
                    Layer
                        { get =
                            \m ->
                                o.pointer.get m
                                    |> Maybe.andThen
                                        (\( c_, m1 ) ->
                                            if c_ == c then
                                                Just m1

                                            else
                                                Nothing
                                        )
                        , set = \m1 -> o.pointer.set ( c, m1 )
                        , channel = LayerChannel c
                        }
            ]


onLayer : Layer m m1 -> List (Procedure c m1 e) -> Procedure c m e
onLayer (Layer layer) procs =
    liftMemory_ layer <| concat procs


addListener :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Procedure c m e)
    }
    -> Procedure c m e
addListener { name, subscription, handler } =
    async
        [ runPromise <|
            Promise <|
                \context ->
                    let
                        myRequestId =
                            context.nextRequestId

                        (LayerChannel layerChannel) =
                            context.layerChannel

                        newContext =
                            { context
                                | nextRequestId = RequestId.inc context.nextRequestId
                                , listeners =
                                    { channel = layerChannel
                                    , requestId = myRequestId
                                    , name = name
                                    , sub = subscription context.state |> Sub.map toListenerMsg
                                    }
                                        :: context.listeners
                            }

                        toListenerMsg e =
                            ListenerMsg
                                { requestId = myRequestId
                                , event = e
                                }

                        awaitForever : Msg e -> Promise c m e (Procedure c m e)
                        awaitForever msg =
                            case msg of
                                ListenerMsg listenerMsg ->
                                    if listenerMsg.requestId == myRequestId then
                                        succeedPromise <|
                                            concat
                                                [ async
                                                    [ runPromise <| justAwaitPromise awaitForever
                                                    ]
                                                , handler listenerMsg.event
                                                    |> concat
                                                ]

                                    else
                                        justAwaitPromise awaitForever

                                _ ->
                                    justAwaitPromise awaitForever
                    in
                    { newContext = newContext
                    , cmds = []
                    , handler = AwaitMsg awaitForever
                    }
        ]


portRequest :
    { name : String
    , request : m -> { requestId : Value } -> c
    , receiver : (Value -> Msg e) -> Sub (Msg e)
    , response : Decoder RequestId -> Decoder ( RequestId, resp )
    }
    -> Promise c m e resp
portRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (LayerChannel layerChannel) =
                    context.layerChannel

                nextPromise : Msg e -> Promise c m e resp
                nextPromise msg =
                    case msg of
                        PortResponseMsg respMsg ->
                            case JD.decodeValue (o.response RequestId.decoder) respMsg.response of
                                Ok ( requestId, resp ) ->
                                    if requestId == myRequestId then
                                        succeedPromise resp

                                    else
                                        justAwaitPromise nextPromise

                                Err _ ->
                                    justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                    , listeners =
                        { channel = layerChannel
                        , requestId = myRequestId
                        , name = o.name
                        , sub =
                            o.receiver
                                (\respValue ->
                                    case JD.decodeValue (o.response RequestId.decoder) respValue of
                                        Err _ ->
                                            NoOp

                                        Ok ( requestId, _ ) ->
                                            if requestId == myRequestId then
                                                PortResponseMsg
                                                    { requestId = myRequestId
                                                    , response = respValue
                                                    }

                                            else
                                                NoOp
                                )
                        }
                            :: context.listeners
                }
            , cmds =
                [ o.request
                    context.state
                    { requestId = RequestId.toValue myRequestId }
                ]
            , handler = AwaitMsg nextPromise
            }


customRequest :
    { name : String
    , request : m -> RequestId -> (e -> Msg e) -> c
    }
    -> Promise c m e e
customRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (LayerChannel layerChannel) =
                    context.layerChannel

                nextPromise : Msg e -> Promise c m e e
                nextPromise msg =
                    case msg of
                        ResponseMsg respMsg ->
                            if respMsg.requestId == myRequestId then
                                succeedPromise respMsg.event

                            else
                                justAwaitPromise nextPromise

                        _ ->
                            justAwaitPromise nextPromise
            in
            { newContext =
                { context
                    | nextRequestId = RequestId.inc context.nextRequestId
                    , listeners =
                        { channel = layerChannel
                        , requestId = myRequestId
                        , name = o.name
                        , sub = Sub.none
                        }
                            :: context.listeners
                }
            , cmds =
                [ o.request
                    context.state
                    myRequestId
                    (\e ->
                        ResponseMsg
                            { requestId = myRequestId
                            , event = e
                            }
                    )
                ]
            , handler = AwaitMsg nextPromise
            }


push : (m -> List c) -> Promise c m e ()
push f =
    Promise <|
        \context ->
            { newContext = context
            , cmds = f context.state
            , handler = Resolved ()
            }


layerEvent : (e -> Maybe a) -> Promise c m e a
layerEvent f =
    Promise <|
        \context ->
            let
                (LayerChannel thisChannel) =
                    context.layerChannel

                handler : Msg e -> Promise c m e a
                handler msg =
                    case msg of
                        ChannelMsg r ->
                            if r.channel /= thisChannel then
                                justAwaitPromise handler

                            else
                                case f r.event of
                                    Nothing ->
                                        justAwaitPromise handler

                                    Just a ->
                                        succeedPromise a

                        _ ->
                            justAwaitPromise handler
            in
            { newContext = context
            , cmds = []
            , handler = AwaitMsg handler
            }



-- TEA


init :
    memory
    -> List (Procedure cmd memory event)
    -> ( Model cmd memory event, List cmd )
init m procs =
    toModel (initContext m) (concat procs)


initContext : m -> Context m e
initContext memory =
    { state = memory
    , listeners = []
    , layerChannel = LayerChannel Channel.init
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

        RunPromise r ->
            let
                (Promise prom) =
                    r.promise

                eff =
                    prom context
            in
            case eff.handler of
                Resolved nextProm ->
                    let
                        ( newModel, newCmds ) =
                            toModel eff.newContext nextProm
                    in
                    ( newModel, eff.cmds ++ newCmds )

                Rejected ->
                    ( EndOfProcess
                        { lastState = eff.newContext.state
                        }
                    , eff.cmds
                    )

                AwaitMsg nextProm ->
                    ( OnGoing
                        { context = eff.newContext
                        , next =
                            \msg nextContext ->
                                toModel
                                    nextContext
                                    (runPromise (nextProm msg))
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

        WithMemory r ->
            toModel context (r.next context.state)

        Jump r ->
            toModel context r.jumpTo

        Nil ->
            ( EndOfProcess
                { lastState = context.state
                }
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

                WithMemory r2 ->
                    WithMemory
                        { r2
                            | next = \m -> concurrent p1 (r2.next m)
                        }

                Jump r2 ->
                    Jump
                        { r2
                            | jumpTo = concurrent p1 r2.jumpTo
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

                WithMemory r1 ->
                    WithMemory
                        { r1
                            | next = \m -> concurrent (r1.next m) p2
                        }

                Jump r1 ->
                    Jump
                        { r1
                            | jumpTo = concurrent r1.jumpTo p2
                        }

                Nil ->
                    p2

        _ ->
            mappend p1 p2


update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg model =
    case model of
        EndOfProcess r ->
            ( EndOfProcess r
            , []
            )

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

                ListenerMsg _ ->
                    onGoing.next msg context

                ResponseMsg r ->
                    let
                        newContext =
                            { context
                                | listeners =
                                    List.filter
                                        (\sub ->
                                            sub.requestId /= r.requestId
                                        )
                                        context.listeners
                            }
                    in
                    onGoing.next msg newContext

                PortResponseMsg r ->
                    let
                        newContext =
                            { context
                                | listeners =
                                    List.filter
                                        (\sub ->
                                            sub.requestId /= r.requestId
                                        )
                                        context.listeners
                            }
                    in
                    onGoing.next msg newContext
