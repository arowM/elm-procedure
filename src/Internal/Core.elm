module Internal.Core exposing
    ( Model(..)
    , Msg(..)
    , mapMsg
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
    , mapCmd, liftEvent
    , modify, await, async, withMemory, jump, addListener
    , putLayer, onLayer
    , init, update
    , Context, Layer(..), Pointer, ThisLayerId(..), runNavCmd
    )

{-|


# Core

@docs Model
@docs Msg
@docs mapMsg


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
@docs mapCmd, liftEvent


# Primitive Procedures

@docs modify, await, async, withMemory, jump, addListener


# Layer

@docs putLayer, onLayer


# TEA

@docs init, update

-}

import Browser.Navigation as Nav
import Internal.LayerId as LayerId exposing (LayerId)
import Internal.RequestId as RequestId exposing (RequestId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)



-- Model


type Model cmd memory event
    = OnGoing (OnGoing_ cmd memory event)
    | EndOfProcess (EndOfProcess_ memory)


type alias OnGoing_ c m e =
    -- New context after the evaluation.
    { context : Context m
    , listeners : List (Listener e)

    -- New state to evaluate next time.
    , next : Msg e -> Context m -> List (Listener e) -> ( Model c m e, List c )
    }


type alias EndOfProcess_ m =
    { lastState : m
    }


{-| Execution time context for procedures
-}
type alias Context memory =
    { state : memory
    , thisLayerId : ThisLayerId memory
    , nextRequestId : RequestId
    , nextLayerId : LayerId
    }


type alias Listener event =
    { layerId : LayerId
    , requestId : RequestId
    , name : String
    , sub : Sub (Msg event)
    }


wrapListener : (e1 -> e0) -> Listener e1 -> Listener e0
wrapListener wrap listener1 =
    { layerId = listener1.layerId
    , requestId = listener1.requestId
    , name =listener1.name
    , sub = Sub.map (mapMsg wrap) listener1.sub
    }



-- Msg


type Msg event
    = LayerMsg
        { layerId : LayerId
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


mapMsg : (event1 -> event0) -> Msg event1 -> Msg event0
mapMsg f msg1 =
    case msg1 of
        LayerMsg r ->
            LayerMsg
                { layerId = r.layerId
                , event = f r.event
                }
        ResponseMsg r ->
            ResponseMsg
                { requestId = r.requestId
                , event = f r.event
                }
        PortResponseMsg r ->
            PortResponseMsg
                { requestId = r.requestId
                , response = r.response
                }
        ListenerMsg r ->
            ListenerMsg
                { requestId = r.requestId
                , event = f r.event
                }
        NoOp ->
            NoOp


unwrapMsg : (e0 -> Maybe e1) -> Msg e0 -> Msg e1
unwrapMsg f msg1 =
    case msg1 of
        LayerMsg r ->
            case f r.event of
                Nothing ->
                    NoOp
                Just e1 ->
                    LayerMsg
                        { layerId = r.layerId
                        , event = e1
                        }
        ResponseMsg r ->
            case f r.event of
                Nothing ->
                    NoOp
                Just e1 ->
                    ResponseMsg
                        { requestId = r.requestId
                        , event = e1
                        }
        PortResponseMsg r ->
            PortResponseMsg
                { requestId = r.requestId
                , response = r.response
                }
        ListenerMsg r ->
            case f r.event of
                Nothing -> NoOp
                Just e ->
                    ListenerMsg
                        { requestId = r.requestId
                        , event = e
                        }
        NoOp ->
            NoOp



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
    = Promise (Context m -> PromiseEffect c m e a)


type alias PromiseEffect c m e a =
    { newContext : Context m
    , cmds : List c
    , addListeners : List (Listener e)
    , closedLayers : List LayerId
    , handler : PromiseHandler c m e a
    }


type PromiseHandler c m e a
    = Resolved a
    | Rejected
    | AwaitMsg (Msg e -> m -> Promise c m e a)


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
            , addListeners = []
            , closedLayers = []
            , handler =
                case effA.handler of
                    Resolved a ->
                        Resolved <| f a

                    Rejected ->
                        Rejected

                    AwaitMsg next ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise f (next msg m)
            }


succeedPromise : a -> Promise c m e a
succeedPromise a =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , handler = Resolved a
            }

failPromise : Promise c m e a
failPromise =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , handler= Rejected
            }


justAwaitPromise : (Msg e -> m -> Promise c m e a) -> Promise c m e a
justAwaitPromise f =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
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
            , addListeners = effF.addListeners ++ effA.addListeners
            , closedLayers = effF.closedLayers ++ effA.closedLayers
            , handler =
                case ( effF.handler, effA.handler ) of
                    ( Resolved f, Resolved a ) ->
                        Resolved <| f a

                    ( Resolved f, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise f (nextPromA msg m)

                    ( AwaitMsg nextPromF, Resolved a ) ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromise (\f -> f a) (nextPromF msg m)

                    ( AwaitMsg nextPromF, AwaitMsg nextPromA ) ->
                        AwaitMsg <|
                            \msg m ->
                                andAsyncPromise (nextPromA msg m) (nextPromF msg m)

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
            , addListeners = eff1.addListeners ++ eff2.addListeners
            , closedLayers = eff1.closedLayers ++ eff2.closedLayers
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
                            \msg m ->
                                andRacePromise
                                    (nextProm2 msg m)
                                    (nextProm1 msg m)
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
                    , addListeners = effA.addListeners ++ effB.addListeners
                    , closedLayers = effA.closedLayers ++ effB.closedLayers
                    , handler = effB.handler
                    }

                Rejected ->
                    { newContext = effA.newContext
                    , cmds = effA.cmds
                    , addListeners = effA.addListeners
                    , closedLayers = effA.closedLayers
                    , handler = Rejected
                    }

                AwaitMsg promNextA ->
                    { newContext = effA.newContext
                    , cmds = effA.cmds
                    , addListeners = effA.addListeners
                    , closedLayers = effA.closedLayers
                    , handler =
                        AwaitMsg <|
                            \msg m ->
                                promNextA msg m
                                    |> andThenPromise f
                    }


liftPromiseMemory :
    Layer_ m m1
    -> Promise c m1 e a
    -> Promise c m e a
liftPromiseMemory o (Promise prom1) =
    Promise <|
        \context ->
            case o.get context.state of
                Nothing ->
                    let
                        (ThisLayerId closedLayersId) =
                            o.layerId
                    in
                    { newContext = context
                    , addListeners = []
                    , closedLayers = [ closedLayersId ]
                    , cmds = []
                    , handler = Rejected
                    }

                Just state1 ->
                    let
                        eff1 =
                            prom1
                                { state = state1
                                , thisLayerId = o.layerId
                                , nextRequestId = context.nextRequestId
                                , nextLayerId = context.nextLayerId
                                }
                    in
                    { newContext =
                        { state = o.set eff1.newContext.state context.state
                        , thisLayerId = context.thisLayerId
                        , nextRequestId = eff1.newContext.nextRequestId
                        , nextLayerId = eff1.newContext.nextLayerId
                        }
                    , cmds = eff1.cmds
                    , addListeners = eff1.addListeners
                    , closedLayers = eff1.closedLayers
                    , handler =
                        case eff1.handler of
                            Resolved a ->
                                Resolved a

                            Rejected ->
                                Rejected

                            AwaitMsg nextProm ->
                                AwaitMsg <|
                                    \msg m ->
                                        case o.get m of
                                            Nothing ->
                                                failPromise
                                            Just m1 ->
                                                liftPromiseMemory o (nextProm msg m1)
                    }


mapPromiseCmd : (c -> cmd) -> Promise c m e a -> Promise cmd m e a
mapPromiseCmd f (Promise prom) =
    Promise <|
        \context ->
            let
                eff = prom context
            in
            { newContext = eff.newContext
            , cmds = List.map f eff.cmds
            , addListeners = eff.addListeners
            , closedLayers = eff.closedLayers
            , handler =
                case eff.handler of
                    Resolved a ->
                        Resolved a

                    Rejected ->
                        Rejected

                    AwaitMsg nextProm ->
                        AwaitMsg <|
                            \msg m ->
                                mapPromiseCmd f (nextProm msg m)
            }


liftPromiseEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Promise c m e1 a -> Promise c m e0 a
liftPromiseEvent o (Promise prom1) =
    Promise <|
        \context ->
            let
                eff1 = prom1 context
            in
            { newContext = eff1.newContext
            , cmds = eff1.cmds
            , addListeners = List.map (wrapListener o.wrap) eff1.addListeners
            , closedLayers = eff1.closedLayers
            , handler =
                case eff1.handler of
                    Resolved a ->
                        Resolved a

                    Rejected ->
                        Rejected

                    AwaitMsg nextProm1 ->
                        AwaitMsg <|
                            \msg0 m ->
                                liftPromiseEvent o
                                    (nextProm1
                                        (unwrapMsg o.unwrap msg0)
                                        m
                                    )
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
    | WithNewLayerId
        -- Supply `m` parameter not to assign new Layer Id on expired Layers.
        { withNewLayerId : m -> LayerId -> Procedure c m e
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

        WithNewLayerId r ->
            WithNewLayerId
                { r | next = mappend r.next p2 }

        WithMemory r ->
            WithMemory
                { r | next = \m -> mappend (r.next m) p2 }

        Jump r ->
            Jump r

        Nil ->
            p2


type ThisLayerId m
    = ThisLayerId LayerId


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

        WithNewLayerId r ->
            WithNewLayerId
                { withNewLayerId =
                    \m c ->
                        case o.get m of
                            Nothing ->
                                Nil

                            Just m1 ->
                                liftMemory_ o (r.withNewLayerId m1 c)
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

        WithNewLayerId r ->
            WithNewLayerId
                { withNewLayerId =
                    \m c ->
                        mapCmd f (r.withNewLayerId m c)
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


liftEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    } -> Procedure c m e1 -> Procedure c m e0
liftEvent o proc =
    case proc of
        Modify r ->
            Modify
            { modify = r.modify
            , next = liftEvent o r.next
            }

        RunPromise r ->
            RunPromise
            { promise =
                liftPromiseEvent o r.promise
                    |> mapPromise (liftEvent o)
            }

        Async r ->
            Async
            { async = liftEvent o r.async
            , next = liftEvent o r.next
            }

        WithNewLayerId r ->
            WithNewLayerId
            { withNewLayerId = \m lid ->
                liftEvent o (r.withNewLayerId m lid)
            , next = liftEvent o r.next
            }
        WithMemory r ->
            WithMemory
            { next = \m ->
                liftEvent o (r.next m)
            }
        Jump r ->
            Jump
            { jumpTo = liftEvent o r.jumpTo
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
    , layerId : ThisLayerId m1
    }


type alias Pointer m m1 =
    { get : m -> Maybe ( LayerId, m1 )
    , set : ( LayerId, m1 ) -> m -> m
    }


withNewLayerId : (LayerId -> List (Procedure c m e)) -> Procedure c m e
withNewLayerId f =
    WithNewLayerId
        { withNewLayerId = \_ -> f >> concat
        , next = Nil
        }


putLayer :
    { pointer : Pointer m m1
    , init : LayerId -> m -> m
    }
    -> (Layer m m1 -> List (Procedure c m e))
    -> Procedure c m e
putLayer o f =
    withNewLayerId <|
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
                        , layerId = ThisLayerId c
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

                        (ThisLayerId thisLayerId) =
                            context.thisLayerId

                        newContext =
                            { context
                                | nextRequestId = RequestId.inc context.nextRequestId
                            }

                        toListenerMsg e =
                            ListenerMsg
                                { requestId = myRequestId
                                , event = e
                                }

                        awaitForever : Msg e -> m -> Promise c m e (Procedure c m e)
                        awaitForever msg _ =
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
                    , addListeners =
                        [ { layerId = thisLayerId
                          , requestId = myRequestId
                          , name = name
                          , sub = subscription context.state |> Sub.map toListenerMsg
                          }
                        ]
                    , closedLayers = []
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

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise c m e resp
                nextPromise msg _ =
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
                }
            , addListeners =
                [ { layerId = thisLayerId
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
                ]
            , closedLayers = []
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

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise c m e e
                nextPromise msg _ =
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
                }
            , addListeners =
                [ { layerId = thisLayerId
                  , requestId = myRequestId
                  , name = o.name
                  , sub = Sub.none
                  }
                ]
            , closedLayers = []
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
            , addListeners = []
            , closedLayers = []
            , handler = Resolved ()
            }


layerEvent : (e -> m -> Maybe a) -> Promise c m e a
layerEvent f =
    Promise <|
        \context ->
            let
                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                handler : Msg e -> m -> Promise c m e a
                handler msg m =
                    case msg of
                        LayerMsg r ->
                            if r.layerId /= thisLayerId then
                                justAwaitPromise handler

                            else
                                case f r.event m of
                                    Nothing ->
                                        justAwaitPromise handler

                                    Just a ->
                                        succeedPromise a

                        _ ->
                            justAwaitPromise handler
            in
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , handler = AwaitMsg handler
            }



-- TEA


init :
    memory
    -> List (Procedure cmd memory event)
    -> ( Model cmd memory event, List cmd )
init m procs =
    toModel (initContext m) [] (concat procs)


initContext : m -> Context m
initContext memory =
    { state = memory
    , thisLayerId = ThisLayerId LayerId.init
    , nextRequestId = RequestId.init
    , nextLayerId = LayerId.inc LayerId.init
    }


toModel : Context m -> List (Listener e) -> Procedure c m e -> ( Model c m e, List c )
toModel context listeners proc =
    case proc of
        Modify r ->
            toModel
                { context
                    | state = r.modify context.state
                }
                listeners
                r.next

        RunPromise r ->
            let
                (Promise prom) =
                    r.promise

                eff =
                    prom context
                newListeners =
                    eff.addListeners ++ listeners
                        |> List.filter
                            (\listener ->
                                not (List.member listener.layerId eff.closedLayers)
                            )
            in
            case eff.handler of
                Resolved nextProm ->
                    let
                        ( newModel, newCmds ) =
                            toModel
                                eff.newContext
                                newListeners
                                nextProm
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
                        , listeners = newListeners

                        , next =
                            \msg nextContext nextListeners ->
                                toModel
                                    nextContext
                                    nextListeners
                                    (runPromise (nextProm msg nextContext.state))
                        }
                    , eff.cmds
                    )

        Async r ->
            toModel context listeners (concurrent r.async r.next)

        WithNewLayerId r ->
            let
                newLayerId =
                    context.nextLayerId

                newContext =
                    { context
                        | nextLayerId = LayerId.inc context.nextLayerId
                    }

                newProc =
                    r.withNewLayerId context.state newLayerId
            in
            toModel newContext listeners (mappend newProc r.next)

        WithMemory r ->
            toModel context listeners (r.next context.state)

        Jump r ->
            toModel context listeners r.jumpTo

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

                WithNewLayerId r2 ->
                    WithNewLayerId
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

                WithNewLayerId r1 ->
                    WithNewLayerId
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
                { context, listeners } = onGoing
            in
            case msg of
                NoOp ->
                    ( model, [] )

                LayerMsg _ ->
                    onGoing.next msg context listeners

                ListenerMsg _ ->
                    onGoing.next msg context listeners

                ResponseMsg r ->
                    let
                        newListeners =
                                    List.filter
                                        (\sub ->
                                            sub.requestId /= r.requestId
                                        )
                                        listeners
                    in
                    onGoing.next msg context newListeners

                PortResponseMsg r ->
                    let
                        newListeners =
                                    List.filter
                                        (\sub ->
                                            sub.requestId /= r.requestId
                                        )
                                        listeners
                    in
                    onGoing.next msg context newListeners
