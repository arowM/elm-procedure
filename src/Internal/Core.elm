module Internal.Core exposing
    ( Model(..)
    , Msg (..)
    , mapMsg
    , Key(..)
    , runNavCmd
    , Promise
    , Layer
    , Pointer
    , layerView
    , succeedPromise
    , mapPromise
    , andRacePromise
    , andThenPromise
    , syncPromise
    , liftPromiseMemory, liftPromiseEvent, mapPromiseCmd
    , portRequest, customRequest
    , layerEvent
    , none, sequence, concurrent
    , modify, push, currentState, reject, listen
    , newLayer, onLayer
    , init, update
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
@docs andRacePromise
@docs andThenPromise
@docs syncPromise
@docs liftPromiseMemory, liftPromiseEvent, mapPromiseCmd
@docs portRequest, customRequest
@docs layerEvent
@docs Layer, Pointer
@docs none, sequence, concurrent

# Primitive Procedures

@docs modify, push, currentState, reject, listen


# Layer

@docs newLayer, onLayer


# TEA

@docs init, update

-}

import Browser.Navigation as Nav
import Html exposing (Html)
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


{-| Execution time context for Procedures
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


syncPromise : Promise c m e a -> Promise c m e (a -> b) -> Promise c m e b
syncPromise (Promise promA) (Promise promF) =
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
                                syncPromise (nextPromA msg m) (nextPromF msg m)

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
    Pointer_ m m1
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




-- Primitive Promises


none : Promise c m e ()
none =
    succeedPromise ()


sequence : List (Promise c m e ()) -> Promise c m e ()
sequence =
    List.foldl
        (\a acc ->
            acc
                |> andThenPromise
                    (\() -> a)
        )
        none

concurrent : List (Promise c m e ()) -> Promise c m e ()
concurrent =
    List.foldl
        (\a acc ->
            acc
                |> mapPromise
                    (\() _ -> ())
                |> syncPromise a
        )
        none

currentState : Promise c m e m
currentState =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , handler = Resolved context.state
            }


genNewLayerId : Promise c m e LayerId
genNewLayerId =
    Promise <|
        \context ->
            let
                newLayerId = context.nextLayerId
                newContext =
                    { context | nextLayerId = LayerId.inc newLayerId }
            in
            { newContext = newContext
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , handler = Resolved newLayerId
            }


modify : (m -> m) -> Promise c m e ()
modify f =
    Promise <|
        \context ->
            { newContext =
                { context
                    | state = f context.state
                }
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , handler = Resolved ()
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



reject : Promise c m e ()
reject =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , handler = Rejected
            }


type Layer m = Layer LayerId m


newLayer :
    { get : (Layer m1 -> Maybe m1) -> m -> Maybe m1
    , modify : (Layer m1 -> Layer m1) -> m -> m
    }
    -> m1 -> Promise c m e (Layer m1, Pointer m m1)
newLayer o m1 =
    genNewLayerId
        |> andThenPromise
            (\layerId ->
                let
                    unwrapper : Layer m1 -> Maybe m1
                    unwrapper (Layer layerId_ m1_) =
                        if layerId_ == layerId then
                            Just m1_
                        else
                            Nothing

                    modifier : m1 -> Layer m1 -> Layer m1
                    modifier newM1 (Layer layerId_ oldM1) =
                        if layerId_ == layerId then
                            Layer layerId newM1
                        else
                            Layer layerId_ oldM1
                in
                succeedPromise <|
                    ( Layer layerId m1
                    , Pointer
                        { get =
                            \m ->
                                o.get unwrapper m
                        , set = \newM1 ->
                            o.modify (modifier newM1)
                        , layerId = ThisLayerId layerId
                        }
                    )
            )


onLayer : Pointer m m1 -> Promise c m1 e a -> Promise c m e a
onLayer (Pointer layer) procs =
    liftPromiseMemory layer procs


layerView : Layer m -> (m -> Html e) -> (String, Html (Msg e))
layerView (Layer layerId m) f =
    ( LayerId.toString layerId
    , f m
        |> Html.map
            (\e -> LayerMsg
                { layerId = layerId
                , event = e
                }
            )
    )


type ThisLayerId m
    = ThisLayerId LayerId

type Pointer m m1
    = Pointer (Pointer_ m m1)


type alias Pointer_ m m1 =
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    , layerId : ThisLayerId m1
    }


listen :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Promise c m e ())
    }
    -> Promise c m e ()
listen { name, subscription, handler } =
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

                awaitForever : Msg e -> m -> Promise c m e ()
                awaitForever msg _ =
                    case msg of
                        ListenerMsg listenerMsg ->
                            if listenerMsg.requestId == myRequestId then
                                concurrent
                                    [ justAwaitPromise awaitForever
                                    , handler listenerMsg.event
                                        |> sequence
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


layerEvent : Promise c m e e
layerEvent =
    Promise <|
        \context ->
            let
                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                handler : Msg e -> m -> Promise c m e e
                handler msg _ =
                    case msg of
                        LayerMsg r ->
                            if r.layerId /= thisLayerId then
                                justAwaitPromise handler

                            else
                                succeedPromise r.event

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
    -> List (Promise cmd memory event ())
    -> ( Model cmd memory event, List cmd )
init m proms =
    toModel (initContext m) [] (sequence proms)


initContext : m -> Context m
initContext memory =
    { state = memory
    , thisLayerId = ThisLayerId LayerId.init
    , nextRequestId = RequestId.init
    , nextLayerId = LayerId.inc LayerId.init
    }


toModel : Context m -> List (Listener e) -> Promise c m e () -> ( Model c m e, List c )
toModel context listeners (Promise prom) =
            let
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
                Resolved () ->
                    ( EndOfProcess
                        { lastState = eff.newContext.state
                        }
                    , eff.cmds
                    )

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
                                    (nextProm msg nextContext.state)
                        }
                    , eff.cmds
                    )


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
