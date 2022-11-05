module Internal.Core exposing
    ( Model(..), memoryState
    , Msg(..), rootLayerMsg
    , mapMsg
    , Key(..)
    , Promise
    , succeedPromise
    , mapPromise
    , andRacePromise
    , andThenPromise
    , syncPromise
    , liftPromiseMemory, liftPromiseEvent, mapPromiseCmd
    , portRequest, customRequest
    , layerEvent
    , Layer(..), Pointer(..), isPointedBy
    , layerView, keyedLayerView, layerDocument, eventAttr, eventMixin
    , none, sequence, concurrent
    , Void, void
    , modify, push, currentState, return, lazy, listen
    , newLayer, onLayer
    , init, update
    , elementView, documentView, subscriptions
    , Scenario(..), TestModel(..)
    , noneScenario, noneTest
    , concatScenario
    , putListItemMarkup
    , toTest
    , toMarkup
    , InvalidMarkupReason(..)
    , invalidMarkup
    , Section
    , section
    , cases
    , Pointer_, runNavCmd
    )

{-|


# Core

@docs Model, memoryState
@docs Msg, rootLayerMsg
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
@docs Layer, Pointer, isPointedBy
@docs layerView, keyedLayerView, layerDocument, eventAttr, eventMixin
@docs none, sequence, concurrent


# Primitive Procedures

@docs Void, void
@docs modify, push, currentState, return, lazy, listen


# Layer

@docs newLayer, onLayer


# TEA

@docs init, update
@docs elementView, documentView, subscriptions


# Scenario

@docs Scenario, TestModel
@docs noneScenario, noneTest
@docs concatScenario
@docs putListItemMarkup
@docs toTest
@docs toMarkup
@docs InvalidMarkupReason
@docs invalidMarkup
@docs Section
@docs section
@docs cases

-}

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Expect
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Internal.LayerId as LayerId exposing (LayerId)
import Internal.Markup as Markup
import Internal.RequestId as RequestId exposing (RequestId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Test exposing (Test)
import Test.Sequence as SeqTest
import Url exposing (Url)



-- Model


type Model cmd memory event
    = OnGoing (OnGoing_ cmd memory event)
    | EndOfProcess (EndOfProcess_ memory)


memoryState : Model cmd memory event -> memory
memoryState model =
    case model of
        OnGoing { context } ->
            context.state

        EndOfProcess { lastState } ->
            lastState


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
    , name = listener1.name
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
        { response : Value
        }
    | ListenerMsg
        { requestId : RequestId
        , event : event
        }
    | ViewStubMsg
        { event : event
        }
    | NoOp


rootLayerMsg : e -> Msg e
rootLayerMsg e =
    LayerMsg
        { layerId = LayerId.init
        , event = e
        }


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
                { response = r.response
                }

        ListenerMsg r ->
            ListenerMsg
                { requestId = r.requestId
                , event = f r.event
                }

        ViewStubMsg r ->
            ViewStubMsg
                { event = f r.event
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
                { response = r.response
                }

        ListenerMsg r ->
            case f r.event of
                Nothing ->
                    NoOp

                Just e ->
                    ListenerMsg
                        { requestId = r.requestId
                        , event = e
                        }

        ViewStubMsg r ->
            case f r.event of
                Nothing ->
                    NoOp

                Just e ->
                    ViewStubMsg
                        { event = e
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
    , closedRequests : List RequestId
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
            , addListeners = effA.addListeners
            , closedLayers = effA.closedLayers
            , closedRequests = effA.closedRequests
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
            , closedRequests = []
            , handler = Resolved a
            }

closeRequest : RequestId -> Promise c m e a -> Promise c m e a
closeRequest rid (Promise prom) =
    Promise <|
        \context ->
            let
                eff = prom context
            in
            { eff | closedRequests = rid :: eff.closedRequests
            }


failPromise : Promise c m e a
failPromise =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , closedRequests = []
            , handler = Rejected
            }


justAwaitPromise : (Msg e -> m -> Promise c m e a) -> Promise c m e a
justAwaitPromise f =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , closedRequests = []
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
            , closedRequests = effF.closedRequests ++ effA.closedRequests
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
            , closedRequests = eff1.closedRequests ++ eff2.closedRequests
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
                    , closedRequests = effA.closedRequests ++ effB.closedRequests
                    , handler = effB.handler
                    }

                Rejected ->
                    { newContext = effA.newContext
                    , cmds = effA.cmds
                    , addListeners = effA.addListeners
                    , closedLayers = effA.closedLayers
                    , closedRequests = effA.closedRequests
                    , handler = Rejected
                    }

                AwaitMsg promNextA ->
                    { newContext = effA.newContext
                    , cmds = effA.cmds
                    , addListeners = effA.addListeners
                    , closedLayers = effA.closedLayers
                    , closedRequests = effA.closedRequests
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
                    , closedRequests = []
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
                    , closedRequests = eff1.closedRequests
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
                eff =
                    prom context
            in
            { newContext = eff.newContext
            , cmds = List.map f eff.cmds
            , addListeners = eff.addListeners
            , closedLayers = eff.closedLayers
            , closedRequests = eff.closedRequests
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
    -> Promise c m e1 a
    -> Promise c m e0 a
liftPromiseEvent o (Promise prom1) =
    Promise <|
        \context ->
            let
                eff1 =
                    prom1 context
            in
            { newContext = eff1.newContext
            , cmds = eff1.cmds
            , addListeners = List.map (wrapListener o.wrap) eff1.addListeners
            , closedLayers = eff1.closedLayers
            , closedRequests = eff1.closedRequests
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


none : Promise c m e Void
none =
    succeedPromise OnGoingProcedure


sequence : List (Promise c m e Void) -> Promise c m e Void
sequence =
    List.foldl
        (\a acc ->
            acc
                |> andThenPromise
                    (\v ->
                        case v of
                            CompletedProcedure ->
                                succeedPromise CompletedProcedure

                            OnGoingProcedure ->
                                a
                    )
        )
        none


concurrent : List (Promise c m e Void) -> Promise c m e Void
concurrent =
    List.foldl
        (\a acc ->
            succeedPromise (\_ _ -> OnGoingProcedure)
                |> syncPromise acc
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
            , closedRequests = []
            , handler = Resolved context.state
            }


genNewLayerId : Promise c m e LayerId
genNewLayerId =
    Promise <|
        \context ->
            let
                newLayerId =
                    context.nextLayerId

                newContext =
                    { context | nextLayerId = LayerId.inc newLayerId }
            in
            { newContext = newContext
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , closedRequests = []
            , handler = Resolved newLayerId
            }


type Void
    = OnGoingProcedure
    | CompletedProcedure


void : Promise c m e a -> Promise c m e Void
void =
    andThenPromise (\_ -> none)


modify : (m -> m) -> Promise c m e Void
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
            , closedRequests = []
            , handler = Resolved OnGoingProcedure
            }


push : (m -> List c) -> Promise c m e Void
push f =
    Promise <|
        \context ->
            { newContext = context
            , cmds = f context.state
            , addListeners = []
            , closedLayers = []
            , closedRequests = []
            , handler = Resolved OnGoingProcedure
            }


return : Promise c m e Void
return =
    Promise <|
        \context ->
            { newContext = context
            , cmds = []
            , addListeners = []
            , closedLayers = []
            , closedRequests = []
            , handler = Resolved CompletedProcedure
            }


lazy : (() -> Promise c m e Void) -> Promise c m e Void
lazy f =
    Promise <|
        \context ->
            let
                (Promise prom) =
                    f ()
            in
            prom context


type Layer m
    = Layer LayerId m


newLayer :
    { get : (Layer m1 -> Maybe m1) -> m -> Maybe m1
    , modify : (Layer m1 -> Layer m1) -> m -> m
    }
    -> m1
    -> Promise c m e ( Layer m1, Pointer m m1 )
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
                        , set =
                            \newM1 ->
                                o.modify (modifier newM1)
                        , layerId = ThisLayerId layerId
                        }
                    )
            )


onLayer : Pointer m m1 -> Promise c m1 e a -> Promise c m e a
onLayer (Pointer layer) procs =
    liftPromiseMemory layer procs


layerView : (m -> Html (Msg e)) -> Layer m -> Html (Msg e)
layerView f (Layer layerId m) =
    f m
        |> Html.map
            (\msg ->
                case msg of
                    ViewStubMsg r ->
                        LayerMsg
                            { layerId = layerId
                            , event = r.event
                            }

                    _ ->
                        msg
            )


keyedLayerView : (m -> Html (Msg e)) -> Layer m -> ( String, Html (Msg e) )
keyedLayerView f (Layer layerId m) =
    ( LayerId.toString layerId
    , f m
        |> Html.map
            (\msg ->
                case msg of
                    ViewStubMsg r ->
                        LayerMsg
                            { layerId = layerId
                            , event = r.event
                            }

                    _ ->
                        msg
            )
    )


layerDocument : (m -> Document (Msg e)) -> Layer m -> Document (Msg e)
layerDocument f (Layer layerId m) =
    f m
        |> (\doc ->
                { title = doc.title
                , body =
                    doc.body
                        |> List.map
                            (Html.map
                                (\msg ->
                                    case msg of
                                        ViewStubMsg r ->
                                            LayerMsg
                                                { layerId = layerId
                                                , event = r.event
                                                }

                                        _ ->
                                            msg
                                )
                            )
                }
           )


eventAttr : Attribute event -> Attribute (Msg event)
eventAttr =
    Attributes.map
        (\e -> ViewStubMsg { event = e })


eventMixin : Mixin e -> Mixin (Msg e)
eventMixin =
    Mixin.map
        (\e -> ViewStubMsg { event = e })


type ThisLayerId m
    = ThisLayerId LayerId


type Pointer m m1
    = Pointer (Pointer_ m m1)


type alias Pointer_ m m1 =
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    , layerId : ThisLayerId m1
    }


isPointedBy : Pointer m m1 -> Layer m1 -> Bool
isPointedBy (Pointer pointer) (Layer layerId _) =
    let
        (ThisLayerId pointerLayerId) =
            pointer.layerId
    in
    pointerLayerId == layerId


listen :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Promise c m e Void)
    }
    -> Promise c m e Void
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

                awaitForever : Msg e -> m -> Promise c m e Void
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
            , closedRequests = []
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
                                            |> closeRequest myRequestId

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
                                                { response = respValue
                                                }

                                        else
                                            NoOp
                            )
                  }
                ]
            , closedLayers = []
            , closedRequests = []
            , cmds =
                [ o.request
                    context.state
                    { requestId = RequestId.toValue myRequestId }
                ]
            , handler = AwaitMsg nextPromise
            }


customRequest :
    { name : String
    , request : (a -> Msg e) -> c
    , wrap : a -> e
    , unwrap : e -> Maybe a
    }
    -> Promise c m e a
customRequest o =
    Promise <|
        \context ->
            let
                myRequestId =
                    context.nextRequestId

                (ThisLayerId thisLayerId) =
                    context.thisLayerId

                nextPromise : Msg e -> m -> Promise c m e a
                nextPromise msg _ =
                    case msg of
                        ResponseMsg respMsg ->
                            if respMsg.requestId == myRequestId then
                                case o.unwrap respMsg.event of
                                    Nothing ->
                                        justAwaitPromise nextPromise

                                    Just a ->
                                        succeedPromise a
                                            |> closeRequest myRequestId

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
            , closedRequests = []
            , cmds =
                [ o.request
                    (\a ->
                        ResponseMsg
                            { requestId = myRequestId
                            , event = o.wrap a
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
            , closedRequests = []
            , handler = AwaitMsg handler
            }



-- TEA


init :
    memory
    -> Promise cmd memory event Void
    -> ( Model cmd memory event, List cmd )
init m prom =
    toModel (initContext m) [] prom


initContext : m -> Context m
initContext memory =
    { state = memory
    , thisLayerId = ThisLayerId LayerId.init
    , nextRequestId = RequestId.init
    , nextLayerId = LayerId.inc LayerId.init
    }


toModel : Context m -> List (Listener e) -> Promise c m e Void -> ( Model c m e, List c )
toModel context listeners (Promise prom) =
    let
        eff =
            prom context

        newListeners =
            eff.addListeners
                ++ listeners
                |> List.filter
                    (\listener ->
                        not (List.member listener.layerId eff.closedLayers) &&
                        not (List.member listener.requestId eff.closedRequests)
                    )
    in
    case eff.handler of
        Resolved _ ->
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
                { context, listeners } =
                    onGoing
            in
            onGoing.next msg context listeners


elementView : (Layer memory -> Html (Msg event)) -> Model cmd memory event -> Html (Msg event)
elementView f model =
    f (Layer LayerId.init (memoryState model))


documentView : (Layer memory -> Document (Msg event)) -> Model cmd memory event -> Document (Msg event)
documentView f model =
    f (Layer LayerId.init (memoryState model))


subscriptions : Model cmd memory event -> Sub (Msg event)
subscriptions model =
    case model of
        EndOfProcess _ ->
            Sub.none

        OnGoing { listeners } ->
            listeners
                |> List.map .sub
                |> Sub.batch



-- Scenario


type Scenario flags c m e
    = Scenario
        { test : TestConfig flags c m e -> TestContext c m e -> SeqTest.Sequence (TestModel c m e)
        , markup : MarkupContext -> MarkupBuilder
        }


type TestModel c m e
    = OnGoingTest (TestContext c m e)
    | TestAfterCases


type alias TestConfig flags c m e =
    { view : m -> Html ()
    , init : flags -> Url -> ( Model c m e, List c )
    }


type alias TestContext c m e =
    Dict SessionId ( Model c m e, List c )


type alias SessionId =
    String


type alias MarkupContext =
    List ( Mixin (), Markup.BlockElement ) -> Markup.Section


type MarkupBuilder
    = OnGoingMarkup MarkupContext
    | MarkupAfterCases (List Markup.Section)
    | InvalidMarkup InvalidMarkupReason


invalidMarkup : InvalidMarkupReason -> MarkupContext -> MarkupBuilder
invalidMarkup reason _ =
    InvalidMarkup reason


type InvalidMarkupReason
    = SiblingScenarioAfterCases
    | OtherInvalidMarkup String


noneScenario : Scenario flags c m e
noneScenario =
    Scenario
        { test = noneTest
        , markup = noneMarkup
        }


noneTest : TestConfig flags c m e -> TestContext c m e -> SeqTest.Sequence (TestModel c m e)
noneTest _ =
    SeqTest.pass >> SeqTest.map OnGoingTest


noneMarkup : MarkupContext -> MarkupBuilder
noneMarkup =
    OnGoingMarkup


putListItemMarkup : Markup.BlockElement -> MarkupContext -> MarkupBuilder
putListItemMarkup item context =
    OnGoingMarkup <|
        \ls ->
            context (( Mixin.none, item ) :: ls)


concatScenario : List (Scenario flags c m e) -> Scenario flags c m e
concatScenario =
    List.foldl
        (\a acc ->
            mappendScenario acc a
        )
        noneScenario


mappendScenario : Scenario flags c m e -> Scenario flags c m e -> Scenario flags c m e
mappendScenario (Scenario s1) (Scenario s2) =
    Scenario
        { test =
            \config context ->
                s1.test config context
                    |> SeqTest.andThen
                        (\m ->
                            case m of
                                OnGoingTest nextContext ->
                                    s2.test config nextContext

                                TestAfterCases ->
                                    SeqTest.fail "Scenario structure" <|
                                        \_ ->
                                            Expect.fail "should not have sibling scenarios after `cases`."
                        )
        , markup =
            \context ->
                case s1.markup context of
                    MarkupAfterCases _ ->
                        InvalidMarkup SiblingScenarioAfterCases

                    InvalidMarkup reason ->
                        InvalidMarkup reason

                    OnGoingMarkup nextContext ->
                        s2.markup nextContext
        }


toTest :
    { init : memory
    , procedure : flags -> Url -> Key -> Promise cmd memory event Void
    , view : Layer memory -> Html (Msg event)
    , sections : List (Section flags cmd memory event)
    }
    -> Test
toTest o =
    List.map
        (\(Section sec) ->
            let
                (Scenario { test }) =
                    sec.content
            in
            test
                { view = \m -> Html.map (\_ -> ()) <| o.view (Layer LayerId.init m)
                , init =
                    \flags url ->
                        init o.init
                            (o.procedure flags url SimKey)
                }
                Dict.empty
                |> SeqTest.run sec.title
        )
        o.sections
        |> Test.describe "Scenario tests"


toMarkup :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Result InvalidMarkupReason Markup.Section
toMarkup o =
    List.foldl
        (\a acc ->
            Result.map2 (++) acc a
        )
        (Ok [])
        (List.map toMarkupSection o.sections)
        |> Result.map
            (\children ->
                Markup.Section
                    { title = o.title
                    , titleMixin = Mixin.none
                    , body = []
                    , bodyMixin = Mixin.none
                    , children = children
                    }
            )


toMarkupSection : Section flags c m e -> Result InvalidMarkupReason (List Markup.Section)
toMarkupSection (Section sec) =
    let
        (Scenario scenario) =
            sec.content

        initMarkupContext : MarkupContext
        initMarkupContext items =
            Markup.Section
                { title = sec.title
                , titleMixin = Mixin.none
                , body =
                    [ ( Mixin.none, Markup.ListItems Mixin.none items )
                    ]
                , bodyMixin = Mixin.none
                , children = []
                }
    in
    case scenario.markup initMarkupContext of
        OnGoingMarkup context ->
            Ok [ context [] ]

        MarkupAfterCases secs ->
            Ok secs

        InvalidMarkup reason ->
            Err reason


type Section flags command memory event
    = Section
        { title : String
        , content : Scenario flags command memory event
        }


section : String -> List (Scenario flags c m e) -> Section flags c m e
section title scenarios =
    Section
        { title = title
        , content = concatScenario scenarios
        }


cases : List (Section flags c m e) -> Scenario flags c m e
cases sections =
    Scenario
        { test =
            \config context ->
                SeqTest.pass ()
                    |> SeqTest.namedCases
                        (\() ->
                            List.map
                                (\(Section sec) ->
                                    let
                                        (Scenario { test }) =
                                            sec.content
                                    in
                                    ( sec.title
                                    , test config context
                                        |> SeqTest.map (\_ -> ())
                                    )
                                )
                                sections
                        )
                    |> SeqTest.map (\_ -> TestAfterCases)
        , markup =
            \context ->
                let
                    sectionMarkups =
                        List.foldl
                            (\a acc ->
                                Result.map2 (++) acc a
                            )
                            (Ok [])
                            (List.map toMarkupSection sections)
                in
                case sectionMarkups of
                    Err err ->
                        InvalidMarkup err

                    Ok markups ->
                        MarkupAfterCases <|
                            context []
                                :: markups
        }
