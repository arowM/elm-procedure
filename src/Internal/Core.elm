module Internal.Core exposing
    ( succeedResponseHandler
    , mapResponseHandler
    , andAsyncResponseHandler
    , andRaceResponseHandler
    , liftResponseHandlerMemory
    , succeedPromise
    , mapPromise
    , andAsyncPromise
    , andRacePromise
    , liftPromiseMemory
    , none, batch
    , modify, push, await, async, addListener
    , putLayer, onLayer
    , init, update
    , HandlerResult(..), Model(..), Msg(..), Procedure(..), Promise(..), ResponseHandler(..)
    )

{-|

@docs Model
@docs Msg
@docs ResponseHandler
@docs succeedResponseHandler
@docs mapResponseHandler
@docs andAsyncResponseHandler
@docs andRaceResponseHandler
@docs liftResponseHandlerMemory
@docs HandlerResult
@docs Promise
@docs succeedPromise
@docs mapPromise
@docs andAsyncPromise
@docs andRacePromise
@docs liftPromiseMemory
@docs Procedure
@docs none, batch, liftMemory
@docs modify, push, await
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
    , subs : List ( SubId, memory -> Maybe (Sub (Msg event)) )
    , nextSubId : SubId
    , nextRequestId : RequestId
    , nextChannel : Channel
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
        , closeSub : Maybe SubId
        }
    | SubMsg
        { subId : SubId
        , event : event
        }
    | NoOp



-- ResponseHandler


type
    ResponseHandler m e a
    -- Supply `m` parameter not to resolve on expired layers
    = ResponseHandler (m -> Msg e -> HandlerResult m e a)


type HandlerResult m e a
    = Resolved a
    | AwaitAgain (ResponseHandler m e a)
    | Reject


mapResponseHandler : (a -> b) -> ResponseHandler m e a -> ResponseHandler m e b
mapResponseHandler f (ResponseHandler handler) =
    ResponseHandler <|
        \m msg ->
            case handler m msg of
                Resolved a ->
                    Resolved <| f a

                AwaitAgain next ->
                    AwaitAgain <| mapResponseHandler f next

                Reject ->
                    Reject


succeedResponseHandler : a -> ResponseHandler m e a
succeedResponseHandler a =
    ResponseHandler <| \_ _ -> Resolved a


andAsyncResponseHandler : ResponseHandler m e a -> ResponseHandler m e (a -> b) -> ResponseHandler m e b
andAsyncResponseHandler (ResponseHandler handlerA) (ResponseHandler handlerF) =
    ResponseHandler <|
        \m msg ->
            case ( handlerF m msg, handlerA m msg ) of
                ( Resolved f, Resolved a ) ->
                    Resolved <| f a

                ( Resolved f, AwaitAgain handlerA2 ) ->
                    AwaitAgain <| mapResponseHandler f handlerA2

                ( AwaitAgain handlerF2, Resolved a ) ->
                    AwaitAgain <| mapResponseHandler (\f -> f a) handlerF2

                ( AwaitAgain handlerF2, AwaitAgain handlerA2 ) ->
                    AwaitAgain <| andAsyncResponseHandler handlerA2 handlerF2

                ( Reject, _ ) ->
                    Reject

                ( _, Reject ) ->
                    Reject


andRaceResponseHandler : ResponseHandler m e a -> ResponseHandler m e a -> ResponseHandler m e a
andRaceResponseHandler (ResponseHandler handler1) (ResponseHandler handler2) =
    ResponseHandler <|
        \m msg ->
            case ( handler1 m msg, handler2 m msg ) of
                ( Resolved a1, _ ) ->
                    Resolved a1

                ( AwaitAgain _, Resolved a2 ) ->
                    Resolved a2

                ( AwaitAgain next1, AwaitAgain next2 ) ->
                    AwaitAgain <| andRaceResponseHandler next1 next2

                ( Reject, r2 ) ->
                    r2

                ( r1, Reject ) ->
                    r1


liftResponseHandlerMemory :
    (m -> Maybe m1)
    -> ResponseHandler m1 e a
    -> ResponseHandler m e a
liftResponseHandlerMemory get (ResponseHandler handler) =
    ResponseHandler <|
        \m msg ->
            case get m of
                Nothing ->
                    Reject

                Just m1 ->
                    case handler m1 msg of
                        Resolved a ->
                            Resolved a

                        AwaitAgain next ->
                            AwaitAgain <| liftResponseHandlerMemory get next

                        Reject ->
                            Reject


customResponseHandler : (Msg e -> List (Procedure c m e)) -> ResponseHandler m e (Procedure c m e)
customResponseHandler f =
            ResponseHandler <|
                \_ msg ->
                    case f msg of
                        [] ->
                            AwaitAgain <| customResponseHandler f
                        procs ->
                            Resolved <| batch procs

-- Promise


type Promise cmd m e a
    = Promise
        { cmds : m -> List (RequestId -> cmd)
        , handler : RequestId -> ResponseHandler m e a
        }


succeedPromise : a -> Promise c m e a
succeedPromise a =
    Promise
        { cmds = \_ -> []
        , handler = \_ -> succeedResponseHandler a
        }


mapPromise : (a -> b) -> Promise c m e a -> Promise c m e b
mapPromise f (Promise prom) =
    Promise
        { cmds = prom.cmds
        , handler = mapResponseHandler f prom.handler
        }


andAsyncPromise : Promise c m e a -> Promise c m e (a -> b) -> Promise c m e b
andAsyncPromise (Promise promA) (Promise promF) =
    Promise
        { cmds = \m -> promA.cmds m ++ promF.cmds m
        , handler = andAsyncResponseHandler promA.handler promF.handler
        }


andRacePromise : Promise c m e a -> Promise c m e a -> Promise c m e a
andRacePromise (Promise prom1) (Promise prom2) =
    Promise
        { cmds = \m -> prom1.cmds m ++ prom2.cmds m
        , handler = andRaceResponseHandler prom1.handler prom2.handler
        }


liftPromiseMemory :
    (m -> Maybe m1)
    -> Promise c m1 e a
    -> Promise c m e a
liftPromiseMemory get (Promise prom) =
    Promise
        { cmds = \m ->
            case get m of
                Nothing ->
                    []
                Just m1 ->
                    prom.cmds m1
        , handler = liftResponseHandlerMemory get prom.handler
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
    | Subscribe
        -- Supply `m` parameter not to assign new Sub on expired Layers.
        { subscription : (SubId, m -> Maybe (Sub (Msg e)))
        , next : Procedure c m e
        }
    | WithNewChannel
        -- Supply `m` parameter not to assign new channel on expired Layers.
        { withNewChannel : m -> Channel -> Procedure c m e
        , next : Procedure c m e
        }
    | WithNewSubId
        -- Supply `m` parameter not to assign new SubId on expired Layers.
        { withNewSubId : m -> SubId -> Procedure c m e
        , next : Procedure c m e
        }
    | WithNewRequestId
        -- Supply `m` parameter not to assign new RequestId on expired Layers.
        { withNewRequestId : m -> RequestId -> Procedure c m e
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

        Subscribe r ->
            Subscribe
                { r | next = mappend r.next p2 }

        WithNewChannel r ->
            WithNewChannel
                { r | next = mappend r.next p2 }

        WithNewSubId r ->
            WithNewSubId
                { r | next = mappend r.next p2 }

        WithNewRequestId r ->
            WithNewRequestId
                { r | next = mappend r.next p2 }

        Nil ->
            p2


liftMemory :
    { get : m -> Maybe ( Channel, m1 )
    , set : ( Channel, m1 ) -> m -> m

    -- Even if the get succeeds, it is still potential that the result is not for subject Layer.
    -- The Channel value for the subject Layer is required for releasing the Layer resources in such cases.
    , channel : Channel
    }
    -> Procedure c m1 e
    -> Procedure c m e
liftMemory o =
    liftMemory_
        { get =
            \m ->
                o.get m
                    |> Maybe.andThen
                        (\( c, m1 ) ->
                            if c == o.channel then
                                Just m1

                            else
                                Nothing
                        )
        , set = \m1 -> o.set ( o.channel, m1 )
        }


liftMemory_ :
    { get : m -> Maybe m1
    , set : m1 -> m -> m
    }
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
                        |> liftPromiseMemory o.get
                }

        Async r ->
            Async
                { async = liftMemory_ o r.async
                , next = liftMemory_ o r.next
                }

        Subscribe r ->
            Subscribe
                { subscription =
                    let
                        (subId, sub) = r.subscription
                    in
                    (subId
                    , \m ->
                        o.get m
                            |> Maybe.andThen sub
                    )
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

        WithNewSubId r ->
            WithNewSubId
                { withNewSubId =
                    \m c ->
                        case o.get m of
                            Nothing ->
                                Nil

                            Just m1 ->
                                liftMemory_ o (r.withNewSubId m1 c)
                , next = liftMemory_ o r.next
                }

        WithNewRequestId r ->
            WithNewRequestId
                { withNewRequestId =
                    \m c ->
                        case o.get m of
                            Nothing ->
                                Nil

                            Just m1 ->
                                liftMemory_ o (r.withNewRequestId m1 c)
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


awaitMsg : (Msg e -> List (Procedure c m e)) -> Procedure c m e
awaitMsg f =
    Promise
        { cmds = \_ -> []
        , handler = customResponseHandler f
        }
        |> runPromise

await : Promise c m e a -> (a -> List (Procedure c m e)) -> Procedure c m e
await prom f =
    mapPromise (f >> batch) prom
        |> runPromise


async : List (Procedure c m e) -> Procedure c m e
async procs =
    Async
        { async = batch procs
        , next = Nil
        }


subscribe : (SubId, m -> Sub (Msg e)) -> Procedure c m e
subscribe (subId, sub) =
    Subscribe
        { subscription = (subId, sub >> Just)
        , next = Nil
        }


withNewChannel : (Channel -> List (Procedure c m e)) -> Procedure c m e
withNewChannel f =
    WithNewChannel
        { withNewChannel = \_ -> f >> batch
        , next = Nil
        }

type Layer m m1 =
    Layer (Layer_ m m1)


type alias Layer_ m m1 =
    { get : m -> Maybe ( Channel, m1 )
    , set : ( Channel, m1 ) -> m -> m
    , channel : Channel
    }

putLayer :
    { get : m -> Maybe ( Channel, m1)
    , set : (Channel, m1) -> m -> m
    , init : Channel -> m -> m
    }
    -> (Layer m m1 -> List (Procedure c m e))
    -> Procedure c m e
putLayer o f =
    withNewChannel <| \c ->
        [ modify (o.init c)
        , batch <| f <|
            Layer
                { get = o.get
                , set = o.set
                , channel = c
                }
        ]

onLayer : Layer m m1 -> List (Procedure c m1 e) -> Procedure c m e
onLayer (Layer layer) procs =
    liftMemory layer <| batch procs


withNewSubId : (SubId -> List (Procedure c m e)) -> Procedure c m e
withNewSubId f =
    WithNewSubId
        { withNewSubId = \_ -> f >> batch
        , next = Nil
        }

withNewRequestId : (RequestId -> List (Procedure c m e)) -> Procedure c m e
withNewRequestId f =
    WithNewRequestId
        { withNewRequestId = \_ -> f >> batch
        , next = Nil
        }


addListener : (m -> Sub e) -> (e -> List (Procedure c m e)) -> Procedure c m e
addListener sub handler =
    withNewSubId <|
        \subId ->
            [ async
                [ let
                    subMsg e =
                        SubMsg
                            { subId = subId
                            , event = e
                            }
                  in
                  subscribe
                    ( subId
                    , sub >> Sub.map subMsg
                    )
                , let
                    awaitForever () =
                        awaitMsg <| \msg ->
                            case msg of
                                SubMsg subMsg ->
                                    if (subMsg.subId == subId) then
                                        [ async
                                            [ awaitForever ()
                                            ]
                                        , handler subMsg.event
                                            |> batch
                                        ]
                                    else
                                        []
                                _ ->
                                    []
                  in
                  awaitForever ()
                ]
            ]


portRequest :
    { request : m -> RequestId -> (e -> Msg e) -> c
    , response : (Value -> Msg e) -> Sub (Msg e)
    , responseBody : Value -> Maybe a
    , requestId : Value -> Maybe RequestId
    }
    -> Promise c m e a
portRequest o =
    -- Promise
    --     { cmds = \m ->
    --         [ \reqId ->
    --             o.request m reqId <| \e ->
    --                 ResponseMsg
    --                     { requestId = 
    --         ]
    --     , handler =
    --         \reqId ->
    --             ResponseHandler <|
    --                 \m msg ->
    --                     case msg of
    --                         ResponseMsg respMsg ->
    --                             if (respMsg.requestId == reqId) then
                                    


    Debug.todo ""


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

                (ResponseHandler handler) =
                    prom.handler

                next : Msg e -> Context m e -> ( Model c m e, List c )
                next msg nextContext =
                    case handler nextContext.state msg of
                        AwaitAgain nextHandler ->
                            toModel nextContext <|
                                RunPromise
                                    { promise =
                                        Promise
                                            { handler = nextHandler
                                            , cmds = \_ -> []
                                            }
                                    }

                        Resolved nextProc ->
                            toModel nextContext nextProc

                        Reject ->
                            ( EndOfProcess, [] )

                ( cmds, nextRequestId ) =
                    List.foldr
                        (\toCmd ( accCmds, accNextReqId ) ->
                            ( toCmd accNextReqId :: accCmds
                            , RequestId.inc accNextReqId
                            )
                        )
                        ( [], context.nextRequestId )
                        (prom.cmds context.state)
            in
            ( OnGoing
                { context = { context | nextRequestId = nextRequestId }
                , next = next
                }
            , cmds
            )

        Async r ->
            toModel context (concurrent r.async r.next)

        Subscribe r ->
            toModel
                { context
                    | subs =
                        r.subscription :: context.subs
                }
                r.next

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

        WithNewSubId r ->
            let
                newSubId =
                    context.nextSubId

                newContext =
                    { context
                        | nextSubId = SubId.inc context.nextSubId
                    }

                newProc =
                    r.withNewSubId context.state newSubId
            in
            toModel newContext (mappend newProc r.next)

        WithNewRequestId r ->
            let
                newRequestId =
                    context.nextRequestId

                newContext =
                    { context
                        | nextRequestId = RequestId.inc context.nextRequestId
                    }

                newProc =
                    r.withNewRequestId context.state newRequestId
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

                Subscribe r2 ->
                    Subscribe
                        { r2
                            | next = concurrent p1 r2.next
                        }

                WithNewChannel r2 ->
                    WithNewChannel
                        { r2
                            | next = concurrent p1 r2.next
                        }

                WithNewSubId r2 ->
                    WithNewSubId
                        { r2
                            | next = concurrent p1 r2.next
                        }

                WithNewRequestId r2 ->
                    WithNewRequestId
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

                Subscribe r1 ->
                    Subscribe
                        { r1
                            | next = concurrent r1.next p2
                        }

                WithNewChannel r1 ->
                    WithNewChannel
                        { r1
                            | next = concurrent r1.next p2
                        }

                WithNewSubId r1 ->
                    WithNewSubId
                        { r1
                            | next = concurrent r1.next p2
                        }

                WithNewRequestId r1 ->
                    WithNewRequestId
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
                oldContext =
                    onGoing.context

                context =
                    { oldContext
                        | subs =
                            List.filter
                                (\( _, f ) ->
                                    f oldContext.state /= Nothing
                                )
                                oldContext.subs
                    }
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
                                                (\( sid, _ ) ->
                                                    sid /= subId
                                                )
                                                context.subs
                                    }

                                Nothing ->
                                    context
                    in
                    onGoing.next msg newContext
