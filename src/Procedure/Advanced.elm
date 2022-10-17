module Procedure.Advanced exposing
    (..)

{-|

# Procedure

@docs Procedure
@docs none, concat
@docs injectCmd

# Layer

@docs Layer
@docs Pointer
@docs Channel
@docs channelString
@docs putLayer, onLayer
@docs publish

# Primitive Procedures

@docs modify, push, await, awaitMsg, async, jump, addListener, quit

# Helper procedures

@docs when
@docs unless
@docs withMemory
@docs withMaybe

# Connect to TEA app

@docs update
@docs elementView
@docs documentView
@docs subscriptions
@docs init
@docs Key
@docs Msg
@docs Model
@docs onUrlChange
@docs onUrlRequest

-}

import Browser exposing (Document)
import Html exposing (Html)
import Internal.Core as Core
import Internal.Channel as Channel
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Url exposing (Url)



-- Procedure

{-| An advanced `Procedure` enables you test for Commands.
You can start with the simpler `Procedure` module, which provides the specialized `Procedure` without `cmd` parameter.

Procedure takes three parameters:

* `cmd`: Abstraction of Command in TEA.
    Since you cannot test the `Cmd msg` value directly, you use a custom type instead.
    When running as an application, you can use `injectCmd` to convert it to the `Cmd msg` type.
* `memory`: Just like Model in TEA.
    Like Model in TEA, _Memory_ in TEPA is where the application state is managed, but in TEPA, Memory is modified in the Procedures.
* `event`: Just like Msg in TEA.
    Unlike Msg in TEA, an _Event_ in TEPA has its destination; an Event is sent to a specific Layer or specific Listener and cannot be received outside of that target.

-}
type alias Procedure cmd memory event =
    Core.Procedure cmd memory event


{-| Construct a `Procedure` instance that does nothing.
-}
none : Procedure c m e
none = Core.none


{-| Return a new Procedure that evaluates given Procedures sequentially.
-}
concat : List (Procedure c m e) -> Procedure c m e
concat = Core.concat


{-| Inject actual Commands.
-}
injectCmd : (c -> Cmd (Msg e)) -> Procedure c m e -> Procedure (Cmd (Msg e)) m e
injectCmd f =
    Core.mapCmds (List.map f >> Cmd.batch)


-- Layer


{-| Layer is a concept that deals with a part of the application, and can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m m1 = Core.Layer m m1


{-| Pointer indicates where the memory for a certain Layer `m1` should be located in the memory `m`.

Note that Pointer is permanent because it is just a indicator, while Layer may be expired because it is an instance.
For example, suppose you set the page state of the application to home page with `putLayer`. Next, you change it to the account information page, and then change it back to the home page. In this case, the same Pointer can be used for both the first and the last `putLayer`. On the other hand, the Layer obtained by the first `putLayer` has already expired and it cannot be used anymore. Even if you publish events to the Layer, nothing occurs.
-}
type alias Pointer m m1 = Core.Pointer m m1


{-| Identifier for Layers.
-}
type alias Channel = Channel.Channel


{-| Convert a Channel into a unique string.

You can use this value as a key for [`Html.Keyed`](https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed) nodes.
-}
channelString : Channel -> String
channelString = Channel.toString


{-| Put new Layer on the application.

Suppose your application have `page` field in its memory:

    type alias Memory =
        { page : Page
        }

    type Page
        = HomePage (Channel, MemoryForHomePage)
        | AccountPage (Channel, MemoryForAccountPage)

You can change pages with `putLayer` as follows:

    homePagePointer : Pointer Memory MemoryForHomePage
    homePagePointer =
        { get = \m ->
            case m.page of
                HomePage layer -> Just layer
                _ -> Nothing
        , set = \layer m ->
            { m | page = HomePage layer }
        }

    myProcedures =
        [ putLayer
            { pointer = homePagePointer
            , init = \channelForTheLayer m ->
                { m
                    | page =
                        HomePage
                            ( channelForTheLayer
                            , initMemoryForHomePage
                            )
                }
            }
            <| \homePageLayerForThisTimeOnly ->
                [ onLayer homePageLayerForThisTimeOnly
                    [ Debug.todo "operations on the home page."
                    ]
                ]
        ]

-}
putLayer :
    { pointer : Pointer m m1
    , init : Channel -> m -> m
    }
    -> (Layer m m1 -> List (Procedure c m e))
    -> Procedure c m e
putLayer = Core.putLayer


{-| Call some Layer Procedures on its parent Layer Procedures.
-}
onLayer : Layer m m1 -> List (Procedure c m1 e) -> Procedure c m e
onLayer = Core.onLayer

{-| Publish an event to the specified Layer.

You can use this function inside the View to notify Procedure that an event has occurred in the Layer specified with the given Channel.
-}
publish : Channel -> event -> Msg event
publish c e =
    Core.ChannelMsg
        { channel = c
        , event = e
        }

-- Primitive Procedures


{-| Construct a `Procedure` instance that modifies the Memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the Memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Procedure c m e
modify = Core.modify

{-| Await a Promise to be resolved, and then use the result to evaluate the subsequent Procedures.
-}
await : Promise c m e a -> (a -> List (Procedure c m e)) -> Procedure c m e
await = Core.await

{-| Construct a `Procedure` instance that evaluates the given Procedures asynchronously:

  - The subsequent sibling Procedures are evaluated immediately
  - The Procedures given as the argument are evaluated concurrently.
  - Even if the subsequent sibling Procedures is completed, the asynchronous Procedures are still alive.

    [ async
        [ Debug.todo "[Procedure A]: This is executed at the same time as [Procedure B]."
        , Debug.todo "[Procedure C]: This is not cancelled even if [Procedure B] is completed."
        ]
    , Debug.todo "[Procedure B]"
    ]

Note: When multiple `async`s are called, there is no guarantee that the Procedures generated by the first called `async` will be executed first.

-}
async : List (Procedure c m e) -> Procedure c m e
async = Core.async


{-| Ignore subsequent Procedures, and evaluate given Procedures instead.

It is convenient for following two situations.


## Make recursive Procedure

Calling itself in the Procedure will result in a compile error; the `jump` enables to build recursive Procedures.

    recursiveProcedures =
        [ await somePromise
        , jump <| \_ -> recursiveProcedures
        ]


## Safe pruning

Sometimes you may want to handle errors as follows:

    unsafePruning =
        [ await requestPosts <|
            \result ->
                case result of
                    ReceivePosts (Err error) ->
                        [ handleError error
                        ]

                    ReceivePosts (Ok posts) ->
                        [ modify <|
                            \m -> { m | posts = posts }
                        ]

                    _ ->
                        []
        , proceduresForNewPosts
        ]

It appears to be nice, but it does not work as intended. Actually, the above Procedures can evaluate the `proceduresForNewPosts` even after evaluating `handleError`. To avoid this, you can use `jump`:

    safePruning =
        [ await requestPosts <|
            \result ->
                case result of
                    ReceivePosts (Err error) ->
                        [ jump <|
                            \_ -> [ handleError error ]
                        ]

                    ReceivePosts (Ok posts) ->
                        [ modify <|
                            \m -> { m | posts = posts }
                        ]

                    _ ->
                        []
        , proceduresForNewPosts
        ]

-}
jump : (() -> List (Procedure c m e)) -> Procedure c m e
jump = Core.jump


{-| Register listener process for a Subscription.

Ensure that it is not used inside a recursive Procedures; otherwise, it will be registered duplicately so that an event will call the handler multiple times.

-}
addListener :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Procedure c m e)
    }
    -> Procedure c m e
addListener = Core.addListener


quit : Procedure c m e
quit = jump <| \_ -> []

-- Helper procedures

{-| Evaluate the given Procedures only if the first argument is `True`, otherwise same as `none`.
-}
when : Bool -> List (Procedure c m e) -> Procedure c m e
when p ps =
    if p then
        batch ps
    else
        none

{-| Evaluate the given Procedures only if the first argument is `False`, otherwise same as `none`.
-}
unless : Bool -> List (Procedure c m e) -> Procedure c m e
unless p =
    when (not p)


{-| Determine next Procedures with current Memory state.
-}
withMemory : (m -> List (Procedure c m e)) -> Procedure c m e
withMemory =
    Advanced.withMemory

{-| Evaluate the Procedures returned by the callback function only if the first argument is `Just`, otherwise same as `none`.
-}
withMaybe : Maybe a -> (a -> List (Procedure c m e)) -> Procedure c m e
withMaybe ma f =
    case ma of
        Nothing ->
            none
        Just a ->
            batch <| f a


-- Connect to TEA app

{-| TEA update function implementation for running your Procedures.
-}
update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update = Core.update


{-| Construct the TEA element view function.
-}
elementView : ((Channel, memory) -> Html (Msg event)) -> Model cmd memory event -> Html (Msg event)
elementView f model =
    let
        memory =
            case model of
                OnGoing {context} ->
                    context.state
                EndOfProcess { lastState} ->
                    lastState
    in
    f (Channel.init, memory)


{-| Just like `Procedure.documentView`.
-}
documentView : ((Channel, memory) -> Document (Msg event)) -> Model cmd memory event -> Document (Msg event)
documentView f model =
    let
        memory =
            case model of
                OnGoing {context} ->
                    context.state
                EndOfProcess { lastState} ->
                    lastState
    in
    f (Channel.init, memory)


{-| TEA subscriptions function implementation for running your Procedures.
-}
subscriptions : Model cmd memory event -> Sub (Msg event)
subscriptions : model =
    case model of
        EndOfProcess _ ->
            Sub.none
        OnGoing { context } ->
            context.listeners
                |> List.map .sub

{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    -> List (Procedure cmd memory event)
    -> ( Model cmd memory event, List cmd )
init = Core.init


{-| `Browser.Nav.Key` alternative.
-}
type alias Key = Core.Key


{-| TEA Message that wraps your events.
-}
type alias Msg event = Core.Msg event

@docs mapCmd
@docs Model
@docs onUrlChange
@docs onUrlRequest
TODO
TODO
TODO
TODO
TODO
TODO
TODO
TODO
TODO
TODO
TODO
TODO
TODO
TODO









-- Channel


{-| Same as `Procedure.Channel`.
-}
type alias Channel =
    Channel.Channel


{-| Same as `Procedure.publish`.
-}
publish : Channel -> e -> Msg e
publish =
    Msg Nothing



-- Procedure


{-| An advanced `Procedure` enables you dependency injection, which is especially useful for testing the application behaviour against sample event sequences.

I recommend starting with the simpler `Procedure` module, which provides the specialized `Procedure` type for running as an application.

-}
type Procedure cmd memory event
    = Procedure (List (Channel -> ProcedureItem cmd memory event))


type ProcedureItem cmd memory event
    = Do (memory -> ( memory, List cmd ))
    | Await (Msg event -> memory -> Maybe (List (ProcedureItem cmd memory event)))
      -- Run concurrently in new thread, killed when the parent thread is killed.
      -- The parent thread keep alive if the new thread alives.
    | Async (List (ProcedureItem cmd memory event))
    | Protected (Channel -> List (ProcedureItem cmd memory event))
    | WithPortId (PortId -> List (ProcedureItem cmd memory event))
    | Sync (List (List (ProcedureItem cmd memory event)))
    | Race (List (List (ProcedureItem cmd memory event)))
    | WithMemory (memory -> List (ProcedureItem cmd memory event))
      -- Ignore subsequent `Procedure`s and run given `Procedure`s in current thread.
    | Jump (memory -> List (ProcedureItem cmd memory event))
    | Subscribe (Sub (Msg event)) (SubId -> List (ProcedureItem cmd memory event))
    | Unsubscribe SubId
    | SubscribeOnce (Sub (Msg event))
    | Quit


{-| Construct a `Procedure` instance that does nothing and is just skipped.
-}
none : Procedure c m e
none =
    Procedure []


{-| Batch `Procedure`s together. The elements are evaluated in order.
-}
batch : List (Procedure c m e) -> Procedure c m e
batch procs =
    List.concatMap (\(Procedure fs) -> fs) procs
        |> Procedure


{-| Just like `Procedure.wrapEvent`.
-}
wrapEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Procedure c m e1
    -> Procedure c m e0
wrapEvent wrapper (Procedure fs) =
    List.map (\f c -> wrapEvent_ wrapper (f c)) fs
        |> Procedure


wrapEvent_ :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> ProcedureItem c m e1
    -> ProcedureItem c m e0
wrapEvent_ wrapper item =
    case item of
        Do g ->
            Do g

        Await g ->
            Await <|
                \msg m ->
                    case msg of
                        NoOp ->
                            Nothing

                        Msg msid c e0 ->
                            wrapper.unwrap e0
                                |> Maybe.andThen
                                    (\e1 ->
                                        g (Msg msid c e1) m
                                            |> Maybe.map
                                                (List.map (wrapEvent_ wrapper))
                                    )

        Async ps ->
            Async <|
                List.map (wrapEvent_ wrapper) ps

        Protected g ->
            Protected <|
                \priv ->
                    List.map (wrapEvent_ wrapper) (g priv)

        WithPortId g ->
            WithPortId <|
                \priv ->
                    List.map (wrapEvent_ wrapper) (g priv)

        Sync pss ->
            Sync <|
                List.map
                    (List.map (wrapEvent_ wrapper))
                    pss

        Race pss ->
            Race <|
                List.map
                    (List.map (wrapEvent_ wrapper))
                    pss

        WithMemory g ->
            WithMemory <|
                \m ->
                    g m
                        |> List.map (wrapEvent_ wrapper)

        Jump g ->
            Jump <|
                \m ->
                    g m
                        |> List.map (wrapEvent_ wrapper)

        Subscribe sub g ->
            Subscribe (Sub.map (mapMsg wrapper.wrap) sub) <|
                \sid ->
                    g sid
                        |> List.map (wrapEvent_ wrapper)

        Unsubscribe sid ->
            Unsubscribe sid

        SubscribeOnce sub ->
            SubscribeOnce (Sub.map (mapMsg wrapper.wrap) sub)

        Quit ->
            Quit


{-| Transform the command produced by an `Procedure`.
-}
mapCmd : (c1 -> c0) -> Procedure c1 m e -> Procedure c0 m e
mapCmd f (Procedure ps) =
    List.map (\p c -> mapCmd_ f (p c)) ps
        |> Procedure


{-| `mapCmd` for lists.
-}
mapCmds : (c1 -> c0) -> List (Procedure c1 m e) -> List (Procedure c0 m e)
mapCmds =
    List.map << mapCmd


mapCmd_ :
    (c1 -> c0)
    -> ProcedureItem c1 m e
    -> ProcedureItem c0 m e
mapCmd_ f item =
    case item of
        Do g ->
            Do <|
                \m ->
                    g m
                        |> Tuple.mapSecond (List.map f)

        Await g ->
            Await <|
                \msg m ->
                    g msg m
                        |> Maybe.map
                            (List.map (mapCmd_ f))

        Async ps ->
            Async <|
                List.map (mapCmd_ f) ps

        Protected g ->
            Protected <|
                \priv ->
                    List.map (mapCmd_ f) (g priv)

        WithPortId g ->
            WithPortId <|
                \priv ->
                    List.map (mapCmd_ f) (g priv)

        Sync pss ->
            Sync <|
                List.map
                    (List.map (mapCmd_ f))
                    pss

        Race pss ->
            Race <|
                List.map
                    (List.map (mapCmd_ f))
                    pss

        WithMemory g ->
            WithMemory <|
                \m ->
                    g m
                        |> List.map (mapCmd_ f)

        Jump g ->
            Jump <|
                \m ->
                    g m
                        |> List.map (mapCmd_ f)

        Subscribe sub g ->
            Subscribe sub <|
                \sid ->
                    g sid
                        |> List.map (mapCmd_ f)

        Unsubscribe sid ->
            Unsubscribe sid

        SubscribeOnce sub ->
            SubscribeOnce sub

        Quit ->
            Quit


{-| Just like `Procedure.liftMemory`.
-}
liftMemory :
    Pointer m0 m1
    -> Procedure c m1 e
    -> Procedure c m0 e
liftMemory pointer (Procedure fs) =
    List.map (\f c -> liftMemory_ pointer (f c)) fs
        |> Procedure


liftMemory_ :
    Pointer m0 m1
    -> ProcedureItem c m1 e
    -> ProcedureItem c m0 e
liftMemory_ pointer item =
    case item of
        Do g ->
            Do <|
                \m0 ->
                    case pointer.get m0 of
                        Nothing ->
                            ( m0, [] )

                        Just m1 ->
                            let
                                ( newM1, cmds ) =
                                    g m1
                            in
                            ( pointer.modify (\_ -> newM1) m0, cmds )

        Await g ->
            Await <|
                \msg m0 ->
                    pointer.get m0
                        |> Maybe.andThen
                            (\m1 ->
                                g msg m1
                                    |> Maybe.map
                                        (List.map (liftMemory_ pointer))
                            )

        Async ps ->
            Async
                (ps
                    |> List.map (liftMemory_ pointer)
                )

        Protected g ->
            Protected <|
                \c ->
                    g c
                        |> List.map (liftMemory_ pointer)

        WithPortId g ->
            WithPortId <|
                \pid ->
                    g pid
                        |> List.map (liftMemory_ pointer)

        Sync pss ->
            Sync <|
                List.map
                    (List.map (liftMemory_ pointer))
                    pss

        Race pss ->
            Race <|
                List.map
                    (List.map (liftMemory_ pointer))
                    pss

        WithMemory g ->
            WithMemory <|
                \m0 ->
                    case pointer.get m0 of
                        Nothing ->
                            []

                        Just m1 ->
                            g m1
                                |> List.map (liftMemory_ pointer)

        Jump g ->
            Jump <|
                \m0 ->
                    case pointer.get m0 of
                        Nothing ->
                            []

                        Just m1 ->
                            g m1
                                |> List.map (liftMemory_ pointer)

        Subscribe sub g ->
            Subscribe sub <|
                \sid ->
                    g sid
                        |> List.map (liftMemory_ pointer)

        Unsubscribe sid ->
            Unsubscribe sid

        SubscribeOnce sub ->
            SubscribeOnce sub

        Quit ->
            Quit


{-| Just like `Procedure.modify`.
-}
modify : (m -> m) -> Procedure c m e1
modify f =
    Procedure
        [ \_ -> Do <| \m0 -> ( f m0, [] )
        ]


{-| Just like `Procedure.push`.
-}
push : (m -> (e -> Msg e) -> cmd) -> Procedure cmd m e
push f =
    Procedure
        [ \c -> Do <| \m -> ( m, [ f m (publish c) ] )
        ]


{-| Just like `Procedure.await`.
-}
await : (e -> m -> List (Procedure c m e)) -> Procedure c m e
await f =
    Procedure
        [ \expected ->
            Await
                (\msg m ->
                    case msg of
                        NoOp ->
                            Nothing

                        Msg _ targetId e ->
                            if targetId == expected then
                                case f e m of
                                    [] ->
                                        Nothing

                                    ps ->
                                        let
                                            (Procedure items) =
                                                batch ps
                                        in
                                        Just <| List.map (\g -> g expected) items

                            else
                                Nothing
                )
        ]


{-| Just like `Procedure.async`.
-}
async : List (Procedure c m e) -> Procedure c m e
async ps =
    let
        (Procedure items) =
            batch ps
    in
    Procedure
        [ \c ->
            Async <|
                List.map (\g -> g c) items
        ]


{-| Same as `Procedure.Pointer`.
-}
type alias Pointer m m1 =
    { get : m -> Maybe m1
    , modify : (m1 -> m1) -> m -> m
    }


{-| Just like `Procedure.asyncOn`.
-}
asyncOn :
    { get : m -> Maybe ( Channel, m1 )
    , set : m1 -> m -> m
    }
    -> Channel
    -> (Pointer m m1 -> List (Procedure c m e))
    -> Procedure c m e
asyncOn o c f =
    let
        pointer =
            { get = \m ->
                o.get m
                    |> Maybe.andThen
                        (\(c_, m1) ->
                            if c /= c_ then
                                Nothing
                            else
                                Just m1
                        )
            , modify = \g m ->
                o.get m
                    |> Maybe.map
                        (\(c_, m1) ->
                            if c /= c_ then
                                m
                            else
                                o.set (g m1) m
                        )
                    |> Maybe.withDefault m
            }

        (Procedure items) =
            batch (f pointer)

    in
    Procedure
        [ \_ ->
            Async <| List.map (\g -> g c) items
        ]


{-| Just like `Procedure.protected`.
-}
protected :
    List (Procedure c m e)
    -> Procedure c m e
protected ps =
    Procedure
        [ \_ ->
            Protected <|
                \priv ->
                    let
                        (Procedure items) =
                            batch ps
                    in
                    List.map (\g -> g priv) items
        ]


{-| Just like `Procedure.sync`.
-}
sync : List (Procedure c m e) -> Procedure c m e
sync ps =
    Procedure
        [ \c ->
            Sync <|
                List.map (\(Procedure items) -> List.map (\g -> g c) items) ps
        ]


{-| Just like `Procedure.race`.
-}
race : List (Procedure c m e) -> Procedure c m e
race ps =
    Procedure
        [ \c ->
            Race <|
                List.map (\(Procedure items) -> List.map (\g -> g c) items) ps
        ]


{-| Just like `Procedure.quit`.
-}
quit : Procedure c m e
quit =
    Procedure [ \_ -> Quit ]


{-| Just like `Procedure.jump`.
-}
jump :
    (() -> List (Procedure c m e))
    -> Procedure c m e
jump f =
    Procedure
        [ \c ->
            Jump <|
                \_ ->
                    let
                        (Procedure items) =
                            batch (f ())
                    in
                    List.map (\g -> g c) items
        ]


{-| Just like `Procedure.subscribe`.
-}
subscribe :
    Sub e
    -> List (Procedure c m e)
    -> Procedure c m e
subscribe sub ps =
    Procedure
        [ \c ->
            Subscribe (Sub.map (publish c) sub) <|
                \sid ->
                    let
                        (Procedure items) =
                            batch ps
                    in
                    List.map (\g -> g c) items ++ [ Unsubscribe sid ]
        ]


{-| Just like `Procedure.subscribeOnce`.
-}
subscribeOnce : Sub e -> Procedure c m e
subscribeOnce sub =
    subscribeOnce_ (\c -> Sub.map (publish c) sub)


subscribeOnce_ : (Channel -> Sub (Msg e)) -> Procedure c m e
subscribeOnce_ mkSub =
    Procedure
        [ \c -> SubscribeOnce (mkSub c)
        ]


{-| Just like `Procedure.withMemory`.
-}
withMemory : (m -> List (Procedure c m e)) -> Procedure c m e
withMemory f =
    Procedure
        [ \c ->
            WithMemory <|
                \m ->
                    let
                        (Procedure items) = batch (f m)
                    in
                    List.map (\g -> g c) items
        ]


{-| Just like `Procedure.when`.
-}
when : Bool -> List (Procedure c m e) -> Procedure c m e
when p ls =
    if p then
        batch ls

    else
        none


{-| Just like `Procedure.unless`.
-}
unless : Bool -> List (Procedure c m e) -> Procedure c m e
unless p =
    when (not p)


{-| Just like `Procedure.withMaybe`.
-}
withMaybe : Maybe a -> (a -> List (Procedure c m e)) -> Procedure c m e
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            f a
                |> batch



-- Local procedures


{-| Just like `Procedure.portRequest`.
-}
portRequest :
    { requestPort : Value -> cmd
    , requestBody : m -> Value
    , responsePort : (Value -> Msg e) -> Sub (Msg e)
    , responseBody : Decoder a
    }
    -> (Result JD.Error a -> e)
    -> Procedure cmd m e
portRequest conf toEvent =
    withPortId <|
        \pid ->
            [ push <|
                \m _ ->
                    conf.requestPort <|
                        JE.object
                            [ ( "id", PortId.toValue pid )
                            , ( "body", conf.requestBody m )
                            ]
            , subscribeOnce_
                (\c ->
                    conf.responsePort
                        (\v ->
                            case JD.decodeValue (responseDecoder conf.responseBody) v of
                                Err err ->
                                    toEvent (Err err)
                                        |> publish c

                                Ok ( pid_, body ) ->
                                    if pid == pid_ then
                                        toEvent (Ok body)
                                            |> publish c

                                    else
                                        NoOp
                        )
                )
            ]


withPortId :
    (PortId -> List (Procedure c m e))
    -> Procedure c m e
withPortId f =
    Procedure
        [ \c ->
            WithPortId <|
                \pid ->
                    let
                        (Procedure items) =
                            batch (f pid)
                    in
                    List.map (\g -> g c) items
        ]


responseDecoder : Decoder a -> Decoder ( PortId, a )
responseDecoder decoder =
    JD.map2 (\id body -> ( id, body ))
        (JD.field "id" PortId.decoder)
        (JD.field "body" decoder)



-- Observing


{-| Just like `Procedure.observe`.
-}
observe :
    r
    -> (( Channel, r ) -> List (Procedure c m e))
    -> Procedure c m e
observe r f =
    Procedure
        [ \_ ->
            Protected <|
                \priv ->
                    let
                        (Procedure ps) =
                            f ( priv, r ) |> batch
                    in
                    List.map (\g -> g priv) ps
        ]


{-| Just like `Procedure.observeList`.
-}
observeList :
    List r
    -> (List ( Channel, r ) -> List (Procedure c m e))
    -> Procedure c m e
observeList rs f =
    List.foldr
        (\r acc ps ->
            [ observe r (\p -> acc (p :: ps))
            ]
        )
        f
        rs
        []
        |> batch


{-| Just like `Procedure.Model`.
-}
type alias Model cmd memory event = Core.Model cmd memory event


type alias Thread cmd memory event =
    Model cmd memory event


{-| Just like `Procedure.elementView`.
-}
elementView : ((Channel, memory) -> Html (Msg event)) -> Model cmd memory event -> Html (Msg event)
elementView f (Thread { newState }) =
    f (Channel.init, newState.memory)


{-| Just like `Procedure.documentView`.
-}
documentView : ((Channel, memory) -> Document (Msg event)) -> Model cmd memory event -> Document (Msg event)
documentView f (Thread { newState }) =
    f (Channel.init, newState.memory)


{-| Just like `Procedure.update`.
-}
update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg (Thread t) =
    case msg of
        NoOp ->
            ( Thread t, [] )

        Msg msid _ _ ->
            let
                (Thread t2) =
                    case msid of
                        Just sid ->
                            t.next msg
                                { newState
                                    | subs =
                                        List.filter
                                            (\( sid_, _ ) -> sid_ /= sid)
                                            newState.subs
                                }

                        Nothing ->
                            t.next msg newState

                newState =
                    t.newState
            in
            ( Thread t2, t2.cmds )


{-| Just like `Procedure.subsctiptions`.
-}
subscriptions : Model cmd memory event -> Sub (Msg event)
subscriptions (Thread t) =
    t.newState.subs
        |> List.map Tuple.second
        |> Sub.batch


{-| Just like `Procedure.init`.
-}
init :
    memory
    -> List (Procedure cmd memory event)
    -> ( Model cmd memory event, List cmd )
init initialMemory procs =
    let
        (Procedure items) =
            batch procs

        (Thread t) =
            toThread <|
                fromProcedure
                    { memory = initialMemory
                    , memoryChannel = Channel.inc
                    , nextChannel = Channel.inc Channel.init
                    , subs = []
                    , nextSubId = SubId.init
                    , nextPortId = PortId.init
                    }
                    (List.map (\f -> f Channel.init) items)
    in
    ( Thread t, t.cmds )


{-| Just like `Procedure.onUrlChange`.
-}
onUrlChange : (Url -> event) -> Url -> (Msg event)
onUrlChange f =
    f >> publish Channel.init


{-| Just like `Procedure.onUrlRequest`.
-}
onUrlRequest : (Browser.UrlRequest -> event) -> Browser.UrlRequest -> (Msg event)
onUrlRequest f =
    f >> publish Channel.init



{-| Intermediate type, which helps to handle operations that affects ancestor threads.
-}
type FromProcedure cmd memory event
    = FromProcedure
        { newState : ThreadState memory event
        , cmds : List cmd
        , next :
            Maybe
                { procedure : Msg event -> ThreadState memory event -> FromProcedure cmd memory event
                , onKilled : memory -> ( memory, List cmd )
                }
        }


toThread : FromProcedure cmd memory event -> Thread cmd memory event
toThread (FromProcedure fp) =
    case fp.next of
        Nothing ->
            Thread
                { newState = fp.newState
                , cmds = fp.cmds
                , next = endOfThread
                }

        Just next ->
            Thread
                { newState = fp.newState
                , cmds = fp.cmds
                , next = \msg s -> toThread (next.procedure msg s)
                }


endOfThread : Msg event -> ThreadState memory event -> Thread cmd memory event
endOfThread _ state =
    Thread
        { newState = state
        , cmds = []
        , next = endOfThread
        }


fromProcedure : ThreadState memory event -> List (ProcedureItem cmd memory event) -> FromProcedure cmd memory event
fromProcedure state procs =
    case procs of
        [] ->
            FromProcedure
                { newState = state
                , cmds = []
                , next = Nothing
                }

        (Do f) :: ps ->
            let
                ( memory1, cmds1 ) =
                    f state.memory

                state1 =
                    { state | memory = memory1 }

                (FromProcedure fp1) =
                    fromProcedure state1 ps
            in
            FromProcedure { fp1 | cmds = cmds1 ++ fp1.cmds }

        (Await f) :: ps2 ->
            FromProcedure
                { newState = state
                , cmds = []
                , next =
                    Just
                        { procedure =
                            \msg s ->
                                case f msg s.memory of
                                    Nothing ->
                                        fromProcedure s procs

                                    Just ps1 ->
                                        fromProcedure s (ps1 ++ ps2)
                        , onKilled = \m -> ( m, [] )
                        }
                }

        (Async ps1) :: ps2 ->
            fromProcedure state ps1
                |> andAsync (\s -> fromProcedure s ps2)

        (Protected g) :: ps2 ->
            let
                nextChannel =
                    Channel.inc state.nextChannel

                ps1 =
                    g nextChannel

                state1 =
                    { state | nextChannel = nextChannel }
            in
            fromProcedure state1 (ps1 ++ ps2)

        (WithPortId g) :: ps2 ->
            let
                nextPortId =
                    PortId.inc state.nextPortId

                ps1 =
                    g nextPortId

                state1 =
                    { state | nextPortId = nextPortId }
            in
            fromProcedure state1 (ps1 ++ ps2)

        (Sync ps) :: ps2 ->
            fromProcDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

        (Race ps) :: ps2 ->
            fromProcRaceDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

        (WithMemory f) :: ps2 ->
            fromProcedure state (f state.memory ++ ps2)

        (Jump f) :: _ ->
            fromProcedure state (f state.memory)

        (Subscribe sub g) :: ps2 ->
            let
                thisSubId =
                    state.nextSubId

                nextSubId =
                    SubId.inc state.nextSubId

                ps1 =
                    g nextSubId

                state1 =
                    { state
                        | nextSubId = nextSubId
                        , subs = ( thisSubId, sub ) :: state.subs
                    }
            in
            fromProcedure state1 (ps1 ++ ps2)

        (SubscribeOnce sub) :: ps2 ->
            let
                thisSubId =
                    state.nextSubId

                nextSubId =
                    SubId.inc state.nextSubId

                sub2 =
                    Sub.map (setMsgSubId thisSubId) sub

                state1 =
                    { state
                        | nextSubId = nextSubId
                        , subs = ( thisSubId, sub2 ) :: state.subs
                    }
            in
            fromProcedure state1 ps2

        (Unsubscribe sid) :: ps2 ->
            let
                state1 =
                    { state
                        | subs =
                            List.filter
                                (\( sid_, _ ) -> sid_ /= sid)
                                state.subs
                    }
            in
            fromProcedure state1 ps2

        Quit :: _ ->
            endOfProcedure state


endOfProcedure : ThreadState memory event -> FromProcedure cmd memory event
endOfProcedure s =
    FromProcedure
        { newState = s
        , cmds = []
        , next = Nothing
        }



{-
   applyFinally : (memory -> ( memory, List cmd )) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
   applyFinally f (FromProcedure fp) =
       case fp.next of
           Nothing ->
               let
                   state1 =
                       fp.newState

                   ( memory1, cmds1 ) =
                       f state1.memory

                   state2 =
                       { state1 | memory = memory1 }
               in
               FromProcedure
                   { newState = state2
                   , cmds = fp.cmds ++ cmds1
                   , next = Nothing
                   }

           Just next ->
               FromProcedure
                   { fp
                       | next =
                           Just
                               { procedure =
                                   \msg s ->
                                       next.procedure msg s
                                           |> applyFinally f
                               , onKilled = mergeUpdates f next.onKilled
                               }
                   }
-}


mergeUpdates : (memory -> ( memory, List cmd )) -> (memory -> ( memory, List cmd )) -> memory -> ( memory, List cmd )
mergeUpdates f g memory =
    let
        ( memory1, cmds1 ) =
            f memory

        ( memory2, cmds2 ) =
            g memory1
    in
    ( memory2, cmds1 ++ cmds2 )


{-| Run a function after the given `FromProcedure` ends.
-}
andThen : (ThreadState memory event -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andThen f (FromProcedure fp) =
    case fp.next of
        Nothing ->
            let
                (FromProcedure fp2) =
                    f fp.newState
            in
            FromProcedure
                { fp2 | cmds = fp.cmds ++ fp2.cmds }

        Just next ->
            FromProcedure
                { fp
                    | next =
                        Just
                            { procedure =
                                \msg s ->
                                    next.procedure msg s
                                        |> andThen f
                            , onKilled = next.onKilled
                            }
                }


andAsync : (ThreadState memory event -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andAsync f (FromProcedure fp1) =
    let
        (FromProcedure fp2) =
            f fp1.newState
    in
    case ( fp1.next, fp2.next ) of
        ( Nothing, _ ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next = fp2.next
                }

        ( _, Nothing ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next = fp1.next
                }

        ( Just next1, Just next2 ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next =
                    Just
                        { procedure =
                            \msg s ->
                                next1.procedure msg s
                                    |> andAsync (next2.procedure msg)
                        , onKilled =
                            mergeUpdates next1.onKilled next2.onKilled
                        }
                }


{-| Merge dependent procedures for `Sync` into one procedure.
-}
fromProcDeps : ThreadState memory event -> List (List (ProcedureItem cmd memory event)) -> FromProcedure cmd memory event
fromProcDeps state ps =
    List.foldl
        (\p acc ->
            acc
                |> andNextDep (\s -> fromProcedure s p)
        )
        (FromProcedure
            { newState = state
            , cmds = []
            , next = Nothing
            }
        )
        ps


andNextDep : (ThreadState memory event -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andNextDep =
    andAsync


{-| Merge dependent procedures for `Race` into one procedure.
If given empty list, it skips to next `Procedure`.
-}
fromProcRaceDeps : ThreadState memory event -> List (List (ProcedureItem cmd memory event)) -> FromProcedure cmd memory event
fromProcRaceDeps state ps =
    case ps of
        [] ->
            FromProcedure
                { newState = state
                , cmds = []
                , next = Nothing
                }

        q :: qs ->
            List.foldl
                (\p acc ->
                    acc
                        |> andNextRaceDep (\s -> fromProcedure s p)
                )
                (fromProcedure state q)
                qs


andNextRaceDep : (ThreadState memory event -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andNextRaceDep f (FromProcedure fp1) =
    let
        (FromProcedure fp2) =
            f fp1.newState
    in
    case ( fp1.next, fp2.next ) of
        ( Nothing, Nothing ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next = Nothing
                }

        ( Nothing, Just next2 ) ->
            let
                newState2 =
                    fp2.newState

                ( memory3, cmds3 ) =
                    next2.onKilled newState2.memory

                newState3 =
                    { newState2 | memory = memory3 }
            in
            FromProcedure
                { newState = newState3
                , cmds = fp1.cmds ++ fp2.cmds ++ cmds3
                , next = Nothing
                }

        ( Just next1, Nothing ) ->
            let
                newState2 =
                    fp2.newState

                ( memory3, cmds3 ) =
                    next1.onKilled newState2.memory

                newState3 =
                    { newState2 | memory = memory3 }
            in
            FromProcedure
                { newState = newState3
                , cmds = fp1.cmds ++ fp2.cmds ++ cmds3
                , next = Nothing
                }

        ( Just next1, Just next2 ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next =
                    Just
                        { procedure =
                            \msg s ->
                                next1.procedure msg s
                                    |> andNextRaceDep (next2.procedure msg)
                        , onKilled =
                            mergeUpdates next1.onKilled next2.onKilled
                        }
                }


{-| Same as `Procedure.Msg`.
-}
type alias Msg event = Core.Msg event


setMsgSubId : SubId -> Msg event -> Msg event
setMsgSubId sid msg =
    case msg of
        NoOp ->
            NoOp

        Msg _ c e ->
            Msg (Just sid) c e


{-| Same as `Procedure.mapMsg`.
-}
mapMsg : (a -> b) -> Msg a -> Msg b
mapMsg f msg =
    case msg of
        NoOp ->
            NoOp

        Msg msid c a ->
            Msg msid c (f a)
