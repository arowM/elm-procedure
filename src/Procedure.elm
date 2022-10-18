module Procedure exposing
    ( Procedure
    , none, concat
    , Layer
    , Pointer
    , Channel
    , channelString
    , putLayer, onLayer
    , publish
    , modify, await, async, jump, addListener, quit
    , when
    , unless
    , withMemory
    , withMaybe
    , injectCmd
    , element
    , document
    , application
    , Program
    , Document
    , update
    , elementView
    , documentView
    , subscriptions
    , init
    , Key, realKey
    , Msg
    , Model
    , onUrlChange
    , onUrlRequest
    )

{-|


# Procedure

@docs Procedure
@docs none, concat


# Layer

@docs Layer
@docs Pointer
@docs Channel
@docs channelString
@docs putLayer, onLayer
@docs publish


# Primitive Procedures

@docs modify, await, async, jump, addListener, quit


# Helper procedures

@docs when
@docs unless
@docs withMemory
@docs withMaybe


# Browser alternatives

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#connect-to-tea-app) is also available for more advanced use cases, which enables you to introduce TEPA partially into your existing TEA app.

@docs injectCmd
@docs element
@docs document
@docs application
@docs Program
@docs Document


# Connect to TEA app

@docs update
@docs elementView
@docs documentView
@docs subscriptions
@docs init
@docs Key, realKey
@docs Msg
@docs Model
@docs onUrlChange
@docs onUrlRequest

-}

import Browser exposing (Document)
import Browser.Navigation
import Html exposing (Html)
import Internal.Channel as Channel
import Internal.Core as Core
    exposing
        ( Model(..)
        )
import Url exposing (Url)



-- Procedure


{-| An advanced `Procedure` enables you test for Commands.
You can start with the simpler `Procedure` module, which provides the specialized `Procedure` without `cmd` parameter.

Procedure takes three parameters:

  - `cmd`: Abstraction of Command in TEA.
    Since you cannot test the `Cmd msg` value directly, you use a custom type instead.
    When running as an application, you can use `injectCmd` to convert it to the `Cmd msg` type.
  - `memory`: Just like Model in TEA.
    Like Model in TEA, _Memory_ in TEPA is where the application state is managed, but in TEPA, Memory is modified in the Procedures.
  - `event`: Just like Msg in TEA.
    Unlike Msg in TEA, an _Event_ in TEPA has its destination; an Event is sent to a specific Layer or specific Listener and cannot be received outside of that target.

-}
type alias Procedure cmd memory event =
    Core.Procedure cmd memory event


{-| Construct a `Procedure` instance that does nothing.
-}
none : Procedure c m e
none =
    Core.none


{-| Return a new Procedure that evaluates given Procedures sequentially.
-}
concat : List (Procedure c m e) -> Procedure c m e
concat =
    Core.concat



-- Layer


{-| Layer is a concept that deals with a part of the application, and can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m m1 =
    Core.Layer m m1


{-| Pointer indicates where the memory for a certain Layer `m1` should be located in the memory `m`.

Note that Pointer is permanent because it is just a indicator, while Layer may be expired because it is an instance.
For example, suppose you set the page state of the application to home page with `putLayer`. Next, you change it to the account information page, and then change it back to the home page. In this case, the same Pointer can be used for both the first and the last `putLayer`. On the other hand, the Layer obtained by the first `putLayer` has already expired and it cannot be used anymore. Even if you publish events to the Layer, nothing occurs.

-}
type alias Pointer m m1 =
    Core.Pointer m m1


{-| Identifier for Layers.
-}
type alias Channel =
    Channel.Channel


{-| Convert a Channel into a unique string.

You can use this value as a key for [`Html.Keyed`](https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed) nodes.

-}
channelString : Channel -> String
channelString =
    Channel.toString


{-| Put new Layer on the application.

Suppose your application have `page` field in its memory:

    type alias Memory =
        { page : Page
        }

    type Page
        = HomePage ( Channel, MemoryForHomePage )
        | AccountPage ( Channel, MemoryForAccountPage )

You can change pages with `putLayer` as follows:

    homePagePointer : Pointer Memory MemoryForHomePage
    homePagePointer =
        { get =
            \m ->
                case m.page of
                    HomePage layer ->
                        Just layer

                    _ ->
                        Nothing
        , set =
            \layer m ->
                { m | page = HomePage layer }
        }

    myProcedures =
        [ putLayer
            { pointer = homePagePointer
            , init =
                \channelForTheLayer m ->
                    { m
                        | page =
                            HomePage
                                ( channelForTheLayer
                                , initMemoryForHomePage
                                )
                    }
            }
          <|
            \homePageLayerForThisTimeOnly ->
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
putLayer =
    Core.putLayer


{-| Call some Layer Procedures on its parent Layer Procedures.
-}
onLayer : Layer m m1 -> List (Procedure c m1 e) -> Procedure c m e
onLayer =
    Core.onLayer


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
modify =
    Core.modify


type alias Promise cmd memory event a =
    Core.Promise cmd memory event a


{-| Await a Promise to be resolved, and then use the result to evaluate the subsequent Procedures.
-}
await : Promise c m e a -> (a -> List (Procedure c m e)) -> Procedure c m e
await =
    Core.await


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
async =
    Core.async


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
jump =
    Core.jump


{-| Register listener process for a Subscription.

Ensure that it is not used inside a recursive Procedures; otherwise, it will be registered duplicately so that an event will call the handler multiple times.

-}
addListener :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Procedure c m e)
    }
    -> Procedure c m e
addListener =
    Core.addListener


{-| Just ignore subsequent Procedures.
-}
quit : Procedure c m e
quit =
    jump <| \_ -> []



-- Helper procedures


{-| Evaluate the given Procedures only if the first argument is `True`, otherwise same as `none`.
-}
when : Bool -> List (Procedure c m e) -> Procedure c m e
when p ps =
    if p then
        concat ps

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
    Core.withMemory


{-| Evaluate the Procedures returned by the callback function only if the first argument is `Just`, otherwise same as `none`.
-}
withMaybe : Maybe a -> (a -> List (Procedure c m e)) -> Procedure c m e
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            concat <| f a



-- Browser alternatives


{-| Procedure version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).

You can use `injectCmd` to inject actual Cmds into your Procedure build with custom type Commands.

-}
element :
    { init : memory
    , procedures : flags -> List (Procedure (Cmd (Msg event)) memory event)
    , view : ( Channel, memory ) -> Html (Msg event)
    }
    -> Program flags memory event
element option =
    Browser.element
        { init =
            \flags ->
                init option.init (option.procedures flags)
        , view = elementView option.view
        , update = update
        , subscriptions = subscriptions
        }


{-| Procedure version of [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).

You can use `injectCmd` to inject actual Cmds into your Procedure build with custom type Commands.

-}
document :
    { init : memory
    , procedures : flags -> List (Procedure (Cmd (Msg event)) memory event)
    , view : ( Channel, memory ) -> Document (Msg event)
    }
    -> Program flags memory event
document option =
    Browser.document
        { init =
            \flags ->
                init option.init (option.procedures flags)
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions
        }


{-| Inject actual Commands.
-}
injectCmd : (c -> Cmd (Msg e)) -> Procedure c m e -> Procedure (Cmd (Msg e)) m e
injectCmd =
    Core.mapCmd


{-| Procedure version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).

The `onUrlRequest` and `onUrlChange` Events are published to the application root Layer.

-}
application :
    { init : memory
    , procedures : flags -> Url -> Key -> List (Procedure (Cmd (Msg event)) memory event)
    , view : ( Channel, memory ) -> Document (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
    }
    -> Program flags memory event
application option =
    Browser.application
        { init =
            \flags url key ->
                init option.init (option.procedures flags url (Core.RealKey key))
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest option.onUrlRequest
        , onUrlChange = onUrlChange option.onUrlChange
        }


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags memory event =
    Platform.Program flags (Model (Cmd (Msg event)) memory event) (Msg event)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document event =
    Browser.Document event



-- Connect to TEA app


{-| TEA update function implementation for running your Procedures.
-}
update : Msg event -> Model (Cmd (Msg event)) memory event -> ( Model (Cmd (Msg event)) memory event, Cmd (Msg event) )
update msg model =
    Core.update msg model
        |> Tuple.mapSecond Cmd.batch


{-| Construct the TEA element view function.
-}
elementView : (( Channel, memory ) -> Html (Msg event)) -> Model cmd memory event -> Html (Msg event)
elementView f model =
    let
        memory =
            case model of
                OnGoing { context } ->
                    context.state

                EndOfProcess { lastState } ->
                    lastState
    in
    f ( Channel.init, memory )


{-| Just like `Procedure.documentView`.
-}
documentView : (( Channel, memory ) -> Document (Msg event)) -> Model cmd memory event -> Document (Msg event)
documentView f model =
    let
        memory =
            case model of
                OnGoing { context } ->
                    context.state

                EndOfProcess { lastState } ->
                    lastState
    in
    f ( Channel.init, memory )


{-| TEA subscriptions function implementation for running your Procedures.
-}
subscriptions : Model cmd memory event -> Sub (Msg event)
subscriptions model =
    case model of
        EndOfProcess _ ->
            Sub.none

        OnGoing { context } ->
            context.listeners
                |> List.map .sub
                |> Sub.batch


{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    -> List (Procedure (Cmd (Msg event)) memory event)
    -> ( Model (Cmd (Msg event)) memory event, Cmd (Msg event) )
init memory procs =
    Core.init memory procs
        |> Tuple.mapSecond Cmd.batch


{-| `Browser.Navigation.Key` alternative.
-}
type alias Key =
    Core.Key


{-| Retrieve real `Browser.Navigation.Key` from TEPA `Key`.
It always results in `Just` when evaluating Procedure in the real application;
it results in `Nothing` when testing as scenario or generating documentation.
-}
realKey : Key -> Maybe Browser.Navigation.Key
realKey = Core.realKey

{-| TEA Message that wraps your events.
-}
type alias Msg event =
    Core.Msg event


{-| TEA Model that stores your Procedure state.
-}
type alias Model c m e =
    Core.Model c m e


{-| Construct a TEA `onUrlChange` property value.
The Event you provided as an argument can be received on the root Layer.
-}
onUrlChange : (Url -> event) -> Url -> Msg event
onUrlChange f =
    f >> publish Channel.init


{-| Construct a TEA `onUrlRequest` property value.
The Event you provided as an argument can be received on the root Layer.
-}
onUrlRequest : (Browser.UrlRequest -> event) -> Browser.UrlRequest -> Msg event
onUrlRequest f =
    f >> publish Channel.init
