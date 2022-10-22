module Tepa exposing
    ( Promise
    , map
    , liftEvent, mapCmd, onLayer
    , Pointer
    , andRace
    , andThen
    , sync
    , none, sequence, syncAll, andThenSequence
    , modify, push, listen
    , when
    , unless
    , withMaybe
    , succeed
    , currentState, layerEvent
    , portRequest
    , customRequest
    , newLayer
    , layerView, keyedLayerView, layerDocument
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
    , Key, runNavCmd
    , Msg
    , mapMsg
    , Model
    , onUrlChange
    , onUrlRequest
    )

{-|


# Promise

@docs Promise


# Transformers

@docs map
@docs liftEvent, mapCmd, onLayer
@docs Pointer


# Composition

@docs andRace
@docs andThen
@docs sync


# Procedures

Promises that returns `()` are called as a _Procedure_.

@docs none, sequence, syncAll, andThenSequence
@docs modify, push, listen


# Helper Procedures

@docs when
@docs unless
@docs withMaybe


# Primitive Promises

@docs succeed
@docs currentState, layerEvent
@docs portRequest
@docs customRequest


# Layer

@docs Layer
@docs newLayer
@docs layerView, keyedLayerView, layerDocument


# Browser alternatives

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#connect-to-tea-app) is also available for more advanced use cases, which enables you to introduce TEPA partially into your existing TEA app.

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
@docs Key, runNavCmd
@docs Msg
@docs mapMsg
@docs Model
@docs onUrlChange
@docs onUrlRequest

-}

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Internal.Core as Core
    exposing
        ( Msg(..)
        , Promise(..)
        )
import Internal.RequestId exposing (RequestId)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Url exposing (Url)


{-| The Promise represents the eventual completion of an operation and its resulting value. Similar to [Promise in JS](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise).
-}
type alias Promise cmd memory event result =
    Core.Promise cmd memory event result


{-| Build a Promise that is always completed with the given value immediately.

This is usefull for building Promise for concurrent operations with `sync`.

-}
succeed : a -> Promise cmd memory event a
succeed =
    Core.succeedPromise


{-| Transform a resulting value produced by a Promise.
-}
map : (a -> b) -> Promise c m e a -> Promise c m e b
map =
    Core.mapPromise


{-| Transform the Events produced or consumed by a Procedure.
-}
liftEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Promise c m e1 a
    -> Promise c m e0 a
liftEvent =
    Core.liftPromiseEvent


{-| Transform the Commands produced by a Procedure.

You can also use this to inject actual Commands.

-}
mapCmd : (c -> cmd) -> Promise c m e a -> Promise cmd m e a
mapCmd =
    Core.mapPromiseCmd


{-| Call a child Layer Promise.
-}
onLayer : Pointer m m1 -> Promise c m1 e a -> Promise c m e a
onLayer =
    Core.onLayer


{-| _Layer_ is a concept that deals with a part of the application. It can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m =
    Core.Layer m


{-| -}
type alias Pointer m m1 =
    Core.Pointer m m1



-- Composition


{-| Run another Promise concurrently to get the firtst result.

May you want to set timeout on your request:

    requestWithTimeout : Promise Command Memory Event (Result () Response)
    requestWithTimeout =
        myRequest
            |> map Ok
            |> andRace
                (sleep 10000
                    |> map Err
                )

    sleep : Promise Command Memory Event ()
    sleep =
        Debug.todo ""

-}
andRace : Promise c m e a -> Promise c m e a -> Promise c m e a
andRace =
    Core.andRacePromise


{-| Build a new Promise that evaluate two Promises sequentially.
-}
andThen : (a -> Promise c m e b) -> Promise c m e a -> Promise c m e b
andThen =
    Core.andThenPromise


{-| Run another Promise concurrently to get both results.

    type alias Response =
        { resp1 : Resp1
        , resp2 : Resp2
        }

    request1 : Promise Command Memory Event Resp1
    request1 =
        Debug.todo ""

    request2 : Promise Command Memory Event Resp2
    request2 =
        Debug.todo ""

    -- Returns `Response` value when both Promises has been completed.
    batched : Promise Command Memory Event Response
    batched =
        succeed Response
            (sync request1)
            (sync request2)

-}
sync : Promise c m e a -> Promise c m e (a -> b) -> Promise c m e b
sync =
    Core.syncPromise


{-| Evaluate sequence of Procedures that depend on the result of a Promise.

    andThenSequence f =
        andThen (f >> sequence)

-}
andThenSequence : (a -> List (Promise c m e ())) -> Promise c m e a -> Promise c m e ()
andThenSequence f =
    andThen (f >> sequence)



-- Procedures


{-| Procedure that does nothing.
-}
none : Promise c m e ()
none =
    Core.none


{-| Concatenate given sequence of Procedures.
-}
sequence : List (Promise c m e ()) -> Promise c m e ()
sequence =
    Core.sequence


{-| Run Procedures concurrently, and await all to be completed.
-}
syncAll : List (Promise c m e ()) -> Promise c m e ()
syncAll =
    Core.concurrent


{-| Construct a Promise that modifies the Memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the Memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Promise c m e ()
modify =
    Core.modify


{-| Lower level function to push Commands.

Consider using `portRequest` or `customRequest` if possible.

-}
push : (m -> List c) -> Promise c m e ()
push =
    Core.push


{-| Construct a Promise that start Subscription and listen to its Events till the Layer expires.

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer.

    myProcedures : List (Promise Command Memory Event ())
    myProcedures =
        [ newLayer myLayerPosition initValue
            |> andThen
                (\(myLayer, myPointer) ->
                    syncAll
                        [ listen
                            { "tick-listener"
                            , subscription = \_ ->
                                Time.every 1000 Tick
                            , handler = onEveryTick
                            }
                        , Debug.todo "Main Promise"
                        ]
                )

    onEveryTick : Event -> List (Promise c m e ())
    onEveryTick event =
        case event of
            Tick time ->
                Debug.todo "Sequence of Promises"
            _ ->
                []

-}
listen :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Promise c m e ())
    }
    -> Promise c m e ()
listen =
    Core.listen



-- Helper Procedures


{-| Evaluate the sequence of Procedures only if the first argument is `True`, otherwise same as `none`.
-}
when : Bool -> List (Promise c m e ()) -> Promise c m e ()
when p ps =
    if p then
        sequence ps

    else
        none


{-| Evaluate the sequence of Procedures only if the first argument is `False`, otherwise same as `none`.
-}
unless : Bool -> List (Promise c m e ()) -> Promise c m e ()
unless p =
    when (not p)


{-| Evaluate the sequence of Procedures returned by the callback function only if the first argument is `Just`, otherwise same as `none`.
-}
withMaybe : Maybe a -> (a -> List (Promise c m e ())) -> Promise c m e ()
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            sequence <| f a



-- Primitive Promises


{-| Promise that requests current Memory state.

Note that this returns the Memory state when it is resolved:

    type alias Response =
        { memoryOnRequest : Memory
        , response1 : Response1
        }

    -- The `currentState` in this Promise returns the memory state when it called,
    -- i.e., when the `request1` is called, but not when the `request1` receives response.
    myPromise : Promise Command Memory Event Response
    myPromise =
        succeed Response
            (async currentState)
            (async request1)

    request1 : Promise Common Memory Event Response1
    request1 =
        Debug.todo ""

-}
currentState : Promise c m e m
currentState =
    Core.currentState


{-| Lower level Promise that awaits Layer events.

This resolves with any Event the Layer receives; for specific Layer Events, you cane use `withLayerEvent` helper function.

-}
layerEvent : Promise c m e e
layerEvent =
    Core.layerEvent


{-| Build a Promise to send one outgoing port Message and receive the corresponding incoming port Message only once.

For example, we can use `portRequest` to get localStorage value safely.

In JavaScript side:

```js
app.ports.requestGetLocalName.subscribe((req) => {
  try {
    app.ports.receiveGetLocalName.send({
      // The `requestId` value, generated by TEPA, links
      // the subscribe port to the relevant send port.
      requestId: req.requestId,
      body: {
        name: localStorage.getItem(`Name.${req.body.userId}`),
      },
    });
  } catch {
    app.ports.receiveGetLocalName.send({
      requestId: req.id,
      body: {
        name: null,
      },
    });
  }
});
```

In Elm side:

    import Json.Decode as JD
    import Json.Encode as JE exposing (Value)

    port requestGetLocalName : Value -> Cmd msg

    port receiveGetLocalName : (Value -> msg) -> Sub msg

    type alias LocalNameResponse =
        { name : Maybe String
        }

    requestLocalName : String -> Promise Command Memory Event LocalNameResponse
    requestLocalName userId =
        portRequest
            { name = "Request for localStorage value"
            , request =
                \m { requestId } ->
                    requestGetLocalName <|
                        JE.object
                            [ ( "requestId", requestId )
                            , ( "body"
                              , JE.object
                                    [ ( "userId"
                                      , JE.string userId
                                      )
                                    ]
                              )
                            ]
            , receiver = receiveGetLocalName
            , resposne =
                \requestId ->
                    JD.map2 (\rid body -> ( rid, body ))
                        (JD.field "requestId" requestId)
                        (JD.field "body"
                            (JD.map LocalNameResponse
                                (JD.field "name"
                                    (JD.nullable JD.string)
                                )
                            )
                        )
            }

-}
portRequest :
    { name : String
    , request : m -> { requestId : Value } -> c
    , receiver : (Value -> Msg e) -> Sub (Msg e)
    , response : Decoder RequestId -> Decoder ( RequestId, resp )
    }
    -> Promise c m e resp
portRequest =
    Core.portRequest


{-| -}
customRequest :
    { name : String
    , request : m -> RequestId -> (e -> Msg e) -> c
    }
    -> Promise c m e e
customRequest =
    Core.customRequest


{-| -}
newLayer :
    { get : (Layer m1 -> Maybe m1) -> m -> Maybe m1
    , modify : (Layer m1 -> Layer m1) -> m -> m
    }
    -> m1
    -> Promise c m e ( Layer m1, Pointer m m1 )
newLayer =
    Core.newLayer


{-| -}
layerView : Layer m -> (m -> Html e) -> Html (Msg e)
layerView =
    Core.layerView


{-| -}
keyedLayerView : Layer m -> (m -> Html e) -> ( String, Html (Msg e) )
keyedLayerView =
    Core.keyedLayerView


{-| -}
layerDocument : Layer m -> (m -> Document e) -> Document (Msg e)
layerDocument =
    Core.layerDocument



-- Browser alternatives


{-| Procedure version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).

You can use `mapCmd` to inject actual Cmds into your Procedure build with custom type Commands.

-}
element :
    { init : memory
    , procedure : flags -> List (Promise (Cmd (Msg event)) memory event ())
    , view : Layer memory -> Html (Msg event)
    }
    -> Program flags memory event
element option =
    Browser.element
        { init =
            \flags ->
                init option.init (option.procedure flags)
        , view = elementView option.view
        , update = update
        , subscriptions = subscriptions
        }


{-| Procedure version of [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).

You can use `mapCmd` to inject actual Cmds into your Procedure build with custom type Commands.

-}
document :
    { init : memory
    , procedure : flags -> List (Promise (Cmd (Msg event)) memory event ())
    , view : Layer memory -> Document (Msg event)
    }
    -> Program flags memory event
document option =
    Browser.document
        { init =
            \flags ->
                init option.init (option.procedure flags)
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions
        }


{-| Procedure version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).

The `onUrlRequest` and `onUrlChange` Events are published to the application root Layer.

-}
application :
    { init : memory
    , procedure : flags -> Url -> Key -> List (Promise (Cmd (Msg event)) memory event ())
    , view : Layer memory -> Document (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
    }
    -> Program flags memory event
application option =
    Browser.application
        { init =
            \flags url key ->
                init option.init (option.procedure flags url (Core.RealKey key))
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
type alias Document a =
    Browser.Document a



-- Connect to TEA app


{-| TEA update function implementation for running your Procedures.
-}
update : Msg event -> Model (Cmd (Msg event)) memory event -> ( Model (Cmd (Msg event)) memory event, Cmd (Msg event) )
update msg model =
    Core.update msg model
        |> Tuple.mapSecond Cmd.batch


{-| Construct the TEA element view function.
-}
elementView : (Layer memory -> Html (Msg event)) -> Model cmd memory event -> Html (Msg event)
elementView =
    Core.elementView


{-| Just like `Procedure.documentView`.
-}
documentView : (Layer memory -> Document (Msg event)) -> Model cmd memory event -> Document (Msg event)
documentView =
    Core.documentView


{-| TEA subscriptions function implementation for running your Procedures.
-}
subscriptions : Model cmd memory event -> Sub (Msg event)
subscriptions =
    Core.subscriptions


{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    -> List (Promise (Cmd (Msg event)) memory event ())
    -> ( Model (Cmd (Msg event)) memory event, Cmd (Msg event) )
init memory procs =
    Core.init memory procs
        |> Tuple.mapSecond Cmd.batch


{-| `Browser.Navigation.Key` alternative.
-}
type alias Key =
    Core.Key


{-| Retrieve real `Browser.Navigation.Key` from TEPA `Key`.
It always provide real `Browser.Navigation.Key` when evaluating Procedure in the real application;
it returns `Cmd.none` when testing as scenario or generating documentation.
-}
runNavCmd : (Nav.Key -> Cmd msg) -> Key -> Cmd msg
runNavCmd =
    Core.runNavCmd


{-| TEA Message that wraps your events.
-}
type alias Msg event =
    Core.Msg event


{-| -}
mapMsg : (event1 -> event0) -> Msg event1 -> Msg event0
mapMsg =
    Core.mapMsg


{-| TEA Model that stores your Procedure state.
-}
type alias Model c m e =
    Core.Model c m e


{-| Construct a TEA `onUrlChange` property value.
The Event you provided as an argument can be received on the root Layer.
-}
onUrlChange : (Url -> event) -> Url -> Msg event
onUrlChange f =
    Core.rootLayerMsg << f


{-| Construct a TEA `onUrlRequest` property value.
The Event you provided as an argument can be received on the root Layer.
-}
onUrlRequest : (Browser.UrlRequest -> event) -> Browser.UrlRequest -> Msg event
onUrlRequest f =
    Core.rootLayerMsg << f