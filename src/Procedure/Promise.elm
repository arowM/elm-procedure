module Procedure.Promise exposing
    ( Promise
    , map
    , liftEvent, mapCmd, onLayer, Pointer
    , andRace
    , andThen
    , sync
    , sequence, syncAll
    , modify, push, listen
    , succeed
    , currentState, layerEvent
    , portRequest
    , customRequest
    , newLayer
    , layerView
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

@docs sequence, syncAll
@docs modify, push, listen


# Primitive Promises

@docs succeed
@docs currentState, layerEvent
@docs portRequest
@docs customRequest


# Layer

@docs Layer
@docs newLayer
@docs layerView

# 
-}

import Html exposing (Html)
import Internal.Core as Core
    exposing
        ( Msg(..)
        , Promise(..)
        )
import Internal.RequestId exposing (RequestId)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


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
    -> Promise c m e1 a -> Promise c m e0 a
liftEvent = Core.liftPromiseEvent


{-| Transform the Commands produced by a Procedure.

You can also use this to inject actual Commands.
-}
mapCmd : (c -> cmd) -> Promise c m e a -> Promise cmd m e a
mapCmd = Core.mapPromiseCmd


{-| Call a child Layer Promise.
-}
onLayer : Pointer m m1 -> Promise c m1 e a -> Promise c m e a
onLayer = Core.onLayer

{-| _Layer_ is a concept that deals with a part of the application. It can successfully represent elements that are created or removed during the application runtime. Especially, it matches well with Pages in SPAs. The application itself is also a Layer.
-}
type alias Layer m = Core.Layer m

{-|
-}
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
    sleep = Debug.todo ""

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

```elm
type alias Response =
    { resp1 : Resp1
    , resp2 : Resp2
    }

request1 : Promise Command Memory Event Resp1
request1 = Debug.todo ""

request2 : Promise Command Memory Event Resp2
request2 = Debug.todo ""

-- Returns `Response` value when both Promises has been completed.
batched : Promise Command Memory Event Response
batched =
    succeed Response
        (sync request1)
        (sync request2)
```


-}
sync : Promise c m e a -> Promise c m e (a -> b) -> Promise c m e b
sync =
    Core.syncPromise


-- Procedures


{-| Concatenate given sequence of Procedures.
-}
sequence : List (Promise c m e ()) -> Promise c m e ()
sequence = Core.sequence


{-| Run Procedures concurrently, and await all to be completed.
-}
syncAll : List (Promise c m e ()) -> Promise c m e ()
syncAll = Core.concurrent


{-| Construct a Promise that modifies the Memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the Memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Promise c m e ()
modify = Core.modify


{-| Lower level function to push Commands.

Consider using `portRequest` or `customRequest` if possible.
-}
push : (m -> List c) -> Promise c m e ()
push =
    Core.push



{-| Construct a Promise that start Subscription and listen to its Events till the Layer expires.

Keep in mind that this Promise blocks subsequent Promises, so it is common practice to call asynchronously with the main Promise when you create a new layer.

```elm
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
```

-}
listen :
    { name : String
    , subscription : m -> Sub e
    , handler : e -> List (Promise c m e ())
    }
    -> Promise c m e ()
listen = Core.listen


-- Primitive Promises


{-| Promise that requests current Memory state.

Note that this returns the Memory state when it is resolved:

```elm
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
request1 = Debug.todo ""

```
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
    -> m1 -> Promise c m e (Layer m1, Pointer m m1)
newLayer  = Core.newLayer

{-| -}
layerView : Layer m -> (m -> Html e) -> (String, Html (Msg e))
layerView = Core.layerView
