module Procedure.Promise exposing
    ( Promise
    , succeed
    , map
    , andAsync
    , andRace
    , andThen
    , layerEvent
    , portRequest
    , customRequest
    , push
    )

{-|


# Core

@docs Promise
@docs succeed
@docs map


# Composition

@docs andAsync
@docs andRace
@docs andThen


# Common Promises

@docs layerEvent
@docs portRequest
@docs customRequest
@docs push

-}

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


{-| Build a Promise that is always resolved with the given value.

This is usefull for building Promise for concurrent operations with `andAsync`.

-}
succeed : a -> Promise cmd memory event a
succeed =
    Core.succeedPromise


{-| Transform a Promise.
-}
map : (a -> b) -> Promise c m e a -> Promise c m e b
map =
    Core.mapPromise


{-| Run another Promise concurrently to get both results.

The following example build a Promise that requests A and B concurrently, awaits both result, and is resolved with sum of the results.

    succeed (\a b -> a + b)
        |> andAsync requestA
        |> andAsync requestB

-}
andAsync : Promise c m e a -> Promise c m e (a -> b) -> Promise c m e b
andAsync =
    Core.andAsyncPromise


{-| Run another Promise concurrently to get the firtst result.

May you want to set timeout on your request:

    requestWithTimeout =
        myRequest
            |> map Ok
            |> andRace
                (sleep 10000
                    |> map Err
                )

-}
andRace : Promise c m e a -> Promise c m e a -> Promise c m e a
andRace =
    Core.andRacePromise


{-| Build a new Promise that evaluate two Promises sequentially.
-}
andThen : (a -> Promise c m e b) -> Promise c m e a -> Promise c m e b
andThen =
    Core.andThenPromise


{-| Await Layer events. Your callback function is called every time the Layer for Memory `m` receives an Event.
If the callback function returns `Nothing`, it awaits another Event for the Layer; otherwise the Promise is resolved your `Just` value.
-}
layerEvent : (event -> m -> Maybe a) -> Promise c m event a
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
push : (m -> List c) -> Promise c m e ()
push =
    Core.push
