module Procedure exposing
    ( Procedure
    , none
    , batch
    , wrapEvent
    , liftMemory
    , Pointer
    , Channel
    , publish
    , element
    , document
    , application
    , Program
    , Document
    , Msg
    , mapMsg
    , modify
    , push
    , Request
    , runRequest
    , subscribe
    , subscribeOnce
    , await
    , async
    , asyncOn
    , sync
    , race
    , quit
    , jump
    , protected
    , portRequest
    , withMemory
    , when
    , unless
    , withMaybe
    , observe
    , observeList
    , update
    , elementView
    , documentView
    , subscriptions
    , init
    , Model
    , onUrlChange
    , onUrlRequest
    )

{-|


# Procedure

@docs Procedure
@docs none
@docs batch
@docs wrapEvent
@docs liftMemory
@docs Pointer


# Channel

@docs Channel
@docs publish


# Entry point

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#low-level-api) is also available for more advanced use cases, which enables you to introduce TEPA partially into your existing TEA app.

@docs element
@docs document
@docs application
@docs Program
@docs Document
@docs Msg
@docs mapMsg


# Constructors

@docs modify
@docs push
@docs Request
@docs runRequest
@docs subscribe
@docs subscribeOnce
@docs await
@docs async
@docs asyncOn
@docs sync
@docs race
@docs quit
@docs jump
@docs protected


# Helper procedures

@docs portRequest
@docs withMemory
@docs when
@docs unless
@docs withMaybe


# Observing

@docs observe
@docs observeList


# Low level API

@docs update
@docs elementView
@docs documentView
@docs subscriptions
@docs init
@docs Model
@docs onUrlChange
@docs onUrlRequest

-}

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Platform
import Procedure.Advanced as Advanced exposing (Msg)
import Url exposing (Url)



-- Procedure


{-| A `Procedure` describes an application behaviour.
-}
type alias Procedure memory event =
    Advanced.Procedure (Cmd (Msg event)) memory event


{-| Construct a `Procedure` instance that does nothing and is just skipped.
-}
none : Procedure memory event
none =
    Advanced.none


{-| Batch `Procedure`s together. The elements are evaluated in order.
-}
batch : List (Procedure memory event) -> Procedure memory event
batch =
    Advanced.batch


{-| -}
wrapEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Procedure m e1
    -> Procedure m e0
wrapEvent wrapper proc =
    Advanced.wrapEvent wrapper proc
        |> Advanced.mapCmd
            (Cmd.map (mapMsg wrapper.wrap))


{-| -}
liftMemory :
    Pointer m0 m1
    -> Procedure m1 e
    -> Procedure m0 e
liftMemory =
    Advanced.liftMemory



-- Channel


{-| The _Channel_ is the concept, to which you can publish or subscribe _Events_.
-}
type alias Channel =
    Advanced.Channel


{-| Publish an event to a Channel.
-}
publish : Channel -> e -> Msg e
publish =
    Advanced.publish



-- Entry point


{-| Procedure version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).
-}
element :
    { init : memory
    , procedures : flags -> List (Procedure memory event)
    , view : (Channel, memory) -> Html (Msg event)
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
-}
document :
    { init : memory
    , procedures : flags -> List (Procedure memory event)
    , view : (Channel, memory) -> Document (Msg event)
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


{-| Procedure version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).

The `onUrlRequest` and `onUrlChange` Events are published to the application root Channel.

-}
application :
    { init : memory
    , procedures : flags -> Url -> Key -> List (Procedure memory event)
    , view : (Channel, memory) -> Document (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
    }
    -> Program flags memory event
application option =
    Browser.application
        { init =
            \flags url key ->
                init option.init (option.procedures flags url key)
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest option.onUrlRequest
        , onUrlChange = onUrlChange option.onUrlChange
        }


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags memory event =
    Platform.Program flags (Model memory event) (Msg event)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document event =
    Browser.Document event



-- Constructors


{-| Construct a `Procedure` instance that modifies the memory state.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the memory is not updated by another process during it is read and written by the `modify`.

-}
modify : (m -> m) -> Procedure m e
modify =
    Advanced.modify


{-| Construct a `Procedure` instance that issues a Command.

Because `push` is a relatively low level function, `request` will be useful for most cases.

-}
push : (m -> (e -> Msg e) -> Cmd (Msg e)) -> Procedure m e
push =
    Advanced.push


{-| Convenient alias for requests.
-}
type alias Request msg a =
    (a -> msg) -> Cmd msg


{-| -}
runRequest : (a -> e) -> Request (Msg e) a -> Procedure m e
runRequest toEvent req =
    push <|
        \_ toMsg ->
            req (toEvent >> toMsg)


{-| Construct a `Procedure` instance that subscribes a Subscription.
The Subscription lives untill the given Procedure list is completed.
In other words, the Events issued by this Subscription can only be caught by `await`s inside the given Procedure list.
-}
subscribe :
    Sub e
    -> List (Procedure m e)
    -> Procedure m e
subscribe =
    Advanced.subscribe


{-| Like `subscribe`, but it quit subscribing when it receives first event.
-}
subscribeOnce : Sub e -> Procedure m e
subscribeOnce =
    Advanced.subscribeOnce


{-| Construct a `Procedure` instance that awaits Events for the Channel.
If the callback function returns empty list, it awaits again; otherwise, it evaluates the returned `Procedure`.
For example, the following `await` awaits again if it receives `Event1`; or, it just proceeds to the next procedure if it receives `Event2`.

    [ Procedure.await <|
        \e _ ->
            case e of
                Event1 -> []
                Event2 -> [ Procedure.none ]
    , nextProcedures
    , ...

Note1: The memory state passed to the callback function may become outdated during running the process for the `Procedure` retuned by the function, so it is safe to use this value only to determine whether to accept or ignore events.

Note2: Technically, all the `modify` `push` `async` procedures appeared before the `await` will be executed internally as a single operation. This avoids the situation where an event triggered by a `push` passes through while processing tons of subsequent `modify`s and `push`s, thus ensuring that the `await` always can catch the event caused by the previous procedures.

Note3: `push`s written before an `await` will not necessarily cause events in the written order. For example, if the first `push` sends a request to the server and it fires an event with its result, and the second `push` sleeps for 0.1 seconds and then returns another event, the first event can fire later if the server is slow to respond. To avoid this situation, after using one `push`, catch it with `await` and use the next `push`. The `sync` can be also helpfull for handling such situations.

-}
await : (e -> m -> List (Procedure m e)) -> Procedure m e
await =
    Advanced.await


{-| Construct a `Procedure` instance that evaluates the given `Procedure`s asynchronously:

  - The subsequent `Procedure`s in the original process are evaluated immediately.
  - The `Procedure`s given as the argument are also evaluated immediately as the asynchronous process.
  - Even if the original process completed, the asynchronous process is not cancelled.
  - If the original process cancelled by `race`, the asynchronous process is also killed.

Note: When multiple `async`s are called, there is no guarantee that the procedures generated by the first called `async` will be executed first.

-}
async : List (Procedure m e) -> Procedure m e
async =
    Advanced.async


{-| Same as `Procedure.Pointer`.
-}
type alias Pointer m m1 =
    { get : m -> Maybe m1
    , modify : (m1 -> m1) -> m -> m
    }


{-| Similar to `async`, but given Procedures `await` for the given Channel.
-}
asyncOn :
    { get : m -> Maybe ( Channel, m1 )
    , set : m1 -> m -> m
    }
    -> Channel
    -> (Pointer m m1 -> List (Procedure m e))
    -> Procedure m e
asyncOn =
    Advanced.asyncOn


{-| Construct a `Procedure` instance that wait for all the given `Procedure`s to be completed.

Each `Procedure` is evaluated in the independent process, but the subsequent `Procedure`s in the original process are **not** evaluated immediately, but wait for all the given `Procedure`s to be completed.

-}
sync : List (Procedure m e) -> Procedure m e
sync =
    Advanced.sync


{-| Construct a `Procedure` instance that wait for one of the given `Procedure`s to be completed.

Each `Procedure` is evaluated in the independent process, but the subsequent `Procedure`s in the original process are **not** evaluated immediately, but wait for one of the given `Procedure`s to be completed.

Note1: If one of the `Procedure` completed, all other `Procedure`s will be killed after processing until the next `await`, where asynchronous processes spawned by the killed processes are also killed.

-}
race : List (Procedure m e) -> Procedure m e
race =
    Advanced.race


{-| Quit the thread immediately.

Subsequent `Procedure`s are not evaluated and are discarded.

-}
quit : Procedure m e
quit =
    Advanced.quit


{-| Ignore subsequent `Procedure`s, and just evaluate given `Procedure`s.

It is convenient for following two situations.


## Make recursive Procedure

Calling itself in the Procedure will result in a compile error; the `jump` enables to build recursive `Procedure`s.

    import Procedure exposing (Msg, Procedure)
    import Time exposing (Posix)

    clockProcedures : List (Procedure Memory Event)
    clockProcedures =
        [ Procedure.await <|
            \event _ ->
                case event of
                    ReceiveTick time ->
                        [ Procedure.modify <|
                            \m -> { m | time = time }
                        ]

                    _ ->
                        []
        , Procedure.jump <| \_ -> clockProcedures
        ]


## Safe pruning

Sometimes you may want to handle errors as follows:

    unsafePruning : List (Procedure Memory Event)
    unsafePruning =
        [ requestPosts
        , Procedure.await <|
            \event _ ->
                case event of
                    ReceivePosts (Err error) ->
                        [ handleError error
                            |> Procedure.batch
                        ]

                    ReceivePosts (Ok posts) ->
                        [ Procedure.modify <|
                            \m -> { m | posts = posts }
                        ]

                    _ ->
                        []
        , Procedure.batch proceduresForNewPosts
        ]

It appears to be nice, but it does not work as intended. Actually, the above `Procedure`s can evaluate the `proceduresForNewPosts` even after evaluating `handleError`. To avoid this, you can use `jump`:

    safePruning : List (Procedure Memory Event)
    safePruning =
        [ requestPosts
        , Procedure.await <|
            \event _ ->
                case event of
                    ReceivePosts (Err error) ->
                        [ Procedure.jump <| \_ -> handleError error
                        ]

                    ReceivePosts (Ok posts) ->
                        [ Procedure.modify <|
                            \m -> { m | posts = posts }
                        ]

                    _ ->
                        []
        , Procedure.batch proceduresForNewPosts
        ]

-}
jump :
    (() -> List (Procedure m e))
    -> Procedure m e
jump =
    Advanced.jump


{-| Construct a `Procedure` instance that uses private Channel.

    sleep : Float -> Procedure m Event
    sleep msec =
        Procedure.protected
            [ Procedure.push <|
                \_ toMsg ->
                    -- This publishes `WakeUp` event for private Channel,
                    -- so it only affects within `protected`.
                    Process.sleep msec
                        |> Task.perform (\() -> toMsg WakeUp)
            , Procedure.await <|
                \event _ ->
                    case event of
                        WakeUp ->
                            -- Do nothing, but do not await the next event.
                            [ Procedure.none
                            ]

                        _ ->
                            -- Do nothing, and await the next event again.
                            []
            ]

If the given `Modifier` has already expired, it does nothing and is just skipped.

-}
protected :
    List (Procedure m e)
    -> Procedure m e
protected =
    Advanced.protected



-- Helper procedures


{-| Determine next Procedures with current memory state.
-}
withMemory : (m -> List (Procedure m e)) -> Procedure m e
withMemory =
    Advanced.withMemory


{-| Evaluate the given `Procedure`s only if the first argument is `True`, otherwise same as `none`.
-}
when : Bool -> List (Procedure m e) -> Procedure m e
when =
    Advanced.when


{-| Evaluate the given `Procedure`s only if the first argument is `False`, otherwise same as `none`.
-}
unless : Bool -> List (Procedure m e) -> Procedure m e
unless =
    Advanced.unless


{-| Evaluate the `Procedure`s returned by the callback function only if the first argument is `Just`, otherwise same as `none`.
-}
withMaybe : Maybe a -> (a -> List (Procedure m e)) -> Procedure m e
withMaybe =
    Advanced.withMaybe



-- Local procedures


{-| Helper function to do subscribe and send for a port in one safe operation.

For example, we can use `portRequest` to get localStorage value safely.

In JavaScript side:

```js
app.ports.requestGetLocalName.subscribe((req) => {
  try {
    app.ports.receiveGetLocalName.send({
      // The `id` value, generated by TEPA, links
      // the subscribe port to the relevant send port.
      id: req.id,
      body: {
        name: localStorage.getItem(`Name.${req.body.userId}`),
      },
    });
  } catch {
    app.ports.receiveGetLocalName.send({
      id: req.id,
      body: {
        name: null,
      },
    });
  }
});
```

In Elm side:

    import App.UserId as UserId exposing (UserId)
    import Json.Decode as JD
    import Json.Encode as JE
    import Procedure exposing (Procedure, Request)


    getLocalName : UserId -> Procedure Memory Event
    getLocalName uid =
        Procedure.portRequest
            { requestPort = requestGetLocalName
            , requestBody = \_ ->
                JE.object
                    [ ( "userId", UserId.toValue uid )
                    ]
            , responsePort = receiveGetLocalName
            , responseBody = JD.field "name" (JD.nullable JD.string)
            }
            ReceiveLocalName


    myProcedures : UserId -> List (Procedure Memory Event)
    myProcedures uid =
        [ getLocalName uid
        , Procedure.await <|
            \event _ ->
                case event of
                    ReceiveLocalName name ->
                        [ Procedure.modify <|
                            \m -> setUserName uid name m
                        ]

                    _ ->
                        []
        , ...
        ]

-}
portRequest :
    { requestPort : Value -> Cmd (Msg e)
    , requestBody : m -> Value
    , responsePort : (Value -> Msg e) -> Sub (Msg e)
    , responseBody : Decoder a
    }
    -> (Result JD.Error a -> e)
    -> Procedure m e
portRequest =
    Advanced.portRequest



-- Observing


{-| Start observing a resource.

See [spa-sample](https://github.com/arowM/elm-procedure-architecture/tree/main/spa-sample) for real usage.

-}
observe :
    r
    -> (( Channel, r ) -> List (Procedure m e))
    -> Procedure m e
observe =
    Advanced.observe


{-| Start observing a list resource.

See [spa-sample](https://github.com/arowM/elm-procedure-architecture/tree/main/spa-sample) for real usage.

-}
observeList :
    List r
    -> (List ( Channel, r ) -> List (Procedure m e))
    -> Procedure m e
observeList =
    Advanced.observeList



-- Low level API


{-| TEA update function implementation for running your Procedures.
-}
update : Msg event -> Model memory event -> ( Model memory event, Cmd (Msg event) )
update msg model =
    Advanced.update msg model
        |> Tuple.mapSecond Cmd.batch


{-| TEA subscriptions function implementation for running your Procedures.
-}
subscriptions : Model memory event -> Sub (Msg event)
subscriptions =
    Advanced.subscriptions


{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    -> List (Procedure memory event)
    -> ( Model memory event, Cmd (Msg event) )
init initialMemory procs =
    Advanced.init initialMemory procs
        |> Tuple.mapSecond Cmd.batch


{-| Construct the TEA element view function.
-}
elementView : ((Channel, memory) -> Html (Msg event)) -> Model memory event -> Html (Msg event)
elementView =
    Advanced.elementView


{-| Construct the TEA document view function.
-}
documentView : ((Channel, memory) -> Document (Msg event)) -> Model memory event -> Document (Msg event)
documentView =
    Advanced.documentView


{-| Construct the TEA `onUrlChange` property value.
-}
onUrlChange : (Url -> event) -> Url -> Msg event
onUrlChange =
    Advanced.onUrlChange


{-| Construct the TEA `onUrlRequest` property value.
-}
onUrlRequest : (Browser.UrlRequest -> event) -> Browser.UrlRequest -> Msg event
onUrlRequest =
    Advanced.onUrlRequest


{-| TEA Message that wraps your events.
-}
type alias Msg event =
    Advanced.Msg event


{-| -}
mapMsg : (a -> b) -> Msg a -> Msg b
mapMsg =
    Advanced.mapMsg


{-| TEA Model that stores your Procedure state.
-}
type alias Model memory event =
    Advanced.Model (Cmd (Msg event)) memory event
