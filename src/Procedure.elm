module Procedure exposing
    ( Procedure
    , none
    , batch
    , wrapEvent
    , Observer
    , ObserverId
    , issue
    , element
    , document
    , application
    , Program
    , Document
    , Msg
    , mapMsg
    , modify
    , push
    , await
    , async
    , sync
    , race
    , quit
    , jump
    , protected
    , request
    , Request
    , when
    , unless
    , withMaybe
    , observe
    , observeList
    , update
    , init
    , rootMsg
    , Model
    , memoryState
    )

{-|


# Procedure

@docs Procedure
@docs none
@docs batch
@docs wrapEvent


# Observer

@docs Observer
@docs ObserverId
@docs issue


# Entry point

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#low-level-api) is also available for more advanced use cases, which enables you to introduce elm-procedure partially into your existing TEA app.

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
@docs await
@docs async
@docs sync
@docs race
@docs quit
@docs jump
@docs protected


# Helper procedures

@docs request
@docs Request
@docs when
@docs unless
@docs withMaybe


# Observing

@docs observe
@docs observeList


# Low level API

@docs update
@docs init
@docs rootMsg
@docs Model
@docs memoryState

-}

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Internal.ObserverId as ObserverId
import Platform
import Procedure.Advanced as Advanced exposing (Msg)
import Procedure.Observer exposing (Observer)
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



-- Observer


{-| The _Observer_ is the concept to observe a partial memory. You can issue an event to the Observer in views, and you can modify the partial memory state or capture events for the Observer in procedures.
-}
type alias Observer m m1 =
    Advanced.Observer m m1


{-| ID for the Observer.
-}
type alias ObserverId =
    Advanced.ObserverId


{-| Issue an event to the Observer.
-}
issue : ObserverId -> e -> Msg e
issue =
    Advanced.issue



-- Entry point


{-| Procedure version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).
-}
element :
    { init : memory
    , procedures : flags -> List (Procedure memory event)
    , view : memory -> Html (Msg event)
    , subscriptions : memory -> Sub (Msg event)
    }
    -> Program flags memory event
element option =
    Browser.element
        { init =
            \flags ->
                init option.init (option.procedures flags)
        , view = option.view << memoryState
        , update = update
        , subscriptions = option.subscriptions << memoryState
        }


{-| Procedure version of [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).
-}
document :
    { init : memory
    , procedures : flags -> List (Procedure memory event)
    , view : memory -> Document (Msg event)
    , subscriptions : memory -> Sub (Msg event)
    }
    -> Program flags memory event
document option =
    Browser.document
        { init =
            \flags ->
                init option.init (option.procedures flags)
        , view = option.view << memoryState
        , update = update
        , subscriptions = option.subscriptions << memoryState
        }


{-| Procedure version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).

The `onUrlRequest` and `onUrlChange` Events are issued to the `global` Observer.

-}
application :
    { init : memory
    , procedures : flags -> Url -> Key -> List (Procedure memory event)
    , view : memory -> Document (Msg event)
    , subscriptions : memory -> Sub (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
    }
    -> Program flags memory event
application option =
    Browser.application
        { init =
            \flags url key ->
                init option.init (option.procedures flags url key)
        , view = option.view << memoryState
        , update = update
        , subscriptions = option.subscriptions << memoryState
        , onUrlRequest = option.onUrlRequest >> rootMsg
        , onUrlChange = option.onUrlChange >> rootMsg
        }


{-| Issue Events to the application root.
You can use it for building your own `Browser.application`:

    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest >> Procedure.rootMsg
        , onUrlChange = OnUrlChange >> Procedure.rootMsg
        }

-}
rootMsg : event -> Msg event
rootMsg =
    issue ObserverId.init


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
If the given `Observer` has already expired, it does nothing and is just skipped.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the memory is not updated by another process during it is read and written by the `modify`.

-}
modify : Observer m m1 -> (m1 -> m1) -> Procedure m e
modify =
    Advanced.modify


{-| Construct a `Procedure` instance that issues `Cmd`s.
If the given `Observer` has already expired, it does nothing and is just skipped.

The `ObserverId` value provided to the callback function is used to `issue` events:

    push observer <|
        \oid _ toEvent ->
            Http.get
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                , expect = Http.expectString (issue oid << toEvent << GotText)
                }

-}
push : Observer m m1 -> (( ObserverId, m1 ) -> Cmd (Msg e)) -> Procedure m e
push =
    Advanced.push


{-| Construct a `Procedure` instance that awaits events for an observer.
If the second argument returns empty list, it awaits again.
Otherwise, it evaluates the returned `Procedure`.

If the given `Observer` has already expired, it awaits forever.

Note1: The memory state passed to the callback function may become outdated during running the process for the `Procedure` retuned by the function, so it is safe to use this value only to determine whether to accept or ignore events.

Note2: Technically, all the `modify` `push` `async` procedures appeared before the `await` will be executed internally as a single operation. This avoids the situation where an event triggered by a `push` passes through while processing tons of subsequent `modify`s and `push`s, thus ensuring that the `await` always can catch the event caused by the previous procedures.

Note3: `push`s written before an `await` will not necessarily cause events in the written order. For example, if the first `push` sends a request to the server and it fires an event with its result, and the second `push` sleeps for 0.1 seconds and then returns another event, the first event can fire later if the server is slow to respond. To avoid this situation, after using one `push`, catch it with `await` and use the next `push`. The `sync` can be also helpfull for handling such situations.

-}
await :
    Observer m m1
    -> (e -> m1 -> List (Procedure m e))
    -> Procedure m e
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


{-| Ignore subsequent `Procedure`s, and evaluate given `Procedure`s in the current process.
If the given `Observer` has already expired, it acts as the `quit`.

It is convenient for following two situations.


## Make recursive Procedure

Calling itself in the `Procedure` will result in a compile error; the `jump` enables to build recursive `Procedure`s.

    import Procedure exposing (Msg, Procedure)
    import Time exposing (Posix)

    clockProcedures : Observer m e Memory PageHome_ -> List (Procedure m e)
    clockProcedures pageHome =
        [ Procedure.await global <|
            \event _ ->
                case event of
                    ReceiveTick time ->
                        [ Procedure.modify pageHome <|
                            \home ->
                                { home | time = time }
                        ]

                    _ ->
                        []
        , Procedure.jump global <| \_ -> clockProcedures pageHome
        ]


## Safe pruning

Sometimes you may want to handle errors as follows:

    unsafePruning : List (Procedure Memory Event)
    unsafePruning =
        [ requestPosts
        , Procedure.await global <|
            \event _ ->
                case event of
                    ReceivePosts (Err error) ->
                        [ handleError error
                            |> Procedure.batch
                        ]

                    ReceivePosts (Ok posts) ->
                        [ Procedure.modify global <|
                            \memory ->
                                { memory | posts = posts }
                        ]

                    _ ->
                        []
        , Procedure.batch proceduresForNewPosts
        ]

It appears to be nice, but it does not work as intended. Actually, the above `Procedure`s can evaluate the `proceduresForNewPosts` even after evaluating `handleError`. To avoid this, you can use `jump`:

    safePruning : List (Procedure Memory Event)
    safePruning =
        [ requestPosts
        , Procedure.await global <|
            \event _ ->
                case event of
                    ReceivePosts (Err error) ->
                        [ Procedure.jump global <| \_ -> handleError error
                        ]

                    ReceivePosts (Ok posts) ->
                        [ Procedure.modify global <|
                            \memory ->
                                { memory | posts = posts }
                        ]

                    _ ->
                        []
        , Procedure.batch proceduresForNewPosts
        ]

-}
jump :
    Observer m m1
    -> (m1 -> List (Procedure m e))
    -> Procedure m e
jump =
    Advanced.jump



-- {-| Evaluate the `Procedure`s provided as the second argument until if the callback function returns non-empty list.
-- If the given `Procedure`s has been completed before the callback function returns non-empty list, it continues to wait for events.
-- If the given `Observer` has already expired, it just evaluates given procedures but it does not proceed to the next procedures.
--
-- The most useful way is to define a function that executes the `Procedure`s for the appropreate SPA page until the URL changes:
--
--     import Procedure exposing (Msg, Procedure)
--     import Url exposing (Url)
--
--     pageController : Route -> Procedure Memory Event
--     pageController route =
--         [ Procedure.doUntil global
--             -- The process for the `pageProcedures` will be killed
--             -- when the URL canges.
--             (pageProcedures route)
--           <|
--             \event _ ->
--                 case event of
--                     UrlChanged url ->
--                         [ Procedure.jump global <| \_ -> pageController (routeFromUrl url)
--                         ]
--
--                     _ ->
--                         []
--         ]
--
-- -}
-- doUntil :
--     Observer m e m1 e1
--     -> List (Procedure m e)
--     -> (e1 -> m1 -> List (Procedure m e))
--     -> Procedure m e
-- doUntil =
--     Advanced.doUntil


{-| Construct a `Procedure` instance that starts private process.

The callback function takes brand-new `Observer` just for it, so it can be used to issue and await private `Event`s for itself:

    sleep : Float -> Procedure Memory Event
    sleep msec =
        Procedure.protected global <|
            \priv ->
                [ Procedure.push priv <|
                    \oid _ ->
                        -- This issues `WakeUp` event for `priv` observer,
                        -- so it only affects this process.
                        Process.sleep msec
                            |> Task.perform (\() -> Procedure.issue oid WakeUp)
                , Procedure.await priv <|
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

If the given `Observer` has already expired, it does nothing and is just skipped.

-}
protected :
    (Observer m m1 -> List (Procedure m e))
    -> Observer m m1
    -> Procedure m e
protected =
    Advanced.protected



-- {-| Aquire a resource, do some work with it, and then release the resource.
--
--   - aquire: Evaluated immediately.
--       - arg1: new `ObserverId` for the aquired resource `r`
--       - arg2: current memory state
--       - returns: ( new memory state, aquired resource )
--   - release: Evaluated when `Procedure`s returned by the callback function, provided as the third argument, are completed or cancelled by `race` or `doUntil`.
--
-- If the given `Observer` has already expired, it does nothing and just skipped.
--
-- -}
-- withResource :
--     Observer m m1
--     ->
--         { aquire : ObserverId -> m1 -> ( m1, r )
--         , release : r -> m1 -> ( m1, Cmd (Msg e) )
--         }
--     -> (r -> List (Procedure m e))
--     -> Procedure m e
-- withResource o c =
--     Advanced.withResource o
--         { aquire = c.aquire
--         , release =
--             \r m1 ->
--                 c.release r m1
--                     |> Tuple.mapSecond List.singleton
--         }
-- Helper procedures


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


{-| Helper function to issue an request via HTTP, port, etc.
An example use case can be found on `App.Session` module in [spa-sample](https://github.com/arowM/elm-procedure/tree/main/spa-sample).
-}
request : (( ObserverId, m1 ) -> (a -> Msg e) -> Cmd (Msg e)) -> Observer m m1 -> Request m e a
request f observer =
    Advanced.request f observer identity


{-| -}
type alias Request m e a =
    (a -> e) -> Procedure m e



-- Observing


{-| Start observing a resource.
-}
observe :
    r
    -> (( ObserverId, r ) -> List (Procedure m e))
    -> Procedure m e
observe =
    Advanced.observe


{-| Start observing a list resource.
-}
observeList :
    List r
    -> (List ( ObserverId, r ) -> List (Procedure m e))
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


{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    -> List (Procedure memory event)
    -> ( Model memory event, Cmd (Msg event) )
init initialMemory procs =
    Advanced.init initialMemory procs
        |> Tuple.mapSecond Cmd.batch


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


{-| Extract current memory state from TEA Model.
-}
memoryState : Model memory event -> memory
memoryState =
    Advanced.memoryState
