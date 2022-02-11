module Procedure exposing
    ( element
    , document
    , application
    , Procedure
    , Procedure_
    , Program
    , Document
    , Model
    , Msg
    , issue
    , publish
    , none
    , batch
    , modify
    , push
    , await
    , async
    , sync
    , race
    , quit
    , jump
    , doUntil
    , protected
    , withResource
    , when
    , unless
    , withMaybe
    , Observer
    , global
    , dig
    , setVariant
    , prepend
    , append
    , insertBefore
    , insertAfter
    , remove
    , memoryState
    , update
    , init
    )

{-|


# Entry point

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#low-level-api) is also available for more advanced use cases.

@docs element
@docs document
@docs application
@docs Procedure
@docs Procedure_
@docs Program
@docs Document
@docs Model


# Msg

@docs Msg
@docs issue
@docs publish


# Primitive Procedures

@docs none
@docs batch
@docs modify
@docs push
@docs await
@docs async
@docs sync
@docs race
@docs quit
@docs jump
@docs doUntil
@docs protected
@docs withResource
@docs when
@docs unless
@docs withMaybe


# Observer

@docs Observer
@docs global
@docs dig
@docs setVariant
@docs prepend
@docs append
@docs insertBefore
@docs insertAfter
@docs remove


# Low level API

@docs memoryState
@docs update
@docs init

-}

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Internal.ObserverId as ObserverId exposing (ObserverId)
import Platform
import Url exposing (Url)



-- Entry Point


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags memory event =
    Platform.Program flags (Model (Cmd (Msg event)) memory event) (Msg event)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document event =
    Browser.Document event


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
                    |> concatPair
        , view =
            \model -> option.view (memoryState model)
        , update =
            \msg model ->
                update msg model |> concatPair
        , subscriptions =
            \model ->
                option.subscriptions (memoryState model)
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
                    |> concatPair
        , view =
            \model -> option.view (memoryState model)
        , update =
            \msg model ->
                update msg model |> concatPair
        , subscriptions =
            \model ->
                option.subscriptions (memoryState model)
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
                    |> concatPair
        , view =
            \model -> option.view (memoryState model)
        , update =
            \msg model ->
                update msg model |> concatPair
        , subscriptions =
            \model ->
                option.subscriptions (memoryState model)
        , onUrlRequest = option.onUrlRequest >> issue ObserverId.init
        , onUrlChange = option.onUrlChange >> issue ObserverId.init
        }


concatPair : ( model, List (Cmd msg) ) -> ( model, Cmd msg )
concatPair ( model, cmds ) =
    ( model, Cmd.batch cmds )



-- Core


{-| -}
type Model cmd memory event
    = Thread
        -- New thread state after the evaluation.
        { newState : ThreadState memory

        -- Side effects caused by the evaluation.
        , cmds : List cmd

        -- New thread to evaluate next time.
        , next : Msg event -> ThreadState memory -> Thread cmd memory event
        }


type alias Thread cmd memory event =
    Model cmd memory event


{-| Extract current memory state.
-}
memoryState : Model cmd memory event -> memory
memoryState (Thread { newState }) =
    newState.memory


{-| -}
update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg (Thread t) =
    let
        (Thread t2) =
            t.next msg t.newState
    in
    ( Thread t2, t2.cmds )


{-| Construct the initial TEA data from `Procedure`s.
-}
init :
    memory
    -> List (Procedure_ cmd memory event)
    -> ( Model cmd memory event, List cmd )
init initialMemory procs =
    let
        (Procedure_ items) =
            batch procs

        (Thread t) =
            toThread <|
                fromProcedure
                    { memory = initialMemory
                    , nextObserverId = ObserverId.inc ObserverId.init
                    }
                    items
    in
    ( Thread t, t.cmds )


{-| -}
type Msg event
    = Msg ObserverId event


{-| Issue an event to the Observer specified by the `ObserverId`.
-}
issue : ObserverId -> event -> Msg event
issue =
    Msg


{-| Issue an event to the global `Observer`.
-}
publish : event -> Msg event
publish =
    Msg ObserverId.init


{-| State to evaluate a thread.
-}
type alias ThreadState memory =
    { memory : memory
    , nextObserverId : ObserverId
    }


{-| Intermediate type, which helps to handle operations that affects ancestor threads.
-}
type FromProcedure cmd memory event
    = FromProcedure
        { newState : ThreadState memory
        , cmds : List cmd
        , next :
            Maybe
                { procedure : Msg event -> ThreadState memory -> FromProcedure cmd memory event
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


endOfThread : Msg event -> ThreadState memory -> Thread cmd memory event
endOfThread _ state =
    Thread
        { newState = state
        , cmds = []
        , next = endOfThread
        }


type ProcedureItem cmd memory event
    = Do (memory -> ( memory, List cmd ))
    | Await (Msg event -> memory -> Maybe (List (ProcedureItem cmd memory event)))
      -- Run concurrently in new thread, killed when the parent thread is killed.
      -- The parent thread keep alive if the new thread alives.
    | Async (List (ProcedureItem cmd memory event))
    | Observe
        (ObserverId
         -> memory
         ->
            ( memory -- new memory state after resource assignment.
            , List (ProcedureItem cmd memory event) -- Procedures for assigned resource.
            , memory
              -> ( memory, List cmd ) -- Evaluated when the second element of this tuple ends.
            )
        )
    | Sync (List (List (ProcedureItem cmd memory event)))
    | Race (List (List (ProcedureItem cmd memory event)))
      -- Ignore subsequent `Procedure`s and run given `Procedure`s in current thread.
    | Jump (memory -> List (ProcedureItem cmd memory event))
    | Quit


fromProcedure : ThreadState memory -> List (ProcedureItem cmd memory event) -> FromProcedure cmd memory event
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

        (Observe f) :: ps2 ->
            let
                ( memory1, ps1, finally ) =
                    f state.nextObserverId state.memory

                state1 =
                    { memory = memory1
                    , nextObserverId = ObserverId.inc state.nextObserverId
                    }
            in
            fromProcedure state1 ps1
                |> applyFinally finally
                |> andThen (\s -> fromProcedure s ps2)

        (Sync ps) :: ps2 ->
            fromProcDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

        (Race ps) :: ps2 ->
            fromProcRaceDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

        (Jump f) :: _ ->
            fromProcedure state (f state.memory)

        Quit :: _ ->
            endOfProcedure state


endOfProcedure : ThreadState memory -> FromProcedure cmd memory event
endOfProcedure s =
    FromProcedure
        { newState = s
        , cmds = []
        , next = Nothing
        }


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
andThen : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
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


andAsync : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
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
fromProcDeps : ThreadState memory -> List (List (ProcedureItem cmd memory event)) -> FromProcedure cmd memory event
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


andNextDep : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andNextDep =
    andAsync


{-| Merge dependent procedures for `Race` into one procedure.
If given empty list, it skips to next `Procedure`.
-}
fromProcRaceDeps : ThreadState memory -> List (List (ProcedureItem cmd memory event)) -> FromProcedure cmd memory event
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


andNextRaceDep : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
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


{-| -}
type alias Procedure memory event =
    Procedure_ (Cmd (Msg event)) memory event


{-| Low level representation for procedures.

You can fill `cmd` with arbitrary types. This makes it easy to test that your Procedure returns the appropriate Cmd.

-}
type Procedure_ cmd memory event
    = Procedure_ (List (ProcedureItem cmd memory event))


{-| Construct a `Procedure` instance that does nothing.
-}
none : Procedure_ cmd memory event
none =
    Procedure_ []


{-| Batch `Procedure`s together. The elements are evaluated in order.
-}
batch : List (Procedure_ cmd memory event) -> Procedure_ cmd memory event
batch procs =
    List.concatMap (\(Procedure_ ps) -> ps) procs
        |> Procedure_


{-| Use to lift memory type.
-}
type alias Lifter a b =
    { get : a -> Maybe b
    , set : b -> a -> a
    }


{-| Use to convert local event types.
-}
type alias Wrapper a b =
    { unwrap : a -> Maybe b
    , wrap : b -> a
    }


{-| Construct a `Procedure` instance that modifies the memory state.
If the given `Observer` has expired, it does nothing.

Note that the update operation, passed as the second argument, is performed atomically; it means the state of the memory is not updated by another process during it is read and written by the `modify`.

-}
modify : Observer memory a -> (a -> a) -> Procedure_ cmd memory event
modify (Observer { lifter }) f =
    Procedure_
        [ Do <|
            \memory ->
                case lifter.get memory of
                    Nothing ->
                        ( memory, [] )

                    Just a ->
                        ( lifter.set (f a) memory, [] )
        ]


{-| Construct a `Procedure` instance that issues a `Cmd`.
If the given `Observer` has expire, it does nothing.

The callback function takes `ObserverId`, which can be used to return an `Event` for the target `Observer`:

    push targetObserver <|
        \oid _ ->
            Http.get
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                , expect = Http.expectString (issue oid << GotText)
                }

-}
push : Observer memory a -> (ObserverId -> a -> cmd) -> Procedure_ cmd memory event
push (Observer { id, lifter }) f =
    Procedure_
        [ Do <|
            \memory ->
                case lifter.get memory of
                    Nothing ->
                        ( memory, [] )

                    Just a ->
                        ( memory, [ f id a ] )
        ]


{-| Construct a `Procedure` instance that awaits the local events for the thread.
If the given `Observer` has expired, it awaits again.

If the second argument returns empty list, it awaits again.
Otherwise, it evaluates the returned `Procedure`.

Note1: The memory state passed to the callback function may become outdated during running the process for the `Procedure` retuned by the function, so it is safe to use this value only to determine whether to accept or ignore events.

Note2: Technically, all the `modify` `push` `async` written before the `await` will be executed internally as a single operation. This avoids the situation where an event triggered by a `push` passes through while processing tons of subsequent `modify`s and `push`s, thus ensuring that the `await` always can catch the event caused by the previous `Procedure`s.

Note3: `push`s written before an `await` will not necessarily cause events in the order written. For example, if the first `push` sends a request to the server and it fires a local event with its result, and the second `push` sleeps for 0.1 seconds and then returns a local event, the first local event can fire later if the server is slow to respond. To avoid this situation, after using one `push`, catch it with `await` and use the next `push`. The `sync` can be helpfull for the situation.

-}
await : Observer memory a -> (event -> a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
await (Observer { id, lifter }) f =
    Procedure_
        [ Await <|
            \(Msg rid event) memory ->
                case lifter.get memory of
                    Just a ->
                        if rid == id then
                            case f event a of
                                [] ->
                                    Nothing

                                ps ->
                                    let
                                        (Procedure_ items) =
                                            batch ps
                                    in
                                    Just items

                        else
                            Nothing

                    Nothing ->
                        Nothing
        ]


{-| Construct a `Procedure` instance that evaluates the given `Procedure`s asynchronously:

  - The subsequent `Procedure`s in the original process are evaluated immediately.
  - The `Procedure`s given as the argument are also evaluated immediately in the another process.
  - Even if the original process completed, the asynchronous process is not cancelled.
  - If the original process cancelled by `race` or `doUntil`, the asynchronous process also killed.

-}
async : List (Procedure_ cmd memory event) -> Procedure_ cmd memory event
async ps =
    let
        (Procedure_ items) =
            batch ps
    in
    Procedure_
        [ Async items
        ]


{-| Construct a `Procedure` instance that runs private process.
If the given `Observer` has expired, it does nothing.

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

-}
protected : Observer memory a -> (Observer memory a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
protected (Observer observer) f =
    observe <|
        \oid memory ->
            ( memory
            , f <|
                Observer
                    { id = oid
                    , lifter = observer.lifter
                    }
            , \m -> ( m, [] )
            )


{-| Aquire a resource, do some work with it, and then release the resource.
If the given `Observer` has expired, it does nothing.

  - aquire: Evaluated immediately.
      - arg1: new `ObserverId` for the aquired resource `r`
      - arg2: current memory state
      - returns: ( new memory state, aquired resource )
  - release: Evaluated when `Procedure`s returned by the callback function completed or cancelled by `race` or `doUntil`.

-}
withResource :
    Observer memory a
    ->
        { aquire : ObserverId -> a -> ( a, r )
        , release : r -> a -> ( a, List cmd )
        }
    -> (r -> List (Procedure_ cmd memory event))
    -> Procedure_ cmd memory event
withResource (Observer { lifter }) { aquire, release } f =
    observe <|
        \rid memory ->
            case lifter.get memory of
                Nothing ->
                    ( memory, [], \m -> ( m, [] ) )

                Just a ->
                    let
                        ( a1, r ) =
                            aquire rid a
                    in
                    ( lifter.set a1 memory
                    , f r
                    , \m ->
                        case lifter.get m of
                            Nothing ->
                                ( m, [] )

                            Just a2 ->
                                let
                                    ( a3, cmds ) =
                                        release r a2
                                in
                                ( lifter.set a3 m, cmds )
                    )


{-| Construct a `Procedure` instance that wait for all the given `Procedure`s to be completed.

Each `Procedure` is evaluated in the independent process, but the subsequent `Procedure`s in the original process are **not** evaluated immediately, but wait for all the given `Procedure`s to be completed.

-}
sync : List (Procedure_ cmd memory event) -> Procedure_ cmd memory event
sync ps =
    Procedure_
        [ Sync <|
            List.map (\(Procedure_ items) -> items) ps
        ]


{-| Construct a `Procedure` instance that wait for one of the given `Procedure`s to be completed.

Each `Procedure` is evaluated in the independent process, but the subsequent `Procedure`s in the original process are **not** evaluated immediately, but wait for one of the given `Procedure`s to be completed.

Note1: If one of the `Procedure` completed, all other `Procedure`s will be killed after processing until the next `await`, where asynchronous processes spawned by the killed processes are also killed.

-}
race : List (Procedure_ cmd memory event) -> Procedure_ cmd memory event
race ps =
    Procedure_
        [ Race <|
            List.map (\(Procedure_ items) -> items) ps
        ]


{-| Quit the thread immediately.

Subsequent `Procedure`s are not evaluated and are discarded.

-}
quit : Procedure_ cmd memory event
quit =
    Procedure_ [ Quit ]


{-| Ignore subsequent `Procedure`s, and evaluate given `Block` in the current thread. It is convenient for following two situations.
If the given `Observer` has expired, it acts as the `quit`.


## Make recursive Procedure

Calling itself in the `Procedure` will result in a compile error; the `jump` enables to build recursive `Procedure`s.

    import Procedure exposing (Msg, Procedure)
    import Time exposing (Posix)

    clockProcedures : Observer Memory PageHome_ -> List (Procedure Memory Event)
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
jump : Observer memory a -> (a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
jump (Observer { lifter }) f =
    Procedure_
        [ Jump <|
            \memory ->
                case lifter.get memory of
                    Nothing ->
                        []

                    Just a ->
                        let
                            (Procedure_ items) =
                                batch (f a)
                        in
                        items
        ]


{-| Evaluate the `Procedure`s provided as the second argument until it the callback function returns non-empty list.
If the given `Observer` has expired, it evaluates given `Procedures`, but it awaits again.

The most useful way is to define a function that executes the `Procedure`s for the appropreate SPA page until the URL changes:

    import Procedure exposing (Msg, Procedure)
    import Url exposing (Url)

    pageController : Route -> Procedure Memory Event
    pageController route =
        [ Procedure.doUntil global
            -- The process for the `pageProcedures` will be killed
            -- when the URL canges.
            (pageProcedures route)
          <|
            \event _ ->
                case event of
                    UrlChanged url ->
                        [ Procedure.jump global <| \_ -> pageController (routeFromUrl url)
                        ]

                    _ ->
                        []
        ]

-}
doUntil : Observer memory a -> List (Procedure_ cmd memory event) -> (event -> a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
doUntil o procs handler =
    sync
        [ await o handler
        , race
            [ batch procs
            , await o <|
                \event a ->
                    case handler event a of
                        [] ->
                            []

                        _ ->
                            [ none ]
            ]
        ]


observe :
    (ObserverId
     -> memory
     ->
        ( memory
        , List (Procedure_ cmd memory event)
        , memory -> ( memory, List cmd )
        )
    )
    -> Procedure_ cmd memory event
observe f =
    Procedure_
        [ Observe <|
            \rid memory ->
                let
                    ( memory2, ps, finally ) =
                        f rid memory

                    (Procedure_ items) =
                        batch ps
                in
                ( memory2, items, finally )
        ]


{-| Evaluate the given `Procedure`s only if the first argument is `True`, otherwise do nothing.
-}
when : Bool -> List (Procedure_ cmd memory event) -> Procedure_ cmd memory event
when p ls =
    if p then
        batch ls

    else
        none


{-| Evaluate the given `Procedure`s only if the first argument is `False`, otherwise do nothing.
-}
unless : Bool -> List (Procedure_ cmd memory event) -> Procedure_ cmd memory event
unless p =
    when (not p)


{-| Evaluate the `Procedure`s returned by the callback function only if the first argument is `Just`, otherwise do nothing.
-}
withMaybe : Maybe a -> (a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            f a
                |> batch



-- Observer


{-| Observer for the `part`.
-}
type Observer memory part
    = Observer
        { id : ObserverId
        , lifter : Lifter memory part
        }


{-| Global Observer.
-}
global : Observer memory memory
global =
    Observer
        { id = ObserverId.init
        , lifter =
            { get = Just
            , set = \x _ -> x
            }
        }


{-| Return a new `Observer` for the specific field.
All the Events issued to the original Observer can be also issued to the new Observer.

    type alias PageHome =
        { formA : FormA
        , formB : FormB
        }

    type alias FormB =
        { name : String
        , age : Int
        }

    pageHomeProcedures : Observer Memory PageHome -> List (Procedure Memory Event)
    pageHomeProcedures pageHome =
        let
            formB : Observer Memory FormB
            formB =
                pageHome
                    |> Procedure.dig
                        { get = .formB >> Just
                        , set = \a memory -> { memory | formB = a }
                        }
        in
        [ Procedure.await formB <|
            \event _ ->
                case event of
                    ChangeFormBName name ->
                        [ Procedure.modify formB <|
                            \memory ->
                                { memory | name = name }
                        ]

                    _ ->
                        []
        ]

-}
dig :
    { get : b -> Maybe a
    , set : a -> b -> b
    }
    -> Observer memory b
    -> Observer memory a
dig lifter (Observer observer) =
    Observer
        { id = observer.id
        , lifter =
            observer.lifter
                |> andCompose lifter
        }


andCompose : Lifter b c -> Lifter a b -> Lifter a c
andCompose l2 l1 =
    { get =
        \a ->
            l1.get a
                |> Maybe.andThen l2.get
    , set =
        \c a ->
            case l1.get a of
                Nothing ->
                    a

                Just b ->
                    l2.set c b
                        |> (\newb -> l1.set newb a)
    }


{-| Set new variant value, and create the `Observer` for it.
If the given `Observer` has expired, the call back function is called, but is passed an expired `Observer`.

    type Memory
        = PageLoading
        | PageHome ( ObserverId, PageHome_ )

    init : Memory
    init =
        PageLoading

    procedures : List (Procedure Memory Event)
    procedures =
        [ Procedure.setVariant
            global
            { wrap = PageHome
            , unwrap =
                \memory ->
                    case memory of
                        PageHome a ->
                            Just a

                        _ ->
                            Nothing
            }
            initialPageHome_
            pageHomeProcedures
        ]

    pageHomeProcedures : Observer Memory PageHome_ -> List (Procedure Memory Event)
    pageHomeProcedures pageHome =
        Debug.todo ""

-}
setVariant :
    Observer memory a
    ->
        { wrap : ( ObserverId, b ) -> a
        , unwrap : a -> Maybe ( ObserverId, b )
        }
    -> b
    -> (Observer memory b -> List (Procedure_ cmd memory event))
    -> Procedure_ cmd memory event
setVariant (Observer parent) wrapper b f =
    withResource
        (Observer
            { id = parent.id
            , lifter =
                { get = Just
                , set = \x _ -> x
                }
            }
        )
        { aquire =
            \rid memory ->
                let
                    r =
                        Observer
                            { id = rid
                            , lifter =
                                parent.lifter
                                    |> andCompose (wrapperToVariantLifter wrapper)
                                    |> andCompose
                                        { get = \( _, x ) -> Just x
                                        , set = \x ( id, _ ) -> ( id, x )
                                        }
                            }
                in
                ( parent.lifter.set (wrapper.wrap ( rid, b )) memory, r )
        , release = \_ m -> ( m, [] )
        }
        f


{-| Prepend new item, and create the `Observer` for it.
If the given `Observer` has expired, it does nothing.

For a sample, see [`sample/src/Advanced.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src/Advanced.elm).

-}
prepend : Observer memory (List ( ObserverId, a )) -> a -> (Observer memory a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
prepend observer a =
    modifyList observer (\rid xs -> ( rid, a ) :: xs)


{-| Append new item, and create the `Observer` for it.
If the given `Observer` has expired, it does nothing.

For a sample, see [`sample/src/Advanced.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src/Advanced.elm).

-}
append : Observer memory (List ( ObserverId, a )) -> a -> (Observer memory a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
append observer a =
    modifyList observer (\rid xs -> xs ++ [ ( rid, a ) ])


{-| Insert new item before the base `Observer`, and create the `Observer` for it.
If the `Observer` provided as the first argument has expired, it does nothing.
If the `Observer` for the base element has expired, the call back function is called, but is passed an expired `Observer`.

For a sample, see [`sample/src/Advanced.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src/Advanced.elm).

-}
insertBefore : Observer memory (List ( ObserverId, a )) -> Observer memory a -> a -> (Observer memory a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
insertBefore observer (Observer base) a =
    modifyList observer <|
        \newRid xs ->
            List.foldr
                (\( rid, v ) acc ->
                    if rid == base.id then
                        ( newRid, a ) :: ( rid, v ) :: acc

                    else
                        ( rid, v ) :: acc
                )
                []
                xs


{-| Insert new item after the base `Observer`, and create the `Observer` for it.
If the `Observer` provided as the first argument has expired, it does nothing.
If the `Observer` for the base element has expired, the call back function is called, but is passed an expired `Observer`.

For a sample, see [`sample/src/Advanced.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src/Advanced.elm).

-}
insertAfter : Observer memory (List ( ObserverId, a )) -> Observer memory a -> a -> (Observer memory a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
insertAfter observer (Observer base) a =
    modifyList observer <|
        \newRid xs ->
            List.foldr
                (\( rid, v ) acc ->
                    if rid == base.id then
                        ( rid, v ) :: ( newRid, a ) :: acc

                    else
                        ( rid, v ) :: acc
                )
                []
                xs


modifyList : Observer memory (List ( ObserverId, a )) -> (ObserverId -> List ( ObserverId, a ) -> List ( ObserverId, a )) -> (Observer memory a -> List (Procedure_ cmd memory event)) -> Procedure_ cmd memory event
modifyList o modifier f =
    protected o <|
        \new ->
            let
                (Observer { id }) =
                    new
            in
            [ modify new <| modifier id
            , batch <|
                f
                    (new
                        |> dig
                            { get = getListItem id
                            , set = setListItem id
                            }
                    )
            ]


getListItem : ObserverId -> List ( ObserverId, a ) -> Maybe a
getListItem target ls =
    List.filterMap
        (\( rid, a ) ->
            if rid == target then
                Just a

            else
                Nothing
        )
        ls
        |> List.head


setListItem : ObserverId -> a -> List ( ObserverId, a ) -> List ( ObserverId, a )
setListItem target a ls =
    List.map
        (\( rid, v ) ->
            if rid == target then
                ( rid, a )

            else
                ( rid, v )
        )
        ls


{-| Remove an item from the list.
-}
remove : Observer memory (List ( ObserverId, a )) -> Observer memory a -> Procedure_ cmd memory event
remove list (Observer item) =
    modify list <| removeListItem item.id


removeListItem : ObserverId -> List ( ObserverId, a ) -> List ( ObserverId, a )
removeListItem target ls =
    List.filter (\( rid, _ ) -> rid /= target) ls


wrapperToVariantLifter : Wrapper a b -> Lifter a b
wrapperToVariantLifter wrapper =
    { get = wrapper.unwrap
    , set =
        \a memory ->
            case wrapper.unwrap memory of
                Nothing ->
                    memory

                Just _ ->
                    wrapper.wrap a
    }
