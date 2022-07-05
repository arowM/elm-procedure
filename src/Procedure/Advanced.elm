module Procedure.Advanced exposing
    ( Procedure
    , none
    , batch
    , wrapEvent
    , mapCmd
    , mapCmds
    , Observer
    , ObserverId
    , issue
    , update
    , init
    , Msg
    , mapMsg
    , Model
    , memoryState
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
    )

{-|


# Procedure

@docs Procedure
@docs none
@docs batch
@docs wrapEvent
@docs mapCmd
@docs mapCmds


# Observer

@docs Observer
@docs ObserverId
@docs issue


# Connect to TEA app

@docs update
@docs init
@docs Msg
@docs mapMsg
@docs Model
@docs memoryState


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

-}

import Internal.Observer as Observer
import Internal.ObserverId as ObserverId



-- Observer


{-| Same as `Procedure.Observer`.
-}
type alias Observer m m1 =
    Observer.Observer m m1


{-| Same as `Procedure.ObserverId`.
-}
type alias ObserverId =
    ObserverId.ObserverId


{-| Same as `Procedure.issue`.
-}
issue : ObserverId -> e -> Msg e
issue =
    Msg



-- Procedure


{-| An advanced `Procedure` enables you dependency injection, which is especially useful for testing the application behaviour against sample event sequences.

I recommend starting with the simpler `Procedure` module, which provides the specialized `Procedure` type for running as an application.

-}
type Procedure cmd memory event
    = Procedure (List (ProcedureItem cmd memory event))


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
    | Protected (ObserverId -> List (ProcedureItem cmd memory event))
    | Sync (List (List (ProcedureItem cmd memory event)))
    | Race (List (List (ProcedureItem cmd memory event)))
      -- Ignore subsequent `Procedure`s and run given `Procedure`s in current thread.
    | Jump (memory -> List (ProcedureItem cmd memory event))
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
    List.concatMap (\(Procedure ps) -> ps) procs
        |> Procedure


{-| Just like `Procedure.wrapEvent`.
-}
wrapEvent :
    { wrap : e1 -> e0
    , unwrap : e0 -> Maybe e1
    }
    -> Procedure c m e1
    -> Procedure c m e0
wrapEvent wrapper (Procedure ps) =
    List.map (wrapEvent_ wrapper) ps
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
                \(Msg oid e0) m ->
                    wrapper.unwrap e0
                        |> Maybe.andThen
                            (\e1 ->
                                g (Msg oid e1) m
                                    |> Maybe.map
                                        (List.map (wrapEvent_ wrapper))
                            )

        Async ls ->
            Async <|
                List.map (wrapEvent_ wrapper) ls

        Observe g ->
            Observe <|
                \oid m ->
                    let
                        ( mNew, ps, h ) =
                            g oid m
                    in
                    ( mNew
                    , List.map (wrapEvent_ wrapper) ps
                    , h
                    )

        Protected g ->
            Protected <|
                \oid ->
                    g oid
                        |> List.map (wrapEvent_ wrapper)

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

        Jump g ->
            Jump <|
                \m ->
                    g m
                        |> List.map (wrapEvent_ wrapper)

        Quit ->
            Quit


{-| Transform the command produced by an `Procedure`.
-}
mapCmd : (c1 -> c0) -> Procedure c1 m e -> Procedure c0 m e
mapCmd f (Procedure ps) =
    List.map (mapCmd_ f) ps
        |> Procedure


{-| `mapCmd` for lists.
-}
mapCmds : (c1 -> c0) -> List (Procedure c1 m e) -> List (Procedure c0 m e)
mapCmds =
    List.map << mapCmd


mapCmd_ : (c1 -> c0) -> ProcedureItem c1 m e -> ProcedureItem c0 m e
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

        Async ls ->
            Async <|
                List.map (mapCmd_ f) ls

        Observe g ->
            Observe <|
                \oid m ->
                    let
                        ( mNew, ps, h ) =
                            g oid m
                    in
                    ( mNew
                    , List.map (mapCmd_ f) ps
                    , \mtmp ->
                        h mtmp
                            |> Tuple.mapSecond
                                (List.map f)
                    )

        Protected g ->
            Protected <|
                \oid ->
                    g oid
                        |> List.map (mapCmd_ f)

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

        Jump g ->
            Jump <|
                \m ->
                    g m
                        |> List.map (mapCmd_ f)

        Quit ->
            Quit


{-| Just like `Procedure.modify`.
-}
modify : Observer m m1 -> (m1 -> m1) -> Procedure c m e1
modify o f =
    Procedure
        [ Do <|
            \m0 ->
                case Observer.mget o m0 of
                    Nothing ->
                        ( m0, [] )

                    Just ( oid1, m1 ) ->
                        ( Observer.set o ( oid1, f m1 ) m0, [] )
        ]


{-| Just like `Procedure.push`.
-}
push : Observer m m1 -> (( ObserverId, m1 ) -> cmd) -> Procedure cmd m e1
push o f =
    Procedure
        [ Do <|
            \m0 ->
                case Observer.mget o m0 of
                    Nothing ->
                        ( m0, [] )

                    Just ( oid1, m1 ) ->
                        ( m0, [ f ( oid1, m1 ) ] )
        ]


{-| Just like `Procedure.await`.
-}
await :
    Observer m m1
    -> (e1 -> m1 -> List (Procedure c m e1))
    -> Procedure c m e1
await o f =
    Procedure
        [ Await
            (\(Msg targetId e0) m0 ->
                Observer.mget o m0
                    |> Maybe.andThen
                        (\( oid1, m1 ) ->
                            if targetId == oid1 then
                                case f e0 m1 of
                                    [] ->
                                        Nothing

                                    ps ->
                                        let
                                            (Procedure items) =
                                                batch ps
                                        in
                                        Just items

                            else
                                Nothing
                        )
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
        [ Async items
        ]


{-| Just like `Procedure.protected`.
-}
protected :
    (Observer m m1 -> List (Procedure c m e1))
    -> Observer m m1
    -> Procedure c m e1
protected f o =
    Procedure
        [ Protected <|
            \expected ->
                let
                    (Procedure items) =
                        Observer.setExpect expected o
                            |> f
                            |> batch
                in
                items
        ]



-- {-| Just like `Procedure.withResource`.
-- -}
-- withResource :
--     Observer m m1
--     ->
--         { aquire : ObserverId -> m1 -> ( m1, r )
--         , release : r -> m1 -> ( m1, List cmd )
--         }
--     -> (r -> List (Procedure cmd m e))
--     -> Procedure cmd m e
-- withResource (Observer { mget, set }) { aquire, release } f =
--     observe_ <|
--         \rid m0 ->
--             case mget m0 of
--                 Nothing ->
--                     ( m0, [], \m -> ( m, [] ) )
--
--                 Just m1 ->
--                     let
--                         ( m1New, r ) =
--                             aquire rid m1
--                     in
--                     ( set m1New m0
--                     , f r
--                     , \finalM0 ->
--                         case mget finalM0 of
--                             Nothing ->
--                                 ( finalM0, [] )
--
--                             Just finalM1 ->
--                                 let
--                                     ( m1Released, cmds ) =
--                                         release r finalM1
--                                 in
--                                 ( set m1Released finalM0, cmds )
--                     )


{-| Just like `Procedure.sync`.
-}
sync : List (Procedure c m e) -> Procedure c m e
sync ps =
    Procedure
        [ Sync <|
            List.map (\(Procedure items) -> items) ps
        ]


{-| Just like `Procedure.race`.
-}
race : List (Procedure c m e) -> Procedure c m e
race ps =
    Procedure
        [ Race <|
            List.map (\(Procedure items) -> items) ps
        ]


{-| Just like `Procedure.quit`.
-}
quit : Procedure c m e
quit =
    Procedure [ Quit ]


{-| Just like `Procedure.jump`.
-}
jump :
    Observer m m1
    -> (m1 -> List (Procedure c m e1))
    -> Procedure c m e1
jump o f =
    Procedure
        [ Jump <|
            \m0 ->
                case Observer.mget o m0 of
                    Nothing ->
                        []

                    Just ( _, m1 ) ->
                        let
                            (Procedure items) =
                                batch (f m1)
                        in
                        items
        ]



-- {-| Just like `Procedure.doUntil`.
-- -}
-- doUntil :
--     Observer m e m1 e1
--     -> List (Procedure c m e)
--     -> (e1 -> m1 -> List (Procedure c m e))
--     -> Procedure c m e
-- doUntil o procs handler =
--     sync
--         [ await o handler
--         , race
--             [ batch procs
--             , await o <|
--                 \e1 m1 ->
--                     case handler e1 m1 of
--                         [] ->
--                             []
--
--                         _ ->
--                             [ none ]
--             ]
--         ]
-- observe_ :
--     (ObserverId
--      -> m
--      ->
--         ( m
--         , List (Procedure cmd m e)
--         , m -> ( m, List cmd )
--         )
--     )
--     -> Procedure cmd m e
-- observe_ f =
--     Procedure
--         [ Observe <|
--             \rid m0 ->
--                 let
--                     ( finalM0, ps, finally ) =
--                         f rid m0
--
--                     (Procedure items) =
--                         batch ps
--                 in
--                 ( finalM0, items, finally )
--         ]


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


{-| Just like `Procedure.request`.
-}
request : (( ObserverId, m1 ) -> (a -> Msg e1) -> cmd1) -> Observer m m1 -> Request cmd m e1 cmd1 a
request f observer toCmd toEvent =
    push observer <|
        \( id, state ) ->
            toCmd <| f ( id, state ) (toEvent >> issue id)


{-| Just like `Procedure.Request`.
-}
type alias Request cmd m e cmd1 a =
    (cmd1 -> cmd) -> (a -> e) -> Procedure cmd m e



-- Observing


{-| Just like `Procedure.observe`.
-}
observe :
    r
    -> (( ObserverId, r ) -> List (Procedure c m e1))
    -> Procedure c m e1
observe r f =
    Procedure
        [ Protected <|
            \priv ->
                let
                    (Procedure ps) =
                        f ( priv, r ) |> batch
                in
                ps
        ]


{-| Just like `Procedure.observeList`.
-}
observeList :
    List r
    -> (List ( ObserverId, r ) -> List (Procedure c m e1))
    -> Procedure c m e1
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


{-| Just like `Procedure.memoryState`.
-}
memoryState : Model cmd memory event -> memory
memoryState (Thread { newState }) =
    newState.memory


{-| Just like `Procedure.update`.
-}
update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg (Thread t) =
    let
        (Thread t2) =
            t.next msg t.newState
    in
    ( Thread t2, t2.cmds )


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
                    , nextObserverId = ObserverId.inc ObserverId.init
                    }
                    items
    in
    ( Thread t, t.cmds )


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

        (Protected f) :: ps2 ->
            let
                ps1 =
                    f state.nextObserverId

                state1 =
                    { state
                        | nextObserverId = ObserverId.inc state.nextObserverId
                    }
            in
            fromProcedure state1 (ps1 ++ ps2)

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


{-| Same as `Procedure.Msg`.
-}
type Msg event
    = Msg ObserverId event


{-| Same as `Procedure.mapMsg`.
-}
mapMsg : (a -> b) -> Msg a -> Msg b
mapMsg f (Msg id a) =
    Msg id (f a)
