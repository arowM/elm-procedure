module Procedure.Advanced exposing
    ( Procedure
    , none
    , batch
    , wrapEvent
    , mapCmd
    , mapCmds
    , Modifier
    , Channel
    , publish
    , update
    , elementView
    , documentView
    , subscriptions
    , init
    , Msg
    , mapMsg
    , Model
    , modify
    , push
    , subscribe
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


# Modifier

@docs Modifier


# Channel

@docs Channel
@docs publish


# Connect to TEA app

@docs update
@docs elementView
@docs documentView
@docs subscriptions
@docs init
@docs Msg
@docs mapMsg
@docs Model


# Constructors

@docs modify
@docs push
@docs subscribe
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

import Browser exposing (Document)
import Html exposing (Html)
import Internal.Channel as Channel
import Internal.Modifier as Modifier
import Internal.SubId as SubId



-- Modifier


{-| Same as `Procedure.Modifier`.
-}
type alias Modifier memory part =
    Modifier.Modifier memory part



-- Channel


{-| Same as `Procedure.Channel`.
-}
type alias Channel =
    Channel.Channel


{-| Same as `Procedure.publish`.
-}
publish : Channel -> e -> Msg e
publish =
    Msg



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
    | Sync (List (List (ProcedureItem cmd memory event)))
    | Race (List (List (ProcedureItem cmd memory event)))
      -- Ignore subsequent `Procedure`s and run given `Procedure`s in current thread.
    | Jump (memory -> List (ProcedureItem cmd memory event))
    | Subscribe (Sub (Msg event)) (SubId -> List (ProcedureItem cmd memory event))
    | Unsubscribe SubId
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
                \(Msg c e0) m ->
                    wrapper.unwrap e0
                        |> Maybe.andThen
                            (\e1 ->
                                g (Msg c e1) m
                                    |> Maybe.map
                                        (List.map (wrapEvent_ wrapper))
                            )

        Async ls ->
            Async <|
                List.map (wrapEvent_ wrapper) ls

        Protected g ->
            Protected <|
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

        Async ls ->
            Async <|
                List.map (mapCmd_ f) ls

        Protected g ->
            Protected <|
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

        Quit ->
            Quit


{-| Just like `Procedure.modify`.
-}
modify : Modifier m m1 -> (m1 -> m1) -> Procedure c m e1
modify mod f =
    Procedure
        [ \_ ->
            Do <|
                \m0 ->
                    Modifier.mget mod m0
                        |> Maybe.map f
                        |> Maybe.map
                            (\m1 ->
                                ( Modifier.set mod m1 m0, [] )
                            )
                        |> Maybe.withDefault
                            ( m0, [] )
        ]


{-| Just like `Procedure.push`.
-}
push : Modifier m m1 -> (( Channel, m1 ) -> cmd) -> Procedure cmd m e1
push mod f =
    Procedure
        [ \c ->
            Do <|
                \m0 ->
                    Modifier.mget mod m0
                        |> Maybe.map
                            (\m1 ->
                                ( m0, [ f ( c, m1 ) ] )
                            )
                        |> Maybe.withDefault
                            ( m0, [] )
        ]


{-| Just like `Procedure.await`.
-}
await :
    Modifier m m1
    -> (e1 -> m1 -> List (Procedure c m e1))
    -> Procedure c m e1
await mod f =
    Procedure
        [ \expected ->
            Await
                (\(Msg targetId e1) m0 ->
                    Modifier.mget mod m0
                        |> Maybe.andThen
                            (\m1 ->
                                if targetId == expected then
                                    case f e1 m1 of
                                        [] ->
                                            Nothing

                                        ps ->
                                            let
                                                (Procedure items) =
                                                    batch ps
                                            in
                                            Just <| List.map (apply expected) items

                                else
                                    Nothing
                            )
                )
        ]


apply : a -> (a -> b) -> b
apply c f =
    f c


{-| Just like `Procedure.async`.
-}
async : List (Procedure c m e) -> Procedure c m e
async ps =
    let
        (Procedure items) =
            batch ps
    in
    Procedure
        [ \c -> Async (List.map (apply c) items)
        ]


{-| Just like `Procedure.protected`.
-}
protected :
    List (Procedure c m e1)
    -> Procedure c m e1
protected ps =
    Procedure
        [ \_ ->
            Protected <|
                \priv ->
                    let
                        (Procedure items) =
                            batch ps
                    in
                    List.map (apply priv) items
        ]


{-| Just like `Procedure.sync`.
-}
sync : List (Procedure c m e) -> Procedure c m e
sync ps =
    Procedure
        [ \c ->
            Sync <|
                List.map (\(Procedure items) -> List.map (apply c) items) ps
        ]


{-| Just like `Procedure.race`.
-}
race : List (Procedure c m e) -> Procedure c m e
race ps =
    Procedure
        [ \c ->
            Race <|
                List.map (\(Procedure items) -> List.map (apply c) items) ps
        ]


{-| Just like `Procedure.quit`.
-}
quit : Procedure c m e
quit =
    Procedure [ \_ -> Quit ]


{-| Just like `Procedure.jump`.
-}
jump :
    Modifier m m1
    -> (m1 -> List (Procedure c m e1))
    -> Procedure c m e1
jump mod f =
    Procedure
        [ \c ->
            Jump <|
                \m0 ->
                    case Modifier.mget mod m0 of
                        Nothing ->
                            []

                        Just m1 ->
                            let
                                (Procedure items) =
                                    batch (f m1)
                            in
                            List.map (apply c) items
        ]


{-| Just like `Procedure.subscribe`.
-}
subscribe :
    Sub (Msg e1)
    -> List (Procedure c m e1)
    -> Procedure c m e1
subscribe sub ps =
    Procedure
        [ \c ->
            Subscribe sub <|
                \sid ->
                    let
                        (Procedure items) =
                            batch ps
                    in
                    List.map (apply c) items ++ [ Unsubscribe sid ]
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


{-| Just like `Procedure.request`.
-}
request : (m1 -> (a -> Msg e1) -> cmd1) -> Modifier m m1 -> Request cmd m e1 cmd1 a
request f mod toCmd toEvent =
    push mod <|
        \( c, m1 ) ->
            toCmd <| f m1 (toEvent >> publish c)


{-| Just like `Procedure.Request`.
-}
type alias Request cmd m e cmd1 a =
    (cmd1 -> cmd) -> (a -> e) -> Procedure cmd m e



-- Observing


{-| Just like `Procedure.observe`.
-}
observe :
    r
    -> (( Channel, r ) -> List (Procedure c m e1))
    -> Procedure c m e1
observe r f =
    Procedure
        [ \_ ->
            Protected <|
                \priv ->
                    let
                        (Procedure ps) =
                            f ( priv, r ) |> batch
                    in
                    List.map (apply priv) ps
        ]


{-| Just like `Procedure.observeList`.
-}
observeList :
    List r
    -> (List ( Channel, r ) -> List (Procedure c m e1))
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
        { newState : ThreadState memory event

        -- Side effects caused by the evaluation.
        , cmds : List cmd

        -- New thread to evaluate next time.
        , next : Msg event -> ThreadState memory event -> Thread cmd memory event
        }


type alias Thread cmd memory event =
    Model cmd memory event


{-| Just like `Procedure.elementView`.
-}
elementView : (memory -> Html (Msg event)) -> Model cmd memory event -> Html (Msg event)
elementView f (Thread { newState }) =
    f newState.memory


{-| Just like `Procedure.documentView`.
-}
documentView : (memory -> Document (Msg event)) -> Model cmd memory event -> Document (Msg event)
documentView f (Thread { newState }) =
    f newState.memory


{-| Just like `Procedure.update`.
-}
update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg (Thread t) =
    let
        (Thread t2) =
            t.next msg t.newState
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
                    , nextChannel = Channel.inc Channel.init
                    , subs = []
                    , nextSubId = SubId.init
                    }
                    (List.map (\f -> f Channel.init) items)
    in
    ( Thread t, t.cmds )


{-| State to evaluate a thread.
-}
type alias ThreadState memory event =
    { memory : memory
    , nextChannel : Channel
    , subs : List ( SubId, Sub (Msg event) )
    , nextSubId : SubId
    }


type alias SubId =
    SubId.SubId


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

        (Sync ps) :: ps2 ->
            fromProcDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

        (Race ps) :: ps2 ->
            fromProcRaceDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

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
type Msg event
    = Msg Channel event


{-| Same as `Procedure.mapMsg`.
-}
mapMsg : (a -> b) -> Msg a -> Msg b
mapMsg f (Msg id a) =
    Msg id (f a)
