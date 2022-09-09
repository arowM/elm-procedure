module Procedure.Scenario exposing
    ( Scenario
    , none
    , batch
    , Section
    , section
    , cases
    , Session
    , withNewSession
    , Page
    , onPage
    , userComment
    , userEvent
    , systemComment
    , systemCommand
    , externalComment
    , externalEvent
    , toTest
    , toHtml
    , State
    , state
    , expectState
    , Operation
    , operation
    , mapOperation
    , pushOperation
    , Effect
    , effect
    , expectEffect
    , fromJust
    )

{-| Module for Scenario-Driven Development.

# Scenario

@docs Scenario
@docs none
@docs batch
@docs Section
@docs section
@docs cases
@docs Session
@docs withNewSession
@docs Page
@docs onPage
@docs userComment
@docs userEvent
@docs systemComment
@docs systemCommand
@docs externalComment
@docs externalEvent
@docs toTest
@docs toHtml

# Session

@docs Session

# User Operation

@docs Operation
@docs operation
@docs mapOperation
@docs pushOperation


# System State

@docs State
@docs state
@docs expectState


# Side Effect

@docs Effect
@docs effect
@docs expectEffect

# Helper

@docs fromJust


-}


import Procedure.Advanced as Advanced
import Html exposing (Html)
import Test exposing (Test)
import Expect exposing (Expectation)

-- Scenario


{-|
-}
type Scenario cmd memory event
    = Scenario (List (ScenarioItem cmd memory event))


type ScenarioItem cmd memory event
    = UserComment String
    | SystemComment String
    | Unexpected Test


{-| -}
type Section c m e = Section c m e


{-| -}
section : String -> List (Scenario c m e) -> Section c m e
section = Debug.todo ""


{-| -}
onPage : Session c m e -> Page c m e c1 m1 e1 -> (Session c1 m1 e1 -> List (Scenario c1 m1 e1)) -> Scenario c m e
onPage = Debug.todo ""


{-| -}
type Session cmd memory event =
    Session ()


{-| -}
withNewSession : (Session c m e -> List (Scenario c m e)) -> Scenario c m e
withNewSession = Debug.todo ""


{-| -}
userEvent : Session c m event -> String -> event -> Scenario c m event
userEvent = Debug.todo ""


{-| -}
type alias Page c m e c1 m1 e1 =
    { command :
        { unwrap : c -> Maybe c1
        , wrap : c1 -> c
        }
    , memory :
        { unwrap : m -> Maybe m1
        , wrap : m1 -> m
        }
    , event :
        { unwrap : e -> Maybe e1
        , wrap : e1 -> e
        }
    }


{-| Batch some sections together.
-}
cases : List (Section c m e) -> Scenario c m e
cases = Debug.todo ""


batch : List (Scenario c m e) -> Scenario c m e
batch =
    List.concatMap (\(Scenario items) -> items)
        >> Scenario


{-| -}
none : Scenario c m e
none =
    Scenario []


{-| -}
toTest :
    { init : m
    , sections : List (Section c m e)
    } -> Test
toTest = Debug.todo ""


{-| -}
toHtml :
    { title : String
    , sections : List (Section c m e)
    } -> Html msg
toHtml = Debug.todo ""


-- Scenario Constructors

{-|
-}
userComment : Session c m e -> String -> Scenario c m e
userComment _ str =
    Scenario
        [ UserComment str
        ]


{-|
-}
systemComment : Session c m e -> String -> Scenario c m e
systemComment _ str =
    Scenario
        [ SystemComment str
        ]


{-|
-}
systemCommand : Session c m e -> String -> (Advanced.Channel -> c) -> Scenario c m e
systemCommand = Debug.todo ""

{-| -}
externalComment : String -> Session c m e -> String -> Scenario c m e
externalComment = Debug.todo ""


{-| -}
externalEvent : String -> Session c m e -> String -> e -> Scenario c m e
externalEvent = Debug.todo ""

-- State

{-|
-}
type State m
    = State String (m -> Expectation)

{-| `State` constructor.
-}
state : String -> (m -> Maybe err) -> State m
state str f =
    State str <| \m ->
        case f m of
            Nothing ->
                Expect.pass

            Just err ->
                Expect.ok (Err err)

{-|
-}
expectState : State m -> Scenario c m e
expectState = Debug.todo ""

-- Effect

{-|
-}
type Effect cmd
    = Effect String (cmd -> Bool)

{-|
-}
effect : String -> (cmd -> Bool) -> Effect cmd
effect = Effect

{-|
-}
expectEffect : Effect c -> Scenario c m e
expectEffect = Debug.todo ""



-- User Event

{-|
-}
type Operation event
    = Operation String event

{-|
-}
operation : String -> event -> Operation event
operation = Operation


{-|
-}
mapOperation : (e1 -> e0) -> Operation e1 -> Operation e0
mapOperation f (Operation str e) =
    Operation str (f e)


{-|
-}
pushOperation : Operation e -> Scenario c m e
pushOperation = Debug.todo ""


-- Helper

{-|
-}
fromJust : String -> Maybe a -> (a -> List (Scenario c m e)) -> Scenario c m e
fromJust str ma f =
    case ma of
        Nothing ->
            Scenario
                [ Unexpected <| Test.test str <|
                    \_ ->
                        Expect.notEqual Nothing ma
                ]

        Just a ->
            f a
                |> batch
