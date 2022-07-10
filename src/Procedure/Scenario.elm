module Procedure.Scenario exposing
    ( Scenario
    , batch
    , none
    , userComment
    , systemComment
    , runTest
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
    , assertJust
    )

{-| Module for Scenario-Driven Development.

# Scenario

@docs Scenario
@docs batch
@docs none
@docs userComment
@docs systemComment
@docs runTest
@docs toHtml


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

@docs assertJust


-}


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



{-| Batch `Scenario`s together. The elements are evaluated in order.
-}
batch : List (Scenario c m e) -> Scenario c m e
batch =
    List.concatMap (\(Scenario items) -> items)
        >> Scenario


{-| -}
none : Scenario c m e
none =
    Scenario []


{-| -}
runTest : List (Scenario c m e) -> Test
runTest = Debug.todo ""

{-| -}
toHtml : List (Scenario c m e) -> Html msg
toHtml = Debug.todo ""


-- Scenario Constructors

{-|
-}
userComment : String -> Scenario c m e
userComment str =
    Scenario
        [ UserComment str
        ]


{-|
-}
systemComment : String -> Scenario c m e
systemComment str =
    Scenario
        [ SystemComment str
        ]


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
assertJust : String -> Maybe a -> (a -> List (Scenario c m e)) -> Scenario c m e
assertJust str ma f =
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
