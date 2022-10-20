module Procedure.Scenario exposing
    ( Scenario
    , none
    , concat
    , toTest
    , toHtml
    , toMarkdown
    , Section
    , section
    , cases
    , User
    , defineUser
    , Session
    , defineSession
    , userComment
    , systemComment
    , expectCommands
    , expectMemory
    , expectAppView
    , loadApp
    , Route
    , userEvent
    , listenerEvent
    , portResponse
    , customResponse
    , fromJust
    , onLayer
    )

{-| Module for Scenario-Driven Development.


# Core

@docs Scenario
@docs none
@docs concat
@docs toTest
@docs toHtml
@docs toMarkdown


# Section

@docs Section
@docs section
@docs cases


# User

@docs User
@docs defineUser


# Session

@docs Session
@docs defineSession


# Primitives


## Comments

@docs userComment
@docs systemComment


## Expectations

@docs expectCommands
@docs expectMemory
@docs expectAppView


## Event Simulators

@docs loadApp
@docs Route
@docs userEvent
@docs listenerEvent


## Response Simulators

@docs portResponse
@docs customResponse


# Conditions

@docs fromJust
@docs onLayer

-}

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Expect.Builder as ExpBuilder
import Internal.Core as Core
    exposing
        ( Key(..)
        , Model(..)
        , Msg(..)
        , Pointer
        , Procedure
        )
import Internal.LayerId as LayerId exposing (LayerId)
import Internal.Markup as Markup
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Html as Html exposing (Html)
import Test exposing (Test)
import Url exposing (Url)


type alias ExpBuilder a =
    ExpBuilder.Builder a



-- Scenario


{-| Scenario describes how the application reacts to the user operations along the time line.

The Scenario you built can be converted to tests with `toTest`, and to documents with `toHtml` or `toMarkdown`.

-}
type Scenario flags cmd memory event
    = UserComment
        { user : User
        , comment : String
        , next : Scenario flags cmd memory event
        }
    | SystemComment
        { session : Session
        , comment : String
        , next : Scenario flags cmd memory event
        }
    | LoadApp
        { session : Session
        , description : String
        , flags : flags
        , route : Route
        , next : Scenario flags cmd memory event
        }
    | UserEvent
        { session : Session
        , description : String
        , event : ( LayerId, memory ) -> Maybe ( LayerId, event )
        , next : Scenario flags cmd memory event
        }
    | ListenerEvent
        { session : Session
        , description : String
        , target : String
        , event : event
        , next : Scenario flags cmd memory event
        }
    | ExpectCommands
        { session : Session
        , description : String
        , expectation : ExpBuilder (List cmd)
        , next : Scenario flags cmd memory event
        }
    | ExpectMemory
        { session : Session
        , description : String
        , expectation : ExpBuilder memory
        , next : Scenario flags cmd memory event
        }
    | ExpectAppView
        { session : Session
        , description : String
        , expectation : Html () -> Expectation
        , next : Scenario flags cmd memory event
        }
    | PortResponse
        { session : Session
        , description : String
        , target : String
        , response : Value
        , next : Scenario flags cmd memory event
        }
    | CustomResponse
        { session : Session
        , description : String
        , target : String
        , response : event
        , next : Scenario flags cmd memory event
        }
    | NextCases
        { cases : List (Section flags cmd memory event)
        }
    | Unexpected
        { reason : UnexpectedScenario
        }
    | Nil


type UnexpectedScenario
    = IsNotJust String
    | ScenarioAfterNextCases


{-| Return a new Scenario that evaluates given Scenarios sequentially.
-}
concat : List (Scenario flags c m e) -> Scenario flags c m e
concat =
    List.foldl (\a acc -> mappend acc a) Nil


mappend : Scenario flags c m e -> Scenario flags c m e -> Scenario flags c m e
mappend s1 s2 =
    case s1 of
        UserComment r ->
            UserComment { r | next = mappend r.next s2 }

        SystemComment r ->
            SystemComment { r | next = mappend r.next s2 }

        LoadApp r ->
            LoadApp { r | next = mappend r.next s2 }

        UserEvent r ->
            UserEvent { r | next = mappend r.next s2 }

        ListenerEvent r ->
            ListenerEvent { r | next = mappend r.next s2 }

        ExpectCommands r ->
            ExpectCommands { r | next = mappend r.next s2 }

        ExpectMemory r ->
            ExpectMemory { r | next = mappend r.next s2 }

        ExpectAppView r ->
            ExpectAppView { r | next = mappend r.next s2 }

        PortResponse r ->
            PortResponse { r | next = mappend r.next s2 }

        CustomResponse r ->
            CustomResponse { r | next = mappend r.next s2 }

        NextCases _ ->
            Unexpected
                { reason = ScenarioAfterNextCases
                }

        Unexpected r ->
            Unexpected r

        Nil ->
            s2


{-| A Scenario that does nothing.
-}
none : Scenario flags c m e
none =
    Nil



-- Section


{-| Titled Sequence of Scenarios.
-}
type Section flags command memory event
    = Section
        { title : String
        , content : Scenario flags command memory event
        }


liftSection_ : Pointer m m1 -> Section flags c m1 e -> Section flags c m e
liftSection_ pointer (Section r) =
    Section
        { title = r.title
        , content = onLayer_ pointer r.content
        }


{-| Constructor for `Section`.

It takes Section title and its sequence of Scenarios.

-}
section : String -> List (Scenario flags c m e) -> Section flags c m e
section title scenarios =
    Section
        { title = title
        , content = concat scenarios
        }


{-| You will want to create branches within your scenario. In such cases, you can use cases to branch into multiple scenarios.

    mySection : Section Flags Command Memory Event
    mySection =
        section "Common scenario"
            [ doSomething
            , cases
                [ section "When user chose Goat as their first pet."
                    goatScenario
                , section "When user chose Dog as their first pet."
                    dogScenario
                , section "When user chose Cat as their first pet."
                    catScenario
                ]
            ]

You cannot put Scenarios after `cases`; otherwise document generation and tests fails:

    invalidScenario : Scenario Flags Command Memory Event
    invalidScenario =
        section "Invalid scenario"
            [ doSomething
            , cases someCases
            , youCannotPutAnyScenario
            ]

-}
cases : List (Section flags c m e) -> Scenario flags c m e
cases sections =
    NextCases
        { cases = sections
        }


{-| An application user.
-}
type User
    = User
        { name : String
        }


{-| Define a user for your Scenario.
-}
defineUser :
    { name : String
    }
    -> User
defineUser =
    User


{-| Session is a unit that connects one application instance and its user. Basically, it corresponds to a tab in a browser.

So, for example, if you want to create a scenario where a user opens and operates two tabs, you need to define two separate sessions for the same user:

    sakuraChan : User
    sakuraChan =
        defineUser
            { name = "Sakura-chan"
            }

    sakuraChanMainSession : Session
    sakuraChanMainSession =
        defineSession
            { user = sakuraChan
            , name = "Main tab on the Sakura-chan's machine"
            }

    sakuraChanSecondSession : Session
    sakuraChanSecondSession =
        defineSession
            { user = sakuraChan
            , name = "Second tab on the Sakura-chan's machine"
            }

-}
type Session
    = Session
        { user : User
        , name : String
        }


{-| Define a session for your Scenario.
-}
defineSession :
    { name : String
    , user : User
    }
    -> Session
defineSession =
    Session



-- Primitives
-- -- Comments


{-| User comment.

    myScenario =
        [ userComment sakuraChan
            "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan
            "Today I'll try a goat management service."
        , Debug.todo "..."
        ]

This Scenario only affects document generation, and is ignored for scenario test generation.

You can start with `userComment` and `systemComment` to build the skeleton of your scenario, and gradually replace `userComment` with Event Simulator and `systemComment` with Expectation.

-}
userComment : User -> String -> Scenario flags c m e
userComment user comment =
    UserComment
        { user = user
        , comment = comment
        , next = Nil
        }


{-| System comment.

This Scenario only affects document generation, and is ignored for scenario test generation.

-}
systemComment : Session -> String -> Scenario flags c m e
systemComment session comment =
    SystemComment
        { session = session
        , comment = comment
        , next = Nil
        }



-- -- Expectations


{-| Describe your expectations for the commands your application issues at the point.
Only useful when the app is built with `Procedure.Advanced` package.

Suppose your application requests user information to the server on loading:

    import Expect.Builder as ExpBuilder

    myScenario =
        [ Debug.todo "Load the app"
        , expectCommands sakuraChanMainSession
            "Requests user information to the server."
            { expectation =
                ExpBuilder.oneOfListItem
                    (ExpBuilder.custom <|
                        \cmd ->
                            case cmd of
                                RequestUserInfoToServer _ ->
                                    ExpBuilder.pass

                                _ ->
                                    ExpBuilder.fail
                                        "No the expected command"
                    )
            }
        , Debug.todo "..."
        ]

You use [elm-expectation-builder]() to describe your expectation flexibly.

-}
expectCommands :
    Session
    -> String
    ->
        { expectation : ExpBuilder (List command)
        }
    -> Scenario flags command m e
expectCommands session description { expectation } =
    ExpectCommands
        { session = session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| Describe your expectations for the application memory state at the point.

Suppose your application has a counter:

    import Expect.Builder as ExpBuilder

    myScenario =
        [ Debug.todo "After some operations..."
        , expectMemory sakuraChanMainSession
            "Requests user information to the server."
            { expectation =
                ExpBuilder.partial .counter <|
                    ExpBuilder.lessThan 4
            }
        , Debug.todo "..."
        ]

You use [elm-expectation-builder]() to describe your expectation flexibly.

-}
expectMemory :
    Session
    -> String
    ->
        { expectation : ExpBuilder memory
        }
    -> Scenario flags c memory e
expectMemory session description { expectation } =
    ExpectMemory
        { session = session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| Describe your expectations for the application's view at the point.

Suppose your application has a popup:

    import Html.Attribute exposing (attribute)
    import Test.Html.Query as Query
    import Test.Html.Selector as Selector

    myScenario =
        [ Debug.todo "After some operations..."
        , expectAppView sakuraChanMainSession
            "Show popup message."
            { expectation =
                \html ->
                    Query.fromHtml html
                        |> Query.find [ Selector.id "popup" ]
                        |> Query.has
                            [ Selector.attribute
                                (attribute "aria-hidden" "false")
                            ]
            }
        , Debug.todo "..."
        ]

You use [elm-expectation-builder]() to describe your expectation flexibly.

Note that the `expectation` field takes page whole view even if you use it in `onLayer` function.

    onLayer popup
        [ expectAppView sakuraChanMainSession
            "expectation about the whole application view"
            { expectation =
                \html ->
                    Debug.todo
                        "the argument is not the partial view for the Layer, but for the whole page."
            }
        , Debug.todo "..."
        ]

-}
expectAppView :
    Session
    -> String
    ->
        { expectation : Html () -> Expectation
        }
    -> Scenario flags c m event
expectAppView session description { expectation } =
    ExpectAppView
        { session = session
        , description = description
        , expectation = expectation
        , next = Nil
        }



-- -- Event Simulators


{-| A type representing a route to access.

Each field has the same meaning as [Url](https://package.elm-lang.org/packages/elm/url/latest/Url#Url).

-}
type alias Route =
    { path : String
    , query : Maybe String
    , fragment : Maybe String
    }


{-| Load the app. You can also use `loadApp` to reload the app.

    import Json.Encode as JE

    myScenario =
        [ userComment sakuraChan
            "Hi. I'm Sakura-chan, the cutest goat girl in the world."
        , userComment sakuraChan
            "I'll open the home page..."
        , loadApp sakuraChanMainSession
            "Load the home page."
            { route =
                { path = "/"
                , query = Nothing
                , fragment = Nothing
                }
            , flags =
                JE.object []
            }
        , systemComment sakuraChanMainSession
            "Show home page."
        , userComment sakuraChan
            "Oops, I accidentally hit the F5 button..."
        , loadApp sakuraChanMainSession
            "Reload the page."
            { route =
                { path = "/"
                , query = Nothing
                , fragment = Nothing
                }
            , flags =
                JE.object []
            }
        , Debug.todo "..."
        ]

-}
loadApp :
    Session
    -> String
    ->
        { route : Route
        , flags : flags
        }
    -> Scenario flags c m e
loadApp session description o =
    LoadApp
        { session = session
        , description = description
        , flags = o.flags
        , route = o.route
        , next = Nil
        }


{-| Publish an event to its Layer.

Suppose your application has a popup:

    myScenario =
        [ Debug.todo "After some operations..."
        , onLayer popup
            [ userEvent sakuraChanMainSession
                "Click cancel button."
                { event = ClickPopupCancelButton
                }
            ]
        , Debug.todo "..."
        ]

The example above publishes `ClickPopupCancelButton` event to the LayerId for the `popup` Layer.

-}
userEvent :
    Session
    -> String
    ->
        { event : event
        }
    -> Scenario flags c memory event
userEvent session description o =
    UserEvent
        { session = session
        , description = description
        , event = \( c, _ ) -> Just ( c, o.event )
        , next = Nil
        }


{-| Publish an event to a Listner.

Suppose your application has a WebSocket message Listener named "WebSocket message Listener":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After some operations..."
        , listenerEvent sakuraChanMainSession
            "Receive WebSocket message"
            { target = "WebSocket message Listener"
            , event =
                WebSocketMessage <|
                    JE.object
                        [ ( "action", JE.string "connected" )
                        ]
            }
        , Debug.todo "..."
        ]

-}
listenerEvent :
    Session
    -> String
    ->
        { target : String
        , event : event
        }
    -> Scenario flags c m event
listenerEvent session description o =
    ListenerEvent
        { session = session
        , description = description
        , target = o.target
        , event = o.event
        , next = Nil
        }



-- -- Response Simulators


{-| Simulate response to the `Procedure.portResponse`.

Suppose your application requests to access localStorage via port request named "Port to get page.account.bio":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After request to the port..."
        , portResponse sakuraChanMainSession
            "Received response."
            { target = "Port to get page.account.bio"
            , response =
                JE.string "I'm Sakura-chan."
            }
        , Debug.todo "..."
        ]

-}
portResponse :
    Session
    -> String
    ->
        { target : String
        , response : Value
        }
    -> Scenario flags command m e
portResponse session description o =
    PortResponse
        { session = session
        , description = description
        , target = o.target
        , response = o.response
        , next = Nil
        }


{-| Simulate response to the `Procedure.customResponse`.

Suppose your application requests user infomation to the backend server via custom request named "Request for user info":

    import Json.Encode as JE

    myScenario =
        [ Debug.todo "After request to the backend..."
        , portResponse sakuraChanMainSession
            "Received response."
            { target = "Request for user info"
            , response =
                UserInfoResponse <|
                    Ok
                        { name = "Sakura-chan"
                        , age = 3
                        }
            }
        , Debug.todo "..."
        ]

-}
customResponse :
    Session
    -> String
    ->
        { target : String
        , response : event
        }
    -> Scenario flags command m event
customResponse session description o =
    CustomResponse
        { session = session
        , description = description
        , target = o.target
        , response = o.response
        , next = Nil
        }



-- Conditions


{-| Extract `Just` value.

If the given value is `Nothing`, document generation and tests fails.

    import Url

    myScenario =
        [ Debug.todo "After some operations..."
        , fromJust "Make URL"
            (Url.fromString "https://example.com/foo/")
          <|
            \url ->
                [ Debug.todo "Scenarios that use `url`"
                ]
        , Debug.todo "..."
        ]

-}
fromJust : String -> Maybe a -> (a -> List (Scenario flags c m e)) -> Scenario flags c m e
fromJust description ma f =
    case ma of
        Nothing ->
            Unexpected
                { reason = IsNotJust description
                }

        Just a ->
            f a
                |> concat


{-| Run Scenarios on the specified Layer.
If the Layer is not accessible, tests fail.
-}
onLayer : Pointer m m1 -> List (Scenario flags c m1 e) -> Scenario flags c m e
onLayer pointer s1s =
    onLayer_ pointer (concat s1s)


onLayer_ : Pointer m m1 -> Scenario flags c m1 e -> Scenario flags c m e
onLayer_ pointer s =
    case s of
        UserComment r ->
            UserComment
                { user = r.user
                , comment = r.comment
                , next = onLayer_ pointer r.next
                }

        SystemComment r ->
            SystemComment
                { session = r.session
                , comment = r.comment
                , next = onLayer_ pointer r.next
                }

        LoadApp r ->
            LoadApp
                { session = r.session
                , description = r.description
                , flags = r.flags
                , route = r.route
                , next = onLayer_ pointer r.next
                }

        UserEvent r ->
            UserEvent
                { session = r.session
                , description = r.description
                , event =
                    \( _, m ) ->
                        pointer.get m
                            |> Maybe.andThen
                                (\( c1, m1 ) ->
                                    r.event ( c1, m1 )
                                )
                , next = onLayer_ pointer r.next
                }

        ListenerEvent r ->
            ListenerEvent
                { session = r.session
                , description = r.description
                , target = r.target
                , event = r.event
                , next = onLayer_ pointer r.next
                }

        ExpectCommands r ->
            ExpectCommands
                { session = r.session
                , description = r.description
                , expectation = r.expectation
                , next = onLayer_ pointer r.next
                }

        ExpectMemory r ->
            ExpectMemory
                { session = r.session
                , description = r.description
                , expectation =
                    ExpBuilder.custom <|
                        \c ->
                            case pointer.get c of
                                Just ( _, c1 ) ->
                                    ExpBuilder.extractOn c1 r.expectation

                                _ ->
                                    ExpBuilder.fail "Unexpected memory state."
                , next = onLayer_ pointer r.next
                }

        ExpectAppView r ->
            ExpectAppView
                { session = r.session
                , description = r.description
                , expectation = r.expectation
                , next = onLayer_ pointer r.next
                }

        PortResponse r ->
            PortResponse
                { session = r.session
                , description = r.description
                , target = r.target
                , response = r.response
                , next = onLayer_ pointer r.next
                }

        CustomResponse r ->
            CustomResponse
                { session = r.session
                , description = r.description
                , target = r.target
                , response = r.response
                , next = onLayer_ pointer r.next
                }

        NextCases r ->
            NextCases
                { cases = List.map (liftSection_ pointer) r.cases
                }

        Unexpected r ->
            Unexpected r

        Nil ->
            Nil



-- Test


{-| Generate scenario tests.
-}
toTest :
    { init : memory
    , procedures : flags -> Url -> Key -> List (Procedure cmd memory event)
    , view : ( LayerId, memory ) -> Html (Msg event)
    , sections : List (Section flags cmd memory event)
    }
    -> Test
toTest o =
    List.map
        (\(Section sec) ->
            Test.describe sec.title <|
                toTests
                    { view = \m -> Html.map (\_ -> ()) <| o.view ( LayerId.init, m )
                    , init =
                        \flags url ->
                            Core.init o.init
                                (o.procedures flags url SimKey)
                    }
                    sec.content
                    Dict.empty
        )
        o.sections
        |> Test.describe "Scenario tests"


type alias TestConfig flags c m e =
    { view : m -> Html ()
    , init : flags -> Url -> ( Model c m e, List c )
    }


toTests : TestConfig flags c m e -> Scenario flags c m e -> Dict String ( Model c m e, List c ) -> List Test
toTests config scenario sessions =
    let
        onlyOnActiveApp : String -> String -> (( Model c m e, List c ) -> List Test) -> List Test
        onlyOnActiveApp sessionName description action =
            case Dict.get sessionName sessions of
                Nothing ->
                    [ Test.test
                        ("[" ++ sessionName ++ "] " ++ description)
                      <|
                        \_ ->
                            Expect.fail
                                "The application is not active on the session. Use `loadApp` beforehand."
                    ]

                Just pair ->
                    action pair
    in
    case scenario of
        UserComment r ->
            toTests config r.next sessions

        SystemComment r ->
            toTests config r.next sessions

        LoadApp r ->
            let
                (Session session) =
                    r.session

                url =
                    { protocol = Url.Http
                    , host = "example.com"
                    , port_ = Nothing
                    , path = r.route.path
                    , query = r.route.query
                    , fragment = r.route.fragment
                    }
            in
            Dict.insert session.name
                (config.init r.flags url)
                sessions
                |> toTests config r.next

        UserEvent r ->
            let
                (Session session) =
                    r.session
            in
            onlyOnActiveApp session.name r.description <|
                \( model, _ ) ->
                    let
                        memory =
                            case model of
                                EndOfProcess { lastState } ->
                                    lastState

                                OnGoing { context } ->
                                    context.state
                    in
                    case r.event ( LayerId.init, memory ) of
                        Nothing ->
                            [ Test.test
                                ("[" ++ session.name ++ "] " ++ r.description)
                              <|
                                \_ ->
                                    Expect.fail
                                        "The layer is not accessible."
                            ]

                        Just ( c, e ) ->
                            Dict.insert session.name
                                (LayerMsg
                                    { layerId = c
                                    , event = e
                                    }
                                    |> applyMsg model
                                )
                                sessions
                                |> toTests config r.next

        ListenerEvent r ->
            let
                (Session session) =
                    r.session
            in
            onlyOnActiveApp session.name r.description <|
                \( model, _ ) ->
                    case model of
                        OnGoing onGoing ->
                            Dict.insert session.name
                                (onGoing.listeners
                                    |> List.filterMap
                                        (\listener ->
                                            if listener.name == r.target then
                                                Just <|
                                                    ListenerMsg
                                                        { requestId = listener.requestId
                                                        , event = r.event
                                                        }

                                            else
                                                Nothing
                                        )
                                    |> applyMsgs (OnGoing onGoing)
                                )
                                sessions
                                |> toTests config r.next

                        EndOfProcess _ ->
                            toTests config r.next sessions

        ExpectCommands r ->
            let
                (Session session) =
                    r.session
            in
            onlyOnActiveApp session.name r.description <|
                \( model, cmds ) ->
                    (Test.test
                        ("[" ++ session.name ++ "] " ++ r.description)
                     <|
                        \_ ->
                            ExpBuilder.applyTo r.expectation cmds
                    )
                        :: (Dict.insert session.name
                                ( model, cmds )
                                sessions
                                |> toTests config r.next
                           )

        ExpectMemory r ->
            let
                (Session session) =
                    r.session
            in
            onlyOnActiveApp session.name r.description <|
                \( model, cmds ) ->
                    let
                        memory =
                            case model of
                                EndOfProcess { lastState } ->
                                    lastState

                                OnGoing { context } ->
                                    context.state
                    in
                    (Test.test
                        ("[" ++ session.name ++ "] " ++ r.description)
                     <|
                        \_ ->
                            ExpBuilder.applyTo r.expectation memory
                    )
                        :: (Dict.insert session.name
                                ( model, cmds )
                                sessions
                                |> toTests config r.next
                           )

        ExpectAppView r ->
            let
                (Session session) =
                    r.session
            in
            onlyOnActiveApp session.name r.description <|
                \( model, cmds ) ->
                    let
                        memory =
                            case model of
                                EndOfProcess { lastState } ->
                                    lastState

                                OnGoing { context } ->
                                    context.state
                    in
                    (Test.test
                        ("[" ++ session.name ++ "] " ++ r.description)
                     <|
                        \_ ->
                            r.expectation (config.view memory)
                    )
                        :: (Dict.insert session.name
                                ( model, cmds )
                                sessions
                                |> toTests config r.next
                           )

        PortResponse r ->
            let
                (Session session) =
                    r.session
            in
            onlyOnActiveApp session.name r.description <|
                \( model, _ ) ->
                    case model of
                        EndOfProcess _ ->
                            [ Test.test
                                ("[" ++ session.name ++ "] " ++ r.description)
                              <|
                                \_ ->
                                    Expect.fail
                                        "The port request has been already resolved."
                            ]

                        OnGoing onGoing ->
                            Dict.insert session.name
                                (onGoing.listeners
                                    |> List.filterMap
                                        (\listener ->
                                            if listener.name == r.target then
                                                Just <|
                                                    PortResponseMsg
                                                        { requestId = listener.requestId
                                                        , response = r.response
                                                        }

                                            else
                                                Nothing
                                        )
                                    |> applyMsgs (OnGoing onGoing)
                                )
                                sessions
                                |> toTests config r.next

        CustomResponse r ->
            let
                (Session session) =
                    r.session
            in
            onlyOnActiveApp session.name r.description <|
                \( model, _ ) ->
                    case model of
                        EndOfProcess _ ->
                            [ Test.test
                                ("[" ++ session.name ++ "] " ++ r.description)
                              <|
                                \_ ->
                                    Expect.fail
                                        "The request has been already resolved."
                            ]

                        OnGoing onGoing ->
                            Dict.insert session.name
                                (onGoing.listeners
                                    |> List.filterMap
                                        (\listener ->
                                            if listener.name == r.target then
                                                Just <|
                                                    ResponseMsg
                                                        { requestId = listener.requestId
                                                        , event = r.response
                                                        }

                                            else
                                                Nothing
                                        )
                                    |> applyMsgs (OnGoing onGoing)
                                )
                                sessions
                                |> toTests config r.next

        NextCases r ->
            List.map
                (\(Section sec) ->
                    Test.describe sec.title <|
                        toTests config sec.content sessions
                )
                r.cases

        Unexpected r ->
            [ Test.test "Scenario structure" <|
                \_ ->
                    Expect.fail <|
                        case r.reason of
                            IsNotJust description ->
                                "ERROR: `fromJust`\n" ++ description

                            ScenarioAfterNextCases ->
                                "ERROR: `cases`\nSome scenarios are after `cases`. You must not put any scenarios after `cases`."
            ]

        Nil ->
            []


applyMsg : Model c m e -> Msg e -> ( Model c m e, List c )
applyMsg model msg =
    Core.update msg model


applyMsgs : Model c m e -> List (Msg e) -> ( Model c m e, List c )
applyMsgs initModel msgs =
    List.foldl
        (\msg ( accModel, accCmds ) ->
            let
                ( newModel, newCmds ) =
                    Core.update msg accModel
            in
            ( newModel, accCmds ++ newCmds )
        )
        ( initModel, [] )
        msgs



-- Document generation


{-| Generate scenario document server.
-}
toHtml :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Html ()
toHtml { title, sections } =
    case toMarkup sections of
        Err err ->
            unexpectedReasonHtml err

        Ok sec ->
            Html.div
                [ Mixin.style "margin" "2em"
                ]
                [ Markup.toHtml ( Mixin.none, title, sec )
                ]


unexpectedReasonHtml : UnexpectedScenario -> Html ()
unexpectedReasonHtml reason =
    Html.div []
        [ Html.text <|
            case reason of
                IsNotJust description ->
                    "ERROR: `fromJust`\n" ++ description

                ScenarioAfterNextCases ->
                    "ERROR: `cases`\nSome scenarios are after `cases`. You must not put any scenarios after `cases`."
        ]


toMarkup : List (Section flags c m e) -> Result UnexpectedScenario Markup.Section
toMarkup sections =
    List.foldr
        (\sec acc ->
            Result.map2 (++)
                (markupSection [] sec)
                acc
        )
        (Ok [])
        sections
        |> Result.map Markup.Sections


markupSection : List ( Mixin (), Markup.BlockElement ) -> Section flags c m e -> Result UnexpectedScenario (List ( Mixin (), String, Markup.Section ))
markupSection inherit (Section r) =
    let
        context =
            markupScenario_ r.title
                r.content
                { appendSections =
                    \items ->
                        [ ( Mixin.none, r.title, Markup.SectionBody [ ( Mixin.none, Markup.ListItems (Mixin.style "list-style-type" "disc") items ) ] ) ]
                , listItems = inherit
                , error = Nothing
                , nextSessionId = 1
                }
    in
    case context.error of
        Nothing ->
            context.listItems
                |> List.reverse
                |> context.appendSections
                |> Ok

        Just err ->
            Err err


type alias MarkupContext =
    { appendSections : List ( Mixin (), Markup.BlockElement ) -> List ( Mixin (), String, Markup.Section )
    , listItems : List ( Mixin (), Markup.BlockElement )
    , error : Maybe UnexpectedScenario
    , nextSessionId : Int
    }


markupScenario_ : String -> Scenario flags c m e -> MarkupContext -> MarkupContext
markupScenario_ title scenario context =
    let
        appendListItem : String -> String -> MarkupContext
        appendListItem name content =
            { context
                | listItems =
                    ( Mixin.none
                    , Markup.Paragraph
                        [ ( Mixin.none
                          , Markup.StrongText name
                          )
                        , ( Mixin.none
                          , Markup.PlainText <|
                                ": "
                                    ++ content
                          )
                        ]
                    )
                        :: context.listItems
            }
    in
    case scenario of
        UserComment r ->
            let
                (User user) =
                    r.user
            in
            markupScenario_ title
                r.next
                (appendListItem user.name r.comment)

        SystemComment r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] System")
                    r.comment
                )

        LoadApp r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] System")
                    r.description
                )

        UserEvent r ->
            let
                (Session session) =
                    r.session

                (User user) =
                    session.user
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] " ++ user.name)
                    r.description
                )

        ListenerEvent r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] " ++ r.target)
                    r.description
                )

        ExpectCommands r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] System")
                    r.description
                )

        ExpectMemory r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] System")
                    r.description
                )

        ExpectAppView r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] System")
                    r.description
                )

        PortResponse r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] " ++ r.target)
                    r.description
                )

        CustomResponse r ->
            let
                (Session session) =
                    r.session
            in
            markupScenario_ title
                r.next
                (appendListItem
                    ("[" ++ session.name ++ "] " ++ r.target)
                    r.description
                )

        NextCases r ->
            let
                rnextCases : Result UnexpectedScenario (List ( Mixin (), String, Markup.Section ))
                rnextCases =
                    List.foldr
                        (\sec acc ->
                            Result.map2 (++)
                                (markupSection
                                    [ ( Mixin.none
                                      , Markup.Paragraph
                                            [ ( Mixin.none
                                              , Markup.PlainText "(After "
                                              )
                                            , ( Mixin.none
                                              , Markup.EmphasizedText title
                                              )
                                            , ( Mixin.none
                                              , Markup.PlainText ")"
                                              )
                                            ]
                                      )
                                    ]
                                    sec
                                )
                                acc
                        )
                        (Ok [])
                        r.cases
            in
            case rnextCases of
                Err err ->
                    { context | error = Just err }

                Ok nextCases ->
                    { context
                        | appendSections =
                            \_ ->
                                (context.listItems
                                    |> List.reverse
                                    |> context.appendSections
                                )
                                    ++ nextCases
                        , listItems = []
                    }

        Unexpected r ->
            { context | error = Just r.reason }

        Nil ->
            context


{-| -}
toMarkdown :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> String
toMarkdown _ =
    "todo"
