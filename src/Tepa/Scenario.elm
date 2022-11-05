module Tepa.Scenario exposing
    ( Scenario
    , none
    , concat
    , toTest
    , toHtml
    , Section
    , section
    , cases
    , User
    , defineUser
    , Session
    , defineSession
    , userComment
    , systemComment
    , expectMemory
    , expectAppView
    , loadApp
    , Route
    , userEvent
    , listenerEvent
    , portResponse
    , customResponse
    , TargetLayer
    , targetOnSelf
    , targetOnChild
    , andOnChild
    , fromJust
    -- , toMarkdown
    )

{-| Module for Scenario-Driven Development.


# Core

@docs Scenario
@docs none
@docs concat
@docs toTest
@docs toHtml


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


# TargetLayer

@docs TargetLayer
@docs targetOnSelf
@docs targetOnChild
@docs andOnChild


# Conditions

@docs fromJust

-}

import Dict
import Expect exposing (Expectation)
import Expect.Builder as ExpBuilder
import Internal.Core as Core
    exposing
        ( Key(..)
        , Layer(..)
        , Model(..)
        , Msg(..)
        , Pointer(..)
        , Promise
        , Void
        )
import Internal.Markup as Markup
import Json.Encode exposing (Value)
import Mixin
import Mixin.Html as Html exposing (Html)
import Test exposing (Test)
import Test.Sequence as SeqTest
import Url exposing (Url)


type alias ExpBuilder a =
    ExpBuilder.Builder a



-- Scenario


{-| Scenario describes how the application reacts to the user operations along the time line.

The Scenario you built can be converted to tests with `toTest`, and to documents with `toHtml` or `toMarkdown`.

-}
type alias Scenario flags cmd memory event =
    Core.Scenario flags cmd memory event


{-| A Scenario that does nothing.
-}
none : Scenario flags c m e
none =
    Core.noneScenario


{-| Return a new Scenario that evaluates given Scenarios sequentially.
-}
concat : List (Scenario flags c m e) -> Scenario flags c m e
concat =
    Core.concatScenario



-- Section


{-| Titled Sequence of Scenarios.
-}
type alias Section flags command memory event =
    Core.Section flags command memory event


{-| Constructor for `Section`.

It takes Section title and its sequence of Scenarios.

-}
section : String -> List (Scenario flags c m e) -> Section flags c m e
section =
    Core.section


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
cases =
    Core.cases


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
        , uniqueName : String
        }


{-| Define a session for your Scenario.
-}
defineSession :
    { uniqueName : String
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
userComment (User user) comment =
    Core.Scenario
        { test = Core.noneTest
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph user.name comment
        }


listItemParagraph : String -> String -> Markup.BlockElement
listItemParagraph name content =
    Markup.Paragraph
        [ ( Mixin.none
          , Markup.StrongText name
          )
        , ( Mixin.none
          , Markup.PlainText <|
                ": "
                    ++ content
          )
        ]


{-| System comment.

This Scenario only affects document generation, and is ignored for scenario test generation.

-}
systemComment : Session -> String -> Scenario flags c m e
systemComment (Session session) comment =
    Core.Scenario
        { test = Core.noneTest
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    comment
        }



-- -- Expectations


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
        { target : TargetLayer m m1
        , expectation : ExpBuilder m1
        }
    -> Scenario flags c m e
expectMemory (Session session) description o =
    Core.Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "expectMemory: The application is not active on the session. Use `loadApp` beforehand."

                    Just ( model, _ ) ->
                        case extractTarget o.target model of
                            Nothing ->
                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                    \_ ->
                                        Expect.fail
                                            "expectMemory: The Layer is not accessible."

                            Just (Core.Layer _ m1) ->
                                SeqTest.pass m1
                                    |> SeqTest.assert description
                                        (ExpBuilder.applyTo o.expectation)
                                    |> SeqTest.map (\_ -> Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    description
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
expectAppView (Session session) description { expectation } =
    Core.Scenario
        { test =
            \config context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "expectAppView: The application is not active on the session. Use `loadApp` beforehand."

                    Just ( model, _ ) ->
                        SeqTest.pass
                            (Core.memoryState model
                                |> config.view
                            )
                            |> SeqTest.assert description expectation
                            |> SeqTest.map (\_ -> Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    description
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
loadApp (Session session) description o =
    let
        url =
            { protocol = Url.Http
            , host = "example.com"
            , port_ = Nothing
            , path = o.route.path
            , query = o.route.query
            , fragment = o.route.fragment
            }
    in
    Core.Scenario
        { test =
            \config context ->
                Dict.insert session.uniqueName
                    (config.init o.flags url)
                    context
                    |> Core.OnGoingTest
                    |> SeqTest.pass
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] System")
                    description
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
        { target : TargetLayer m m1
        , event : event
        }
    -> Scenario flags c m event
userEvent (Session session) description o =
    let
        (User user) =
            session.user
    in
    Core.Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "userEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just ( model, _ ) ->
                        case extractTarget o.target model of
                            Nothing ->
                                SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                                    \_ ->
                                        Expect.fail
                                            "userEvent: The Layer is not accessible."

                            Just (Core.Layer lid _) ->
                                Dict.insert session.uniqueName
                                    (Core.LayerMsg
                                        { layerId = lid
                                        , event = o.event
                                        }
                                        |> applyMsg model
                                    )
                                    context
                                    |> Core.OnGoingTest
                                    |> SeqTest.pass
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] " ++ user.name)
                    description
        }


{-| -}
type TargetLayer m m1
    = TargetLayer (TargetLayer_ m m1)


type alias TargetLayer_ m m1 =
    { get : Layer m -> Maybe (Layer m1)
    }


extractTarget : TargetLayer m m1 -> Model c m event -> Maybe (Layer m1)
extractTarget (TargetLayer target) model =
    Core.layerState model
        |> Maybe.andThen target.get


{-| -}
targetOnSelf : TargetLayer m m
targetOnSelf =
    TargetLayer
        { get = Just
        }


{-| -}
targetOnChild : (m -> Maybe (Layer m1)) -> TargetLayer m m1
targetOnChild f =
    TargetLayer
        { get = \(Layer _ m) -> f m
        }


{-| -}
andOnChild : (m1 -> Maybe (Layer m2)) -> TargetLayer m m1 -> TargetLayer m m2
andOnChild f (TargetLayer target) =
    TargetLayer
        { get =
            \lm ->
                target.get lm
                    |> Maybe.andThen
                        (\(Layer _ m1) -> f m1)
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
listenerEvent (Session session) description o =
    Core.Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "listenerEvent: The application is not active on the session. Use `loadApp` beforehand."

                    Just ( model, _ ) ->
                        case model of
                            Core.OnGoing onGoing ->
                                Dict.insert session.uniqueName
                                    (onGoing.listeners
                                        |> List.filterMap
                                            (\listener ->
                                                if listener.name == o.target then
                                                    Just <|
                                                        ListenerMsg
                                                            { requestId = listener.requestId
                                                            , event = o.event
                                                            }

                                                else
                                                    Nothing
                                            )
                                        |> applyMsgs (OnGoing onGoing)
                                    )
                                    context
                                    |> Core.OnGoingTest
                                    |> SeqTest.pass

                            EndOfProcess _ ->
                                SeqTest.pass (Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "] " ++ o.target)
                    description
        }



-- -- Response Simulators


{-| Simulate response to the `Tepa.portRequest`.

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
        { response : List command -> Value
        }
    -> Scenario flags command m e
portResponse (Session session) description o =
    Core.Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "portResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just ( model, cmds ) ->
                        case model of
                            Core.OnGoing onGoing ->
                                Dict.insert session.uniqueName
                                    (o.response cmds
                                        |> (\v -> PortResponseMsg { response = v })
                                        |> applyMsg (OnGoing onGoing)
                                    )
                                    context
                                    |> Core.OnGoingTest
                                    |> SeqTest.pass

                            EndOfProcess _ ->
                                SeqTest.pass (Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "]")
                    description
        }


{-| Simulate response to the `Tepa.customRequest`.

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
        { response : List command -> Msg event
        }
    -> Scenario flags command m event
customResponse (Session session) description o =
    Core.Scenario
        { test =
            \_ context ->
                case Dict.get session.uniqueName context of
                    Nothing ->
                        SeqTest.fail ("[" ++ session.uniqueName ++ "] " ++ description) <|
                            \_ ->
                                Expect.fail
                                    "customResponse: The application is not active on the session. Use `loadApp` beforehand."

                    Just ( model, cmds ) ->
                        case model of
                            Core.OnGoing onGoing ->
                                Dict.insert session.uniqueName
                                    (o.response cmds
                                        |> applyMsg (OnGoing onGoing)
                                    )
                                    context
                                    |> Core.OnGoingTest
                                    |> SeqTest.pass

                            EndOfProcess _ ->
                                SeqTest.pass (Core.OnGoingTest context)
        , markup =
            Core.putListItemMarkup <|
                listItemParagraph
                    ("[" ++ session.uniqueName ++ "]")
                    description
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
            Core.Scenario
                { test =
                    \_ _ ->
                        SeqTest.fail description <|
                            \_ ->
                                ma
                                    |> Expect.notEqual Nothing
                , markup =
                    Core.invalidMarkup <|
                        Core.OtherInvalidMarkup <|
                            "Error: fromJust\n"
                                ++ description
                }

        Just a ->
            f a
                |> concat



-- Test


{-| Generate scenario tests.
-}
toTest :
    { init : memory
    , procedure : flags -> Url -> Key -> Promise cmd memory event Void
    , view : Layer memory -> Html (Msg event)
    , sections : List (Section flags cmd memory event)
    }
    -> Test
toTest =
    Core.toTest


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
-- {-| -}
-- toMarkdown =
--     Debug.todo ""
--


{-| Generate scenario document server.
-}
toHtml :
    { title : String
    , sections : List (Section flags c m e)
    }
    -> Html ()
toHtml o =
    case Core.toMarkup o of
        Err err ->
            unexpectedReasonHtml err

        Ok sec ->
            Html.div
                [ Mixin.style "margin" "2em"
                ]
                [ Markup.toHtml sec
                ]


unexpectedReasonHtml : Core.InvalidMarkupReason -> Html ()
unexpectedReasonHtml reason =
    Html.div []
        [ Html.text <|
            case reason of
                Core.SiblingScenarioAfterCases ->
                    "ERROR: `cases`\nNo sibling scenarios can be after `cases`."

                Core.OtherInvalidMarkup str ->
                    str
        ]



-- toMarkup : List (Section flags c m e) -> Result UnexpectedScenario Markup.Section
-- toMarkup sections =
--     List.foldr
--         (\sec acc ->
--             Result.map2 (++)
--                 (markupSection [] sec)
--                 acc
--         )
--         (Ok [])
--         sections
--         |> Result.map Markup.Sections
--
--
-- markupSection : List ( Mixin (), Markup.BlockElement ) -> Section flags c m e -> Result UnexpectedScenario (List ( Mixin (), String, Markup.Section ))
-- markupSection inherit (Section r) =
--     let
--         context =
--             markupScenario_ r.title
--                 r.content
--                 { appendSections =
--                     \items ->
--                         [ ( Mixin.none, r.title, Markup.SectionBody [ ( Mixin.none, Markup.ListItems (Mixin.style "list-style-type" "disc") items ) ] ) ]
--                 , listItems = inherit
--                 , error = Nothing
--                 , nextSessionId = 1
--                 }
--     in
--     case context.error of
--         Nothing ->
--             context.listItems
--                 |> List.reverse
--                 |> context.appendSections
--                 |> Ok
--
--         Just err ->
--             Err err
--
--
-- type alias MarkupContext =
--     { appendSections : List ( Mixin (), Markup.BlockElement ) -> List ( Mixin (), String, Markup.Section )
--     , listItems : List ( Mixin (), Markup.BlockElement )
--     , error : Maybe UnexpectedScenario
--     , nextSessionId : Int
--     }
--
--
-- markupScenario_ : String -> Scenario flags c m e -> MarkupContext -> MarkupContext
-- markupScenario_ title scenario context =
--     let
--         appendListItem : String -> String -> MarkupContext
--         appendListItem name content =
--             { context
--                 | listItems =
--                     ( Mixin.none
--                     , Markup.Paragraph
--                         [ ( Mixin.none
--                           , Markup.StrongText name
--                           )
--                         , ( Mixin.none
--                           , Markup.PlainText <|
--                                 ": "
--                                     ++ content
--                           )
--                         ]
--                     )
--                         :: context.listItems
--             }
--     in
--     case scenario of
--         UserComment r ->
--             let
--                 (User user) =
--                     r.user
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem user.name r.comment)
--
--         SystemComment r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.comment
--                 )
--
--         LoadApp r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         UserEvent r ->
--             let
--                 (Session session) =
--                     r.session
--
--                 (User user) =
--                     session.user
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ user.name)
--                     r.description
--                 )
--
--         ListenerEvent r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ r.target)
--                     r.description
--                 )
--
--         ExpectCommands r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         ExpectMemory r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         ExpectAppView r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] System")
--                     r.description
--                 )
--
--         PortResponse r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ r.target)
--                     r.description
--                 )
--
--         CustomResponse r ->
--             let
--                 (Session session) =
--                     r.session
--             in
--             markupScenario_ title
--                 r.next
--                 (appendListItem
--                     ("[" ++ session.name ++ "] " ++ r.target)
--                     r.description
--                 )
--
--         NextCases r ->
--             let
--                 rnextCases : Result UnexpectedScenario (List ( Mixin (), String, Markup.Section ))
--                 rnextCases =
--                     List.foldr
--                         (\sec acc ->
--                             Result.map2 (++)
--                                 (markupSection
--                                     [ ( Mixin.none
--                                       , Markup.Paragraph
--                                             [ ( Mixin.none
--                                               , Markup.PlainText "(After "
--                                               )
--                                             , ( Mixin.none
--                                               , Markup.EmphasizedText title
--                                               )
--                                             , ( Mixin.none
--                                               , Markup.PlainText ")"
--                                               )
--                                             ]
--                                       )
--                                     ]
--                                     sec
--                                 )
--                                 acc
--                         )
--                         (Ok [])
--                         r.cases
--             in
--             case rnextCases of
--                 Err err ->
--                     { context | error = Just err }
--
--                 Ok nextCases ->
--                     { context
--                         | appendSections =
--                             \_ ->
--                                 (context.listItems
--                                     |> List.reverse
--                                     |> context.appendSections
--                                 )
--                                     ++ nextCases
--                         , listItems = []
--                     }
--
--         Unexpected r ->
--             { context | error = Just r.reason }
--
--         Nil ->
--             context
--
--
-- {-| -}
-- toMarkdown :
--     { title : String
--     , sections : List (Section flags c m e)
--     }
--     -> String
-- toMarkdown _ =
--     "todo"
