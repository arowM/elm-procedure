module Procedure.Scenario exposing
    ( .. )

{-| Module for Scenario-Driven Development.

# Entry point

@docs toTest
@docs Key
@docs toHtml

# Scenario

@docs Scenario
@docs none
@docs batch

# Section

@docs Section
@docs section
@docs cases

# Session

@docs Session
@docs withNewSession

# Primitives

## Comments

@docs userComment
@docs systemComment

## Expectations

@docs expectCommand
@docs expectMemory
@docs expectView

## Event Simulators

@docs loadApp
@docs Route
@docs userEvent
@docs listenerEvent

## Request Simulators

@docs portRequest
@docs customRequest

# Conditions

@docs fromJust
@docs onLayer

-}

import Browser.Navigation as Nav
import Dict
import Expect exposing (Expectation)
import Expect.Builder as ExpBuilder
import Mixin.Html as Html exposing (Html)
import Internal.Channel as Channel exposing (Channel)
import Internal.Core as Core exposing
    ( Model(..)
    , Msg(..)
    , Context
    , Pointer
    )
import Internal.Markup as Markup
import Mixin exposing (Mixin)
import Test exposing (Test)



type alias ExpBuilder a = ExpBuilder.Builder a

-- Scenario


{-| -}
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
        , event : event
        , next : Scenario flags cmd memory event
        }
    | ListenerEvent
        { session : Session
        , description : String
        , target : String
        , event : event
        , next : Scenario flags cmd memory event
        }
    | ExpectCommand
        { session : Session
        , description : String
        , expectation : ExpBuilder cmd
        , next : Scenario flags cmd memory event
        }
    | ExpectMemory
        { session : Session
        , description : String
        , expectation : ExpBuilder memory
        , next : Scenario flags cmd memory event
        }
    | ExpectView
        { session : Session
        , description : String
        , expectation : Html () -> Expectation
        , next : Scenario flags cmd memory event
        }
    | PortRequest
        { session : Session
        , description : String
        , name : String
        , expectation : ExpBuilder cmd
        , next : Value -> Scenario flags cmd memory event
        }
    | CustomRequest
        { session : Session
        , description : String
        , name : String
        , expectation : ExpBuilder cmd
        , next : event -> Scenario flags cmd memory event
        }
    | WithListener
        { session : Session
        , name : String
        , next : RequestId -> Scenario flags cmd memory event
        }
    | NextCases
        { cases : List (Section cmd memory event)
        }
    | Unexpected
        { reason : UnexpectedScenario
        }
    | Nil



type UnexpectedScenario
    = IsNotJust String
    | ScenarioAfterNextCases

{-| -}
batch : List (Scenario flags c m e) -> Scenario flags c m e
batch =
    List.foldl (\a acc -> concat acc a) Nil


concat : Scenario flags c m e -> Scenario flags c m e -> Scenario flags c m e
concat s1 s2 =
    case s1 of
        UserComment r ->
            UserComment { r | next = concat r.next s2 }

        SystemComment r ->
            SystemComment { r | next = concat r.next s2 }

        LoadApp r ->
            LoadApp { r | next = concat r.next s2 }

        UserEvent r ->
            UserEvent { r | next = concat r.next s2 }

        ListenerEvent r ->
            ListenerEvent { r | next = concat r.next s2 }

        ExpectCommand r ->
            ExpectCommand { r | next = concat r.next s2 }

        ExpectMemory r ->
            ExpectMemory { r | next = concat r.next s2 }

        ExpectView r ->
            ExpectView { r | next = concat r.next s2 }

        PortRequest r ->
            PortRequest { r | next = \v -> concat (r.next v) s2 }

        CustomRequest r ->
            CustomRequest { r | next = \e -> concat (r.next e) s2 }

        WithListener r ->
            WithListener
                { r
                    | next = \reqId ->
                        concat (r.next reqId) s2
                }
        NextCases _ ->
            Unexpected
                { reason = ScenarioAfterNextCases
                }

        Unexpected r ->
            Unexpected r

        Nil ->
            s2


{-| -}
none : Scenario flags c m e
none =
    Nil


-- Section


{-| -}
type Section command memory event
    = Section
        { title : String
        , content : Scenario flags command memory event
        }


liftSection_ : Pointer m m1 -> Section c1 m1 e1 -> Section c m e
liftSection_ pointer (Section r) =
    Section
        { title = r.title
        , content = onLayer_ pointer r.content
        }


{-| -}
section : String -> List (Scenario flags c m e) -> Section c m e
section title scenarios =
    Section
        { title = title
        , content = batch scenarios
        }


{-| Connect to other sections.
-}
cases : List (Section c m e) -> Scenario flags c m e
cases sections =
    NextCases
        { cases = sections
        }


{-| -}
type User = User
        { name : String
        }


{-| -}
user :
    { name : String
    }
    -> User
user = User

{-| -}
type Session = Session
    { user : User
    , name : String
    }


{-| -}
session :
    { name : String
    , user : User
    }
    -> Session
session = Session


-- Primitives


-- -- Comments


{-| -}
userComment : User -> String -> Scenario flags c m e
userComment user comment =
    UserComment
        { user = user
        , comment = comment
        , next = Nil
        }


{-| -}
systemComment : Session -> String -> Scenario flags c m e
systemComment session comment =
    SystemComment
        { session = session
        , comment = comment
        , next = Nil
        }


-- -- Expectations

{-| -}
expectCommand : Session -> String -> ExpBuilder command -> Scenario flags command m e
expectCommand session description expectation =
    ExpectCommand
        { session = session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| -}
expectMemory : Session -> String -> ExpBuilder memory -> Scenario flags c memory e
expectMemory session description expectation =
    ExpectMemory
        { session = session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| -}
expectView : Session -> String -> (Html () -> Expectation) -> Scenario flags c m event
expectView session description expectation =
    ExpectView
        { session = session
        , description = description
        , expectation = expectation
        , next = Nil
        }



-- -- Event Simulators


{-|
-}
type alias Route =
    { path : List String
    , query : List QueryParameter
    , fragment : Maybe String
    }


{-|
-}
loadApp :
    { session : Session
    , description : String
    , route : Route
    , flags : flags
    }
    -> Scenario flags c m e
loadApp o description =
    LoadApp
        { session = o.session
        , description = o.description
        , flags = o.flags
        , route = o.route
        , next = Nil
        }


{-| -}
userEvent :
    { session : Session
    , description : String
    , event : event
    }
    -> Scenario flags c memory event
userEvent o =
    UserEvent
        { session = o.session
        , description = o.description
        , event = o.event
        , next = Nil
        }


{-| -}
listenerEvent :
    { session : Session
    , description : String
    , target : String
    , event : event
    }
    -> Scenario flags c m event
listenerEvent o =
    ListenerEvent
        { session = o.session
        , description = o.description
        , target = o.target
        , event = o.event
        , next = Nil
        }

-- -- Request Simulators


{-|
-}
portRequest :
    { session : Session
    , description : String
    , name : String
    , expectation : ExpBuilder command
    }
    -> (Value -> Scenario flags command m e)
    -> Scenario flags command m e
portRequest o f =
    PortRequest
        { session = o.session
        , description = o.description
        , name = o.name
        , expectation = o.expectation
        , next = f
        }

customRequest :
    { session : Session
    , description : String
    , name : String
    , expectation : ExpBuilder command
    }
    -> (e -> Scenario flags command m e)
    -> Scenario flags command m e
customRequest o f =
    CustomRequest
        { session = o.session
        , description = o.description
        , name = o.name
        , expectation = o.expectation
        , next = f
        }

-- Conditions


{-| -}
fromJust : String -> Maybe a -> (a -> List (Scenario flags c m e)) -> Scenario flags c m e
fromJust description ma f =
    case ma of
        Nothing ->
            Unexpected
                { reason = IsNotJust description
                }

        Just a ->
            f a
                |> batch


{-| -}
onLayer : Pointer m m1 -> List (Scenario flags c m1 e) -> Scenario flags c m e
onLayer pointer s1s =
    onLayer_ pointer (batch s1s)


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
                { target = r.target
                , description = r.description
                , flags = r.flags
                , url = r.url
                , next = onLayer_ pointer r.next
                }
        UserEvent r ->
            UserEvent
                { session = r.session
                , description = r.description
                , event = r.event
                , next = onLayer_ pointer r.next
                }
        ListenerEvent r ->
            UserEvent
                { session = r.session
                , description = r.description
                , target = r.target
                , event = r.event
                , next = onLayer_ pointer r.next
                }

        ExpectCommand r ->
            ExpectCommand
                { target = r.target
                , description = r.description
                , expectation = r.expectation
                , next = onLayer_ pointer r.next
                }

        ExpectMemory r ->
            ExpectMemory
                { target = r.target
                , description = r.description
                , expectation =
                    ExpBuilder.custom <|
                        \c ->
                            case pointer.get c of
                                Just (_, c1) ->
                                    ExpBuilder.extractOn c1 r.expectation
                                _ ->
                                    ExpBuilder.fail "Unexpected memory state."
                , next = onLayer_ pointer r.next
                }
        ExpectView r ->
            ExpectView
                { target = r.target
                , description = r.description
                , expectation = r.expectation
                , next = onLayer_ pointer r.next
                }
        PortRequest r ->
            PortRequest
                { session = r.session
                , description = r.description
                , name = r.name
                , expectation = r.expectation
                , next = onLayer_ pointer r.next
                }
        CustomRequest r ->
            CustomRequest
                { session = r.session
                , description = r.description
                , name = r.name
                , expectation = r.expectation
                , next = onLayer_ pointer << r.next
                }
        WithListener r ->
            WithListener
                { session = r.session
                , name = r.name
                , next = onLayer_ pointer << r.next
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


{-| -}
type Key
    = NavKey Nav.Key
    | SimKey


{-| -}
toTest :
    { init : memory
    , procedures : flags -> Url -> Key -> List (Procedure cmd memory event)
    , view : (Channel, memory) -> Html (Msg event)
    , sections : List (Section cmd memory event)
    }
    -> Test
toTest o =
    List.map
        (\sec ->
            Test.describe sec.title <|
                toTests
                    { view = \m -> Html.map (\_ -> ()) <| o.view (Channel.init, m)
                    , init = \flags url ->
                        Procedure.init o.init
                            (procedures flags url SimKey)
                    }
                    sec.content
                    Dict.empty
        )
        o.sections
        |> Test.describe "Scenario tests"


type alias TestConfig flags c m e =
    { view : m -> Html ()
    , init : flags -> Url -> Model c m e
    }

toTests : TestConfig flags c m e -> Scenario flags c m e -> Dict String (Model c m e, List cmd) -> List Test
toTests config scenario sessions =
    case scenario of
        UserComment r ->
            toTests config r.next (model, cmds)

        SystemComment r ->
            toTests config r.next (model, cmds)

        LoadApp r ->
            let
                (Session session) = r.session
            in
            Dict.insert session.name
                (config.init r.flag r.url, [])
                |> toTests config r.next

        UserEvent r ->
            let
                (Session session) = r.session
            in
            case Dict.get session.name sessions of
                Nothing ->
                    [ Test.test
                        ("[" ++ session.name ++ "] " ++ r.description)
                        <| \_ -> Expect.fail
                                "The application has not been loaded. Use `loadApp` beforehand."
                    ]

                Just (model, _) ->
                    Dict.insert session.name
                        ( ChannelMsg
                            { channel =
                            , event = r.event
                            }
                            |> applyMsg model
                        )
                        sessions
                        |> toTests config r.next

        ListenerEvent r ->
            let
                (Session session) = r.session
            in
            case Dict.get session.name sessions of
                Nothing ->
                    [ Test.test
                        ("[" ++ session.name ++ "] " ++ r.description)
                        <| \_ -> Expect.fail
                                "The application has not been loaded. Use `loadApp` beforehand."
                    ]
                Just (OnGoing onGoing, _) ->
                    Dict.insert session.name
                        ( onGoing.listeners
                            |> List.filterMap
                                (\listener ->
                                    if (listener.name == r.target) then
                                        Just <|
                                            ListenerMsg
                                                { requestId = listner.requestId
                                                , event = r.event
                                                }
                                    else
                                        Nothing
                                )
                            |> applyMsgs (OnGoing onGoing)
                        )
                        sessions
                        |> toTests config r.next
                Just (EndOfProcess, _) ->
                    toTests config r.next sessions



TODO
TODO
TODO
TODO


        ExpectCommand r ->
        { target : Target
        , description : String
        , expectation : ExpBuilder cmd
        , next : Scenario flags cmd memory event
        }
    | ExpectMemory
        { target : Target
        , description : String
        , expectation : ExpBuilder memory
        , next : Scenario flags cmd memory event
        }
    | ExpectView
        { target : Target
        , description : String
        , expectation : Html () -> Expectation
        , next : Scenario flags cmd memory event
        }
    | NextCases
        { cases : List (Section cmd memory event)
        }
    | Unexpected
        { reason : UnexpectedScenario
        }
    | Nil


applyMsg : Model c m e -> Msg e -> (Model c m e, List c)
applyMsg model msg =
    Core.update msg model


applyMsgs : Model c m e -> List (Msg e) -> (Model c m e, List c)
applyMsgs model msgs =
    List.foldl
        (\msg (model, cmds) ->
            let
                (newModel, newCmds) = Core.update msg model
            in
            ( newModel, cmds ++ newCmds)
        )
        (model, [])
        msgs


-- Document generation

{-| -}
toHtml :
    { title : String
    , sections : List (Section c m e)
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
                [ Markup.toHtml (Mixin.none, title, sec )
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

toMarkup : List (Section c m e) -> Result UnexpectedScenario Markup.Section
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

markupSection : List (Mixin(), Markup.BlockElement) -> Section c m e -> Result UnexpectedScenario (List (Mixin (), String, Markup.Section))
markupSection inherit (Section r) =
    let
        context =
            markupScenario_ r.title r.content
                { appendSections = \items ->
                    [ (Mixin.none, r.title, Markup.SectionBody [ (Mixin.none, Markup.ListItems (Mixin.style "list-style-type" "disc") items) ]) ]
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
    { appendSections : List (Mixin (), Markup.BlockElement) -> List (Mixin (), String, Markup.Section)
    , listItems : List (Mixin (), Markup.BlockElement)
    , error : Maybe UnexpectedScenario
    , nextSessionId : Int
    }


markupScenario_ : String -> Scenario flags c m e -> MarkupContext -> MarkupContext
markupScenario_ title scenario context =
    let
        appendListItem : Target -> String -> MarkupContext
        appendListItem target content = Debug.log "context"
            { context
                | listItems =
                        ( Mixin.none
                        , Markup.Paragraph
                          [ ( Mixin.none
                            , Markup.StrongText <|
                                displayTarget target
                            )
                          , ( Mixin.none
                            , Markup.PlainText <|
                                ": " ++ content
                            )
                          ]
                        ) :: context.listItems
            }

    in
    case scenario of
        Comment r ->
            markupScenario_ title
                r.next
                (appendListItem r.target r.comment)
        LoadApp r ->
            markupScenario_ title
                r.next
                (appendListItem r.target r.description)
        IssueEvent r ->
            markupScenario_ title
                r.next
                (appendListItem r.target r.description)
        ExpectCommand r ->
            markupScenario_ title
                r.next
                (appendListItem r.target r.description)
        ExpectMemory r ->
            markupScenario_ title
                r.next
                (appendListItem r.target r.description)
        ExpectView r ->
            markupScenario_ title
                r.next
                (appendListItem r.target r.description)
        NextCases r ->
            let
                rnextCases : Result UnexpectedScenario (List (Mixin (), String, Markup.Section))
                rnextCases =
                    List.foldr
                        (\sec acc ->
                            Result.map2 (++)
                                (markupSection
                                    [ (Mixin.none
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
                        | appendSections = \_ ->
                            (context.listItems
                                |> List.reverse
                                |> context.appendSections
                            ) ++ nextCases
                        , listItems = []
                    }

        Unexpected r ->
            { context | error = Just r.reason }
        Nil ->
            context
