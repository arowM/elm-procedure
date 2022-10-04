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
    , systemMemory
    , systemView
    , externalComment
    , externalEvent
    , toTest
    , toHtml
    , fromJust
    )

{-| Module for Scenario-Driven Development.

# Entry point

@docs toTest
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

@docs userComment
@docs userEvent
@docs systemComment
@docs systemCommand
@docs systemMemory
@docs systemView
@docs externalComment
@docs externalEvent

# Conditions

@docs fromJust
@docs onPage
@docs Page

-}

import Expect exposing (Expectation)
import Expect.Builder as ExpBuilder
import Mixin.Html as Html exposing (Html)
import Internal.Core exposing
    ( Model(..)
    -- , ThreadState
    , Msg(..)
    )
import Internal.Markup as Markup
import Mixin exposing (Mixin)
import Test exposing (Test)



type alias ExpBuilder a = ExpBuilder.Builder a

-- Scenario


{-| -}
type Scenario cmd memory event
    = Comment
        { target : Target
        , comment : String
        , next : Scenario cmd memory event
        }
    | IssueEvent
        { target : Target
        , description : String
        , event : event
        , next : Scenario cmd memory event
        }
    | ExpectCommand
        { target : Target
        , description : String
        , expectation : ExpBuilder cmd
        , next : Scenario cmd memory event
        }
    | ExpectMemory
        { target : Target
        , description : String
        , expectation : ExpBuilder memory
        , next : Scenario cmd memory event
        }
    | ExpectView
        { target : Target
        , description : String
        , expectation : Html () -> Expectation
        , next : Scenario cmd memory event
        }
    | WithNewSession
        { next : Session -> Scenario cmd memory event
        }
    | NextCases
        { cases : List (Section cmd memory event)
        }
    | Unexpected
        { reason : UnexpectedReason
        }
    | Nil


type UnexpectedReason
    = IsNotJust String
    | ScenarioAfterNextCases

type Target
    = UserTarget Session
    | SystemTarget Session
    | ExternalTarget Session String


displayTarget : Target -> String
displayTarget target =
    case target of
        UserTarget (Session session) ->
            "[Session " ++ String.fromInt session.sessionId ++ "] User"
        SystemTarget (Session session) ->
            "[Session " ++ String.fromInt session.sessionId ++ "] System"
        ExternalTarget (Session session) name ->
            "[Session " ++ String.fromInt session.sessionId ++ "] " ++ name


{-| -}
batch : List (Scenario c m e) -> Scenario c m e
batch =
    List.foldl (\a acc -> concat acc a) Nil


concat : Scenario c m e -> Scenario c m e -> Scenario c m e
concat s1 s2 =
    case s1 of
        Comment r ->
            Comment { r | next = concat r.next s2 }

        IssueEvent r ->
            IssueEvent { r | next = concat r.next s2 }

        ExpectCommand r ->
            ExpectCommand { r | next = concat r.next s2 }

        ExpectMemory r ->
            ExpectMemory { r | next = concat r.next s2 }

        ExpectView r ->
            ExpectView { r | next = concat r.next s2 }

        WithNewSession r ->
            WithNewSession
                { r
                    | next = \session ->
                        concat (r.next session) s2
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
none : Scenario c m e
none =
    Nil


-- Section


{-| -}
type Section command memory event
    = Section
        { title : String
        , content : Scenario command memory event
        }


liftSection_ : Page c m e c1 m1 e1 -> Section c1 m1 e1 -> Section c m e
liftSection_ page (Section r) =
    Section
        { title = r.title
        , content = onPage_ page r.content
        }


{-| -}
section : String -> List (Scenario c m e) -> Section c m e
section title scenarios =
    Section
        { title = title
        , content = batch scenarios
        }


{-| Connect to other sections.
-}
cases : List (Section c m e) -> Scenario c m e
cases sections =
    NextCases
        { cases = sections
        }


{-| -}
type Session
    = Session Session_


type alias Session_ =
    { sessionId : Int
    }


-- Primitives


{-| -}
userComment : Session -> String -> Scenario c m e
userComment session comment =
    Comment
        { target = UserTarget session
        , comment = comment
        , next = Nil
        }


{-| -}
systemComment : Session -> String -> Scenario c m e
systemComment session comment =
    Comment
        { target = SystemTarget session
        , comment = comment
        , next = Nil
        }


{-| -}
externalComment : String -> Session -> String -> Scenario c m e
externalComment name session comment =
    Comment
        { target = ExternalTarget session name
        , comment = comment
        , next = Nil
        }


{-| -}
userEvent : Session -> String -> event -> Scenario c m event
userEvent session description event =
    IssueEvent
        { target = UserTarget session
        , description = description
        , event = event
        , next = Nil
        }


{-| -}
externalEvent : String -> Session -> String -> event -> Scenario c m event
externalEvent name session description event =
    IssueEvent
        { target = ExternalTarget session name
        , description = description
        , event = event
        , next = Nil
        }


{-| -}
systemCommand : Session -> String -> ExpBuilder command -> Scenario command m e
systemCommand session description expectation =
    ExpectCommand
        { target = SystemTarget session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| -}
systemMemory : Session -> String -> ExpBuilder memory -> Scenario c memory e
systemMemory session description expectation =
    ExpectMemory
        { target = SystemTarget session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| -}
systemView : Session -> String -> (Html () -> Expectation) -> Scenario c m event
systemView session description expectation =
    ExpectView
        { target = SystemTarget session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| -}
withNewSession : (Session -> List (Scenario c m e)) -> Scenario c m e
withNewSession f =
    WithNewSession
        { next = \session -> f session |> batch
        }


-- Conditions


{-| -}
fromJust : String -> Maybe a -> (a -> List (Scenario c m e)) -> Scenario c m e
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
type alias Page c m e c1 m1 e1 =
    { unwrapCommand : c -> Maybe c1
    , get : m -> Maybe m1
    , wrapEvent : e1 -> e
    }


{-| -}
onPage : Page c m e c1 m1 e1 -> List (Scenario c1 m1 e1) -> Scenario c m e
onPage page s1s =
    onPage_ page (batch s1s)


onPage_ : Page c m e c1 m1 e1 -> Scenario c1 m1 e1 -> Scenario c m e
onPage_ page s =
    case s of
        Comment r ->
            Comment
                { target = r.target
                , comment = r.comment
                , next = onPage_ page r.next
                }
        IssueEvent r ->
            IssueEvent
                { target = r.target
                , description = r.description
                , event = page.wrapEvent r.event
                , next = onPage_ page r.next
                }

        ExpectCommand r ->
            ExpectCommand
                { target = r.target
                , description = r.description
                , expectation =
                    ExpBuilder.custom <|
                        \c ->
                            case page.unwrapCommand c of
                                Just c1 ->
                                    ExpBuilder.extractOn c1 r.expectation
                                _ ->
                                    ExpBuilder.fail "Unexpected command."
                , next = onPage_ page r.next
                }

        ExpectMemory r ->
            ExpectMemory
                { target = r.target
                , description = r.description
                , expectation =
                    ExpBuilder.custom <|
                        \c ->
                            case page.get c of
                                Just c1 ->
                                    ExpBuilder.extractOn c1 r.expectation
                                _ ->
                                    ExpBuilder.fail "Unexpected memory state."
                , next = onPage_ page r.next
                }
        ExpectView r ->
            ExpectView
                { target = r.target
                , description = r.description
                , expectation = r.expectation
                , next = onPage_ page r.next
                }
        WithNewSession r ->
            WithNewSession
                { next = \session ->
                    r.next session
                        |> onPage_ page
                }
        NextCases r ->
            NextCases
                { cases = List.map (liftSection_ page) r.cases
                }
        Unexpected r ->
            Unexpected r
        Nil ->
            Nil



-- Parse


{-| -}
toTest :
    { init : m
    , sections : List (Section c m e)
    }
    -> Test
toTest =
    Debug.todo ""


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


unexpectedReasonHtml : UnexpectedReason -> Html ()
unexpectedReasonHtml reason =
    Html.div []
        [ Html.text <|
            case reason of
                IsNotJust description ->
                    "ERROR: `fromJust`\n" ++ description
                ScenarioAfterNextCases ->
                    "ERROR: `cases`\nSome scenarios are after `cases`. You must not put any scenarios after `cases`."
        ]

toMarkup : List (Section c m e) -> Result UnexpectedReason Markup.Section
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

markupSection : List (Mixin(), Markup.BlockElement) -> Section c m e -> Result UnexpectedReason (List (Mixin (), String, Markup.Section))
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
    , error : Maybe UnexpectedReason
    , nextSessionId : Int
    }


markupScenario_ : String -> Scenario c m e -> MarkupContext -> MarkupContext
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
        WithNewSession r ->
            markupScenario_ title
                (r.next (Session { sessionId = context.nextSessionId }))
                { context | nextSessionId = context.nextSessionId + 1 }

        NextCases r ->
            let
                rnextCases : Result UnexpectedReason (List (Mixin (), String, Markup.Section))
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
