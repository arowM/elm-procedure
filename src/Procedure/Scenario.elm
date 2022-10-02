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

import Expect exposing (Expect)
import Expect.Builder as ExpBuilder
import Html exposing (Html)
import Internal.Core exposing
    ( Model(..)
    , ThreadState
    , Msg(..)
    )
import Procedure.Advanced exposing (Channel)
import Test exposing (Test)



type alias ExpBuilder a = ExpBuilder.ExpBuilder a

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
        , expectation : ExpBuilder (List cmd)
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
        , expectation : Html (Msg event) -> Expectation
        , next : Scenario cmd memory event
        }
    | WithNewSession
        { next : Session -> Scenario cmd memory event
        }
    | Unexpected
        { error : String
        , description : String
        }
    | Nil


type Target
    = UserTarget Session
    | SystemTarget Session
    | ExternalTarget Session String


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

        WithNewSession r ->
            WithNewSession
                { r
                    | next = \session ->
                        concat (r.next session) s2
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
cases =
    Debug.todo ""



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
systemCommand : Session -> String -> Expect.Builder.Builder command -> Scenario command m e
systemCommand session description expectation =
    ExpectCommand
        { target = SystemTarget session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| -}
systemMemory : Session -> String -> Expect.Builder.Builder memory -> Scenario c memory e
systemMemory session description expectation =
    ExpectMemory
        { target = SystemTarget session
        , description = description
        , expectation = expectation
        , next = Nil
        }


{-| -}
systemView : Session -> String -> (Html (Msg event) -> Expectation) -> Scenario c m event
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
                { error = "thought this value would be `Just`."
                , description = description
                }

        Just a ->
            f a
                |> batch


{-| -}
type alias Page c m e c1 m1 e1 =
    { wrapEvent : e1 -> e

    { unwrapCommand : c -> Maybe c1
    , get : m -> Maybe m1


        command :
        { unwrap : c -> Maybe c1
        , wrap : c1 -> c
        }
    , memory :
        { get : m -> Maybe ( Channel, m1 )
        , set : m1 -> m -> m
        }
    , event :
        { unwrap : e -> Maybe e1
        , wrap : e1 -> e
        }
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
                        \cmds ->

                    ExpBuilder.partial page.unwrapCommand
                        ( ExpBuilder.all
                            [ ExpBuilder.notEqual Nothing
                                |> ExpBuilder.onFail "Unexpected command."
                            , ExpBuilder.fromJust <| r.expectation
                            ]
                        )
                , next = onPage_ page r.next
                }
        ExpectMemory r ->
            ExpectMemory
                { target = r.target
                , description = r.description
                , expectation =
                    ExpBuilder.partial page.get
                        ( ExpBuilder.all
                            [ ExpBuilder.notEqual Nothing
                                |> ExpBuilder.onFail "Unexpected memory state."
                            , ExpBuilder.fromJust <| r.expectation
                            ]
                        )
                , next = onPage_ page r.next
                }
    | WithNewSession
        { next : Session -> Scenario cmd memory event
        }
    | Unexpected
        { error : String
        }
    | Nil



-- Parse


type alias Context =
    { nextSessionId : Int
    }


initContext : Context
initContext =
    { nextSessionId = -2147483648
    }


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
    -> Html msg
toHtml =
    Debug.todo ""



