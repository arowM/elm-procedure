# elm-procedure

[![Build Status](https://app.travis-ci.com/arowM/elm-procedure.svg?branch=main)](https://app.travis-ci.com/arowM/elm-procedure)  
[Document](https://package.elm-lang.org/packages/arowM/elm-procedure/latest/)  
[Live demo 1](https://arowm.github.io/elm-procedure/index.html)  
[Live demo 2](https://arowm.github.io/elm-procedure/list-items.html)  

![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

Extend TEA so that complex processing procedures can be written as they are.

# What is this for?

With elm-procedure, you can translate verbatim the specification of a UX-aware application into an implementation with the same look and feel.

In a UX-aware application, it is natural to write the specification in chronological order.
This is because application users make decisions about what to do next, based on their experience of their previous operations and the application's response to those operations.
However, conventional TEA is not suitable for implementing such specifications: Every time the user interacts with the screen, you have to check the model in the `update` function and try hard to analyze "what time series did the user follow" to choose the next process. A lot of bugs are introduced in this kind of transformation work. The bad news is that these bugs are about the behaviour of the application, so you have to suffer through complex and difficult UI testing.

With elm-procedure, you can solve such drawbacks of TEA. As shown in the following example, it is possible to implement time series processing as it looks. What a magical library!

# Terms

The terms referred to in this document are defined as follows:

* Procedure: Definitions of the processes that the application will perform, in order.
* Memory: State of the application, just like the Model in TEA.
* Event: Events triggered by the user or the external environment, just like the Message in TEA.
    * TEA Messages are _global_: there is no concept of destination.
    * The elm-procedure Events can be _local_: there is concept of destination _Observer_.
        * This frees you from the problem of concurrent processes interfering with each other.
* Observer: Focuses on a specific part of Memory and monitors events.

# A Quick Example

The following code is an excerpt from [`sample/src/Main.elm`](https://github.com/arowM/elm-procedure/tree/main/sample/src/Main.elm).

```elm
import Procedure exposing (Document, Msg, Observer, Procedure, Program, global)
import Procedure.ObserverId exposing (ObserverId)


main : Program () Memory Event
main =
    Procedure.document
        { init = init
        , procedures = procedures
        , view = view
        , subscriptions = subscriptions
        }


-- Procedure


procedures : () -> List (Procedure Memory Event)
procedures () =
    [ sleep 2000

-- Hey, you know?
-- In the conventional TEA, every time you do a sleep
-- operation, you're sent to another branch of `update`
-- function, where you have to check your model to know
-- "Where did I come from?".
-- What an annoying process!

-- With elm-procedure, you just put the subsequent procedure
-- right below it.

    , requestInitialTime

-- How intuitive to be able to write the result of the
-- above request right underneath it!

    , Procedure.await global Just <|
        \event _ ->
            case event of
                ReceiveInitialTime ( zone, time ) ->
                    [ setPage homeViewWrapper
                        { zone = zone
                        , time = time
                        , showActionButton = False
                        }
                        homeProcedures
                    ]

                _ ->
                    -- When returning empty list,
                    -- `await` awaits events again.
                    []
    ]


homeProcedures : Observer Memory HomeView_ -> List (Procedure Memory Event)
homeProcedures home =
    let
        awaitHomeEvent =
            Procedure.await home <|
                \event ->
                    case event of
                        HomeEvent a ->
                            Just a

                        _ ->
                            Nothing
    in
    [ putLog "Asynchronous process for clock..."

-- You can, of course, start and run another procedure asynchronously.

    , Procedure.async <| clockProcedures home

-- The above procedure is running asynchronously,
-- so the following procedures will run concurrently without
-- waiting for them to finish.

-- By specifying `pageHome` observer,
-- you can modify the part of memory directly.
-- No need to fiddle around with the record update syntax!

    , Procedure.modify home <|
        \m -> { m | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , awaitHomeEvent <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ Procedure.modify home <|
                        \m -> { m | showActionButton = False }
                    , putLog """"Action" button has pressed."""
                    ]

                _ ->
                    []

-- Sometimes you want to synchronise your processes, don't
-- you?
-- Use `sync` to make sure that all procedures are completed
-- before moving on to the subsequent procedures.

    , Procedure.sync
        [ sleepProcedures1
            |> Procedure.batch
        , sleepProcedures2
            |> Procedure.batch
        ]
    , putLog "All processes have been completed."

-- Use `race` to make sure that at least one of the
-- procedures is completed before moving on to the subsequent
-- procedures.

    , Procedure.race
        [ sleepProcedures1
            |> Procedure.batch
        , sleepProcedures2
            |> Procedure.batch
        ]
    , putLog "One of the processes has been completed."
    ]


clockProcedures : Observer Memory HomeView_ -> List (Procedure Memory Event)
clockProcedures home =
    let
        awaitHomeEvent =
            Procedure.await home <|
                \event ->
                    case event of
                        HomeEvent a ->
                            Just a

                        _ ->
                            Nothing
    in
    [ awaitHomeEvent <|
        \event _ ->
            case event of
                ReceiveTick time ->
                    [ Procedure.modify home <|
                        \m ->
                            { m | time = time }
                    ]

                _ ->
                    []
    , Procedure.jump home <| \_ -> clockProcedures home
    ]


sleepProcedures1 : List (Procedure Memory Event)
sleepProcedures1 =
    [ putLog "Sleep 5 sec."
    , sleep 5000
    , putLog "Slept 5 sec."
    ]


sleepProcedures2 : List (Procedure Memory Event)
sleepProcedures2 =
    [ putLog "Sleep 10 sec."
    , sleep 10000
    , putLog "Slept 10 sec."
    ]


-- Core


type alias Memory =
    { page : PageView
    , log : String
    }


init : Memory
init =
    { page = PageLoading
    , log = ""
    }


type Event
    = HomeEvent HomeEvent
    | ReceiveInitialTime ( Time.Zone, Posix )
    | WakeUp


{-| For `HomeView`.
-}
type HomeEvent
    = ClickActionButton
    | ReceiveTick Posix



-- View


type PageView
    = LoadingView
    | HomeView ( ObserverId, HomeView_ )


view : VPack Event Event Memory -> Document (Msg Event)
view page =
    case (VPack.memory page).pageView of
        LoadingView ->
            loadingView

        HomeView a ->
            VPack.child page HomeEvent a (homeView page)


loadingView : Document msg
loadingView = Debug.todo "See `sample/src/Main.elm`"


type alias HomeView_ =
    { time : Posix
    , zone : Time.Zone
    , showActionButton : Bool
    }


homeView :
    VPack Event Event Memory
    -> VPack Event HomeEvent HomeView_
    -> Document (Msg Event)
homeView = Debug.todo "See `sample/src/Main.elm`"



-- Subsctiption


subscriptions : VPack Event Event Memory -> Sub (Msg Event)
subscriptions page =
    case (VPack.memory page).pageView of
        LoadingView ->
            Sub.none

        HomeView a ->
            VPack.child page HomeEvent a homeSubscriptions


homeSubscriptions : VPack Event HomeEvent HomeView_ -> Sub (Msg Event)
homeSubscriptions home =
    Time.every 1000 (VPack.issue home << ReceiveTick)
```

# List Item Example

[Live demo](https://arowm.github.io/elm-procedure/list-items.html)  
[Complete source code](https://github.com/arowM/elm-procedure/tree/main/sample/src/ListItems.elm)  

```elm
type alias Memory =
    { cards : List ( ObserverId, GoatCard )
    }


{-| Local memory for a goat card, which is used to manage
a personal information for a goat.
-}
type alias GoatCard =
    { form : Form
    , saved : Saved
    , onEditing : Bool
    }


type alias Form =
    { name : String
    }


type alias Saved =
    { name : String
    }


view : VPack Event Event Memory -> Document (Msg Event)
view page =
    { title = "Advanced sample app"
    , body =
        [ Html.div
            [ style "display" "inline-block"
            , style "padding" "0.4em"
            , style "margin" "0"
            ]
            [ Html.div
                [ style "padding" "0.4em"
                ]
                [ Html.button
                    [ Attributes.type_ "button"
                    , Events.onClick
                        (ClickAddGoatCard
                            |> VPack.issue page
                        )
                    ]
                    [ Html.text "Add new card"
                    ]
                ]
            , Keyed.node "div"
                [ style "padding" "0.4em"
                ]
                ((VPack.memory page).cards
                    |> List.map
                        (\( oid, goatCard ) ->
                            ( ObserverId.toString oid
                            , VPack.child
                                page
                                GoatCardEvent
                                (oid, goatCard)
                                goatCardView
                            )
                        )
                )
            ]
        ]
    }


goatCardView : VPack Event GoatCardEvent GoatCard -> Html (Msg Event)
goatCardView goatCard =
    if (VPack.memory goatCard).onEditing then
        editModeGoatCardView goatCard

    else
        savedModeGoatCardView goatCard


editModeGoatCardView : VPack Event GoatCardEvent GoatCard -> Html (Msg Event)
editModeGoatCardView = Debug.todo "See `sample/src/ListItems.elm`"


savedModeGoatCardView : VPack Event GoatCardEvent GoatCard -> Html (Msg Event)
savedModeGoatCardView = Debug.todo "See `sample/src/ListItems.elm`"



-- Procedure


procedures : () -> List (Procedure Memory Event)
procedures () =
    [ Procedure.await global Just <|
        \event _ ->
            case event of
                ClickAddGoatCard ->
                    [ Procedure.append
                            (global
                                |> Procedure.dig
                                    { get = .cards
                                    , set = \c memory -> { memory | cards = c }
                                    }
                            )
                            initGoatCard
                            (\goatCard ->
                                [ Procedure.async <|
                                    goatCardProcedures goatCard
                                , Procedure.jump global <| \_ -> procedures ()
                                ]
                            )
                    ]

                _ ->
                    []
    ]


{-| Procedure for each `GoatCard` item.

Each `GoatCard` is provided its own `Observer`,
so that an event in a `GoatCard` does not interfere with
a procedure for another `GoatCard`.
-}
goatCardProcedures : Observer Memory GoatCard -> List (Procedure Memory Event)
goatCardProcedures card =
    Debug.todo "See `sample/src/ListItems.elm`"
```
