# elm-procedure

[![Build Status](https://app.travis-ci.com/arowM/elm-procedure.svg?branch=main)](https://app.travis-ci.com/arowM/elm-procedure)  
[Document](https://package.elm-lang.org/packages/arowM/elm-procedure/latest/)  
[Live demo](https://arowm.github.io/elm-procedure/)  
[Live demo (advanced)](https://arowm.github.io/elm-procedure/advanced.html)  

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

    , Procedure.await global <|
        \event _ ->
            case event of
                ReceiveInitialTime ( zone, time ) ->
                    [ setVariant
                        pageHomeWrapper
                        { zone = zone
                        , time = time
                        , showActionButton = False
                        }
                        pageHomeProcedures
                    ]

                _ ->
                    -- When returning empty list,
                    -- `await` awaits events again.
                    []
    ]


pageHomeProcedures : Observer Memory PageHome_ -> List (Procedure Memory Event)
pageHomeProcedures pageHome =
    [ putLog "Asynchronous process for clock..."

-- You can, of course, start and run another procedure asynchronously.

    , Procedure.async <| clockProcedures pageHome

-- The above procedure is running asynchronously,
-- so the following procedures will run concurrently without
-- waiting for them to finish.

-- By specifying `pageHome` observer,
-- you can modify the part of memory directly.
-- No need to fiddle around with the record update syntax!

    , Procedure.modify pageHome <|
        \home -> { home | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , Procedure.await pageHome <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ Procedure.modify pageHome <|
                        \home -> { home | showActionButton = False }
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


clockProcedures : Observer Memory PageHome_ -> List (Procedure Memory Event)
clockProcedures pageHome =
    [ Procedure.await global <|
        \event _ ->
            case event of
                ReceiveTick time ->
                    [ Procedure.modify pageHome <|
                        \home ->
                            { home | time = time }
                    ]

                _ ->
                    []
    , Procedure.jump global <| \_ -> clockProcedures pageHome
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
    = ReceiveTick Posix
    | ClickActionButton
    | ReceiveInitialTime ( Time.Zone, Posix )
    | WakeUp



-- View


type PageView
    = PageLoading
    | PageHome ( ObserverId, PageHome_ )


view : Memory -> Document (Msg Event)
view memory =
    case memory.page of
        PageLoading ->
            pageLoadingView

        PageHome ( oid, home ) ->
            pageHomeView oid memory.log home


pageLoadingView : Document msg
pageLoadingView = Debug.todo "See `sample/src/Main.elm`"


type alias PageHome_ =
    { time : Posix
    , zone : Time.Zone
    , showActionButton : Bool
    }


pageHomeView : ObserverId -> String -> PageHome_ -> Document (Msg Event)
pageHomeView = Debug.todo "See `sample/src/Main.elm`"



-- Subsctiption


subscriptions : Memory -> Sub (Msg Event)
subscriptions _ =
    Time.every 1000 (Procedure.publish << ReceiveTick)
```

# List Item Example

[Live demo](https://arowm.github.io/elm-procedure/advanced.html)  
[Complete source code](https://github.com/arowM/elm-procedure/tree/main/sample/src/Advanced.elm)  

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


view : Memory -> Document (Msg Event)
view memory =
    { title = "Advanced sample app"
    , body =
        [ Html.div []
            [ Html.div []
                [ Html.button
                    [ Attributes.type_ "button"
                    , Events.onClick
                        (ClickAddGoatCard
                            |> Procedure.publish
                        )
                    ]
                    [ Html.text "Add new card"
                    ]
                ]
            , Keyed.node "div" []
                (memory.cards
                    |> List.map
                        (\( oid, goatCard ) ->
                            ( ObserverId.toString oid
                            , goatCardView oid goatCard
                            )
                        )
                )
            ]
        ]
    }


goatCardView : ObserverId -> GoatCard -> Html (Msg Event)
goatCardView oid memory =
    if memory.onEditing then
        editModeGoatCardView oid memory.form

    else
        savedModeGoatCardView oid memory.saved


editModeGoatCardView : ObserverId -> Form -> Html (Msg Event)
editModeGoatCardView = Debug.todo "See `sample/src/Advanced.elm`"


savedModeGoatCardView : ObserverId -> Saved -> Html (Msg Event)
savedModeGoatCardView = Debug.todo "See `sample/src/Advanced.elm`"



-- Procedure


procedures : () -> List (Procedure Memory Event)
procedures () =
    [ Procedure.await global <|
        \event _ ->
            case event of
                ClickAddGoatCard ->
                    [ Procedure.async
                        [ Procedure.append
                            (global
                                |> Procedure.dig
                                    { get = .cards >> Just
                                    , set = \c memory -> { memory | cards = c }
                                    }
                            )
                            initGoatCard
                            goatCardProcedures
                        ]
                    ]

                _ ->
                    []
    , Procedure.jump global <| \_ -> procedures ()
    ]


{-| Procedure for each `GoatCard` item.

Each `GoatCard` is provided its own `Observer`,
so that an event in a `GoatCard` does not interfere with
a procedure for another `GoatCard`.
-}
goatCardProcedures : Observer Memory GoatCard -> List (Procedure Memory Event)
goatCardProcedures card =
    Debug.todo "See `sample/src/Advanced.elm`"
```
