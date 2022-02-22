module Main exposing (Event, Memory, PageView, main)

import Html
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Procedure exposing (Document, Msg, Observer, Procedure, Program, global)
import Procedure.ObserverId exposing (ObserverId)
import Procedure.VPack as VPack exposing (VPack)
import Procedure.Wrapper exposing (Wrapper)
import Process
import Task
import Time exposing (Posix)


main : Program () Memory Event
main =
    Procedure.document
        { init = init
        , procedures = procedures
        , view = view
        , subscriptions = subscriptions
        }


type alias Memory =
    { pageView : PageView
    , log : String
    }


init : Memory
init =
    { pageView = LoadingView
    , log = ""
    }


type Event
    = HomeEvent HomeEvent
    | ReceiveInitialTime ( Time.Zone, Posix )
    | WakeUp


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
loadingView =
    { title = "Sample application -- Loading"
    , body =
        [ Html.div
            [ style "padding" "0.6em"
            , style "margin" "0"
            ]
            [ Html.text "Loading..."
            ]
        ]
    }


type alias HomeView_ =
    { time : Posix
    , zone : Time.Zone
    , showActionButton : Bool
    }


homeView :
    VPack Event Event Memory
    -> VPack Event HomeEvent HomeView_
    -> Document (Msg Event)
homeView page home =
    let
        param =
            { log = (VPack.memory page).log
            , home = VPack.memory home
            }
    in
    { title = "Sample application -- Home"
    , body =
        [ Html.div
            [ style "padding" "0.3em"
            , style "margin" "0"
            ]
            [ Html.div
                [ style "padding" "0.3em"
                , style "margin" "0"
                ]
                [ Html.span []
                    [ Html.text <| "Current time: " ++ formatTime param.home.zone param.home.time
                    ]
                ]
            , Html.div
                [ style "padding" "0.3em"
                , style "margin" "0"
                ]
                [ Html.pre
                    [ style "padding" "0.6em"
                    , style "margin" "0"
                    , style "max-width" "18em"
                    , style "height" "16em"
                    , style "overflow-y" "auto"
                    , style "border" "solid black 1px"
                    ]
                    [ Html.text param.log
                    ]
                ]
            , if param.home.showActionButton then
                Html.div
                    [ style "padding" "0.3em"
                    , style "margin" "0"
                    ]
                    [ Html.button
                        [ Attributes.type_ "button"
                        , Events.onClick (VPack.issue home ClickActionButton)
                        ]
                        [ Html.text "Action"
                        ]
                    ]

              else
                Html.text ""
            ]
        ]
    }


formatTime : Time.Zone -> Posix -> String
formatTime zone time =
    String.concat
        [ Time.toHour zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Time.toMinute zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Time.toSecond zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]



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



-- Procedure


procedures : () -> List (Procedure Memory Event)
procedures () =
    [ sleep 2000
    , requestInitialTime
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
                    -- When returning empty list, `await` awaits events again.
                    []
    ]


homeViewWrapper : Wrapper PageView ( ObserverId, HomeView_ )
homeViewWrapper =
    { wrap = HomeView
    , unwrap =
        \m ->
            case m of
                HomeView a ->
                    Just a

                _ ->
                    Nothing
    }


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
    , Procedure.async <| clockProcedures home
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
    , Procedure.sync
        [ sleepProcedures1
            |> Procedure.batch
        , sleepProcedures2
            |> Procedure.batch
        ]
    , putLog "All processes have been completed."
    , Procedure.modify home <| \m -> { m | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , awaitHomeEvent <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ Procedure.modify home <| \m -> { m | showActionButton = False }
                    , putLog """"Action" button has pressed."""
                    ]

                _ ->
                    []
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



-- -- Helper procedures


requestInitialTime : Procedure Memory Event
requestInitialTime =
    Procedure.push global <|
        \_ _ ->
            Task.map2 (\zone time -> ( zone, time )) Time.here Time.now
                |> Task.perform (Procedure.publish << ReceiveInitialTime)


putLog : String -> Procedure Memory Event
putLog log =
    Procedure.modify global <| \memory -> { memory | log = memory.log ++ log ++ "\n" }


sleep : Float -> Procedure Memory Event
sleep msec =
    Procedure.protected global <|
        \priv ->
            [ Procedure.push priv <|
                \oid _ ->
                    Process.sleep msec
                        |> Task.perform (\() -> Procedure.issue oid WakeUp)
            , Procedure.await priv Just <|
                \event _ ->
                    case event of
                        WakeUp ->
                            -- Do nothing, but do not await the next event.
                            [ Procedure.none
                            ]

                        _ ->
                            -- Do nothing, and await the next event again.
                            []
            ]


setPage :
    { wrap : ( ObserverId, b ) -> PageView
    , unwrap : PageView -> Maybe ( ObserverId, b )
    }
    -> b
    -> (Observer Memory b -> List (Procedure Memory Event))
    -> Procedure Memory Event
setPage =
    Procedure.setVariant
        (global
            |> Procedure.dig
                { get = .pageView
                , set = \a m -> { m | pageView = a }
                }
        )
