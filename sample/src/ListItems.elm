module ListItems exposing (Event, Form, GoatCard, Memory, Saved, main)

import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Procedure exposing (Document, Msg, Observer, Procedure, Program, global)
import Procedure.ObserverId as ObserverId exposing (ObserverId)
import Procedure.VPack as VPack exposing (VPack)


main : Program () Memory Event
main =
    Procedure.document
        { init = init
        , procedures = procedures
        , view = view
        , subscriptions = subscriptions
        }



-- Memory


type alias Memory =
    { cards : List ( ObserverId, GoatCard )
    }


init : Memory
init =
    { cards = []
    }



-- -- Goat Card


{-| Local memory for a goat card, which is used to manage a personal information for a goat.
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


initGoatCard : GoatCard
initGoatCard =
    { form =
        { name = ""
        }
    , saved =
        { name = ""
        }
    , onEditing = True
    }



-- Event


type Event
    = GoatCardEvent GoatCardEvent
    | ClickAddGoatCard


type GoatCardEvent
    = ClickEditGoat
    | ClickRemoveGoat
    | ClickSaveGoat
    | ClickCancelGoat
    | ChangeGoatName String



-- View


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
                                ( oid, goatCard )
                                goatCardView
                            )
                        )
                )
            ]
        ]
    }



-- -- Goat card


goatCardView : VPack Event GoatCardEvent GoatCard -> Html (Msg Event)
goatCardView goatCard =
    if (VPack.memory goatCard).onEditing then
        editModeGoatCardView goatCard

    else
        savedModeGoatCardView goatCard


{-| -}
editModeGoatCardView : VPack Event GoatCardEvent GoatCard -> Html (Msg Event)
editModeGoatCardView goatCard =
    let
        param =
            { form = (VPack.memory goatCard).form
            }
    in
    Html.div
        [ style "border" "solid #333 2px"
        , style "border-radius" "0.2em"
        ]
        [ Html.div
            [ style "display" "inline-block"
            ]
            [ Html.span
                [ style "padding" "0.4em"
                ]
                [ Html.text "name:"
                ]
            , Html.input
                [ style "margin" "0.4em"
                , onChange (VPack.issue goatCard << ChangeGoatName)
                , Attributes.value param.form.name
                ]
                []
            ]
        , Html.div
            [ style "display" "inline-block"
            ]
            [ Html.div
                [ style "padding" "0.4em"
                , style "display" "inline-block"
                ]
                [ Html.button
                    [ style "padding" "0.4em"
                    , Attributes.type_ "button"
                    , Events.onClick (VPack.issue goatCard ClickCancelGoat)
                    ]
                    [ Html.text "Cancel"
                    ]
                , Html.button
                    [ style "padding" "0.4em"
                    , Attributes.type_ "button"
                    , Events.onClick (VPack.issue goatCard ClickSaveGoat)
                    ]
                    [ Html.text "Save"
                    ]
                ]
            ]
        ]


savedModeGoatCardView : VPack Event GoatCardEvent GoatCard -> Html (Msg Event)
savedModeGoatCardView goatCard =
    let
        param =
            { saved = (VPack.memory goatCard).saved
            }
    in
    Html.div
        [ style "border" "solid #333 2px"
        , style "border-radius" "0.2em"
        , style "padding" "0.4em"
        , style "min-width" "16em"
        ]
        [ Html.div
            []
            [ Html.div
                [ style "text-align" "right"
                , style "color" "#733"
                ]
                [ Html.span
                    [ style "padding" "0.4em"
                    , style "text-decoration" "underline"
                    , style "cursor" "pointer"
                    , Attributes.tabindex 0
                    , Attributes.attribute "role" "button"
                    , Events.onClick (VPack.issue goatCard ClickEditGoat)
                    ]
                    [ Html.text "edit"
                    ]
                , Html.span
                    [ style "padding" "0.4em"
                    , style "text-decoration" "underline"
                    , style "cursor" "pointer"
                    , Attributes.tabindex 0
                    , Attributes.attribute "role" "button"
                    , Events.onClick (VPack.issue goatCard ClickRemoveGoat)
                    ]
                    [ Html.text "remove"
                    ]
                ]
            , Html.div
                [ style "padding" "0.4em"
                ]
                [ Html.text <| "name: " ++ param.saved.name
                ]
            ]
        ]



-- Subsctiption


subscriptions : VPack Event Event Memory -> Sub (Msg Event)
subscriptions _ =
    Sub.none



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



-- -- Goat card


{-| This procedure is completed only when the remove button clicked.
-}
goatCardProcedures : Observer Memory GoatCard -> List (Procedure Memory Event)
goatCardProcedures goatCard =
    editModeGoatCardProcedures
        goatCard
        { isInitial = True
        }


{-| Procedure for editing mode.
-}
editModeGoatCardProcedures : Observer Memory GoatCard -> { isInitial : Bool } -> List (Procedure Memory Event)
editModeGoatCardProcedures goatCard opt =
    let
        form : Observer Memory Form
        form =
            goatCard
                |> Procedure.dig
                    { get = .form
                    , set = \a m -> { m | form = a }
                    }

        cards : Observer Memory (List ( ObserverId, GoatCard ))
        cards =
            global
                |> Procedure.dig
                    { get = .cards
                    , set = \c m -> { m | cards = c }
                    }

        awaitGoatCard =
            Procedure.await goatCard <|
                \event ->
                    case event of
                        GoatCardEvent a ->
                            Just a

                        _ ->
                            Nothing
    in
    [ awaitGoatCard <|
        \event _ ->
            case event of
                ClickSaveGoat ->
                    [ Procedure.modify goatCard <|
                        \m ->
                            { m
                                | form =
                                    { name = ""
                                    }
                                , saved =
                                    { name = m.form.name
                                    }
                                , onEditing = False
                            }
                    , Procedure.jump goatCard <| \_ -> savedModeGoatCardProcedures goatCard
                    ]

                ClickCancelGoat ->
                    [ Procedure.when opt.isInitial
                        [ Procedure.remove cards goatCard
                        , Procedure.quit
                        ]
                    , Procedure.modify goatCard <|
                        \m ->
                            { m
                                | form =
                                    { name = ""
                                    }
                                , onEditing = False
                            }
                    , Procedure.jump goatCard <| \_ -> savedModeGoatCardProcedures goatCard
                    ]

                ChangeGoatName str ->
                    [ Procedure.modify form <|
                        \m -> { m | name = str }
                    , Procedure.jump goatCard <| \_ -> editModeGoatCardProcedures goatCard opt
                    ]

                _ ->
                    []
    ]


{-| Procedure for view mode.
-}
savedModeGoatCardProcedures : Observer Memory GoatCard -> List (Procedure Memory Event)
savedModeGoatCardProcedures goatCard =
    let
        cards : Observer Memory (List ( ObserverId, GoatCard ))
        cards =
            global
                |> Procedure.dig
                    { get = .cards
                    , set = \a m -> { m | cards = a }
                    }

        awaitGoatCard =
            Procedure.await goatCard <|
                \event ->
                    case event of
                        GoatCardEvent a ->
                            Just a

                        _ ->
                            Nothing
    in
    [ awaitGoatCard <|
        \event _ ->
            case event of
                ClickEditGoat ->
                    [ Procedure.modify goatCard <|
                        \m ->
                            { m
                                | onEditing = True
                                , form =
                                    { name = m.saved.name
                                    }
                            }
                    , Procedure.jump goatCard <|
                        \_ ->
                            editModeGoatCardProcedures
                                goatCard
                                { isInitial = False
                                }
                    ]

                ClickRemoveGoat ->
                    [ Procedure.remove cards goatCard
                    , Procedure.quit
                    ]

                _ ->
                    []
    ]
