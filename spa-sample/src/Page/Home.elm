module Page.Home exposing
    ( Command(..)
    , Event
    , Memory
    , init
    , mapCommand
    , procedures
    , runCommand
    , view
    )

import App.Route as Route
import App.Session exposing (Session)
import Browser.Navigation exposing (Key)
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Home.EditAccount as EditAccount
import Procedure.Advanced as Procedure exposing (Msg, ObserverId, Procedure)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { editAccountForm : Maybe ( ObserverId, EditAccountFormMemory )
    , session : Session
    , toast : Toast.Memory
    }


{-| -}
init : Session -> Memory
init session =
    { editAccountForm = Nothing
    , session = session
    , toast = Toast.init
    }


{-| -}
type Event
    = ToastEvent Toast.Event
    | ClickShowEditAccountForm
    | ChangeEditAccountFormAccountId String
    | ClickSubmitEditAccount
    | ClickCancelEditAccount
    | ReceiveEditAccountResp (Result Http.Error EditAccount.Response)



-- View


{-| -}
view : ( ObserverId, Memory ) -> Html (Msg Event)
view ( oid, param ) =
    Html.div
        [ localClass "page"
        ]
        [ case param.editAccountForm of
            Nothing ->
                Html.div
                    [ localClass "dashboard_account"
                    ]
                    [ Html.text <| "Hello, " ++ session.id
                    , Html.node "button"
                        [ localClass "dashboard_account_editButton"
                        , Events.onClick
                            (VPack.issue mainPage <| ClickShowEditAccountForm)
                        ]
                        [ Html.text "🖉"
                        ]
                    ]

            Just editAccountFormCore ->
                let
                    editAccountForm =
                        VPack.child
                            EditAccountFormEvent
                            editAccountFormCore
                            mainPage
                in
                editAccountFormView editAccountForm
        , Html.div
            [ localClass "dashboard_links"
            ]
            [ Html.a
                [ localClass "dashboard_links_linkButton-users"
                , Mixin.attribute "href" <| Route.toPath Route.Users
                ]
                [ Html.text "Users"
                ]
            ]
        , Toast.view param.toast
            |> Html.map (Procedure.mapMsg ToastEvent)
        ]



-- -- EditAccountForm


type alias EditAccountFormMemory =
    { form : EditAccount.Form
    , isBusy : Bool
    , showError : Bool
    }


initEditAccountForm : EditAccountFormMemory
initEditAccountForm =
    { form = EditAccount.initForm
    , isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    }


editAccountFormView : ( ObserverId, EditAccountFormMemory ) -> Html (Msg Event)
editAccountFormView ( oid, param ) =
    let
        issue =
            Procedure.issue oid

        errors =
            EditAccount.toFormErrors param.form
    in
    Html.div
        [ localClass "editAccountForm"
        , Mixin.boolAttribute "aria-invalid"
            (param.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "editAccountForm_label-id"
            ]
            [ Html.text "New Account ID:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" param.form.id
                , Mixin.boolAttribute "disabled" param.isBusy
                , Events.onChange (issue << ChangeEditAccountFormAccountId)
                ]
                []
            ]
        , Html.div
            [ localClass "editAccountForm_buttonGroup"
            ]
            [ Html.node "button"
                [ localClass "editAccountForm_buttonGroup_button-cancel"
                , Mixin.boolAttribute "disabled" param.isBusy
                , Events.onClick (issue ClickCancelEditAccount)
                ]
                [ Html.text "Cancel"
                ]
            , Html.node "button"
                [ localClass "editAccountForm_buttonGroup_button-submit"
                , Mixin.boolAttribute "disabled" param.isBusy
                , Events.onClick (issue ClickSubmitEditAccount)
                ]
                [ Html.text "Save"
                ]
            ]
        , Html.div
            [ localClass "editAccountForm_errorField"
            ]
            (List.map
                (\err ->
                    Html.div
                        [ localClass "editAccountForm_errorField_error"
                        ]
                        [ Html.text <| EditAccount.displayFormError err
                        ]
                )
                errors
            )
        ]



-- Procedures


{-| -}
type Command e
    = ToastCommand (Toast.Command e)
    | RequestEditAccount (Result Http.Error EditAccount.Response -> Msg e) EditAccount.EditAccount


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand

        RequestEditAccount toMsg editAccount ->
            EditAccount.request toMsg editAccount


{-| -}
mapCommand : (e1 -> e0) -> Command e1 -> Command e0
mapCommand f cmd =
    case cmd of
        ToastCommand toastCommand ->
            ToastCommand <|
                Toast.mapCommand f toastCommand

        RequestEditAccount toMsg editAccount ->
            RequestLogin (\res -> Procedure.mapMsg f (toMsg res)) editAccount


{-| -}
type alias Procedures m e =
    List (Procedure (Command e) m e)



-- -- Initialization


{-| -}
procedures :
    Key
    -> Modifier m Memory
    -> Procedures m e
procedures key page =
    [ Procedure.async <|
        mainPageProcedures key page
    ]


mainPageProcedures :
    Key
    -> Modifier m Memory
    -> Procedures m e
mainPageProcedures key page =
    [ Procedure.await mainPage <|
        \event _ ->
            case event of
                ClickShowEditAccountForm ->
                    [ Procedure.observe initEditAccountForm <|
                        \editAccountFormCore ->
                            let
                                editAccountForm =
                                    editAccountFormObserver
                                        (Tuple.first editAccountFormCore)
                                        mainPage
                            in
                            [ Procedure.modify mainPage <|
                                \m -> { m | editAccountForm = Just editAccountFormCore }
                            , Procedure.async <|
                                editAccountFormProcedures key page toast mainPage editAccountForm
                            , Procedure.jump mainPage <|
                                \_ -> mainPageProcedures key page toast mainPage
                            ]
                    ]

                _ ->
                    []
    ]


editAccountFormObserver :
    ObserverId
    -> Observer m e MainPageMemory MainPageEvent
    -> Observer m e EditAccountFormMemory EditAccountFormEvent
editAccountFormObserver oid =
    Observer.dig
        { id = oid
        , mget = .editAccountForm
        , set = \m2 m1 -> { m1 | editAccountForm = Just m2 }
        , unwrap =
            \e1 ->
                case e1 of
                    EditAccountFormEvent e2 ->
                        Just e2

                    _ ->
                        Nothing
        , wrap = EditAccountFormEvent
        }


editAccountFormProcedures :
    Key
    -> Observer m e Memory Event
    -> Observer m e Toast.Memory Toast.Event
    -> Observer m e MainPageMemory MainPageEvent
    -> Observer m e EditAccountFormMemory EditAccountFormEvent
    -> Procedures m e
editAccountFormProcedures key page toast mainPage editAccountForm =
    let
        modifyForm f =
            Procedure.modify editAccountForm <|
                \m -> { m | form = f m.form }
    in
    [ Procedure.await editAccountForm <|
        \event memoryOnEvent ->
            case event of
                ChangeAccountId id ->
                    [ modifyForm <|
                        \m -> { m | id = id }
                    , Procedure.jump editAccountForm <|
                        \_ -> editAccountFormProcedures key page toast mainPage editAccountForm
                    ]

                ClickCancel ->
                    [ Procedure.modify mainPage <|
                        \m -> { m | editAccountForm = Nothing }
                    , Procedure.quit
                    ]

                ClickSubmitAccount ->
                    [ Procedure.modify editAccountForm <|
                        \m -> { m | isBusy = True }
                    , case EditAccount.fromForm memoryOnEvent.form of
                        Err _ ->
                            [ Procedure.modify editAccountForm <|
                                \m ->
                                    { m
                                        | isBusy = False
                                        , showError = True
                                    }
                            , Procedure.jump editAccountForm <|
                                \_ -> editAccountFormProcedures key page toast mainPage editAccountForm
                            ]
                                |> Procedure.batch

                        Ok editAccount ->
                            [ EditAccount.request editAccount ReceiveEditAccountResp editAccountForm
                                |> Procedure.mapCmd EditAccountCommand
                            , Procedure.await editAccountForm <|
                                \event2 _ ->
                                    case event2 of
                                        ReceiveEditAccountResp (Err err) ->
                                            [ Toast.pushHttpError err toast
                                                |> Procedure.mapCmd ToastCommand
                                            , Procedure.modify editAccountForm <|
                                                \m ->
                                                    { m | isBusy = False }
                                            , Procedure.jump editAccountForm <|
                                                \_ ->
                                                    editAccountFormProcedures key page toast mainPage editAccountForm
                                            ]

                                        ReceiveEditAccountResp (Ok resp) ->
                                            [ Procedure.modify page <|
                                                \m -> { m | session = resp.session }
                                            , Procedure.modify mainPage <|
                                                \m -> { m | editAccountForm = Nothing }
                                            , Procedure.quit
                                            ]

                                        _ ->
                                            []
                            ]
                                |> Procedure.batch
                    ]

                _ ->
                    []
    ]



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_home--" ++ name)
