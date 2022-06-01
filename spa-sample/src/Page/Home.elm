module Page.Home exposing
    ( Command(..)
    , Event
    , Memory
    , Procedures
    , init
    , procedures
    , runCommand
    , view
    )

import App.Session exposing (Session)
import App.Route as Route
import Browser.Navigation exposing (Key)
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Home.EditAccount as EditAccount
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer as Observer exposing (Observer)
import Procedure.ObserverId exposing (ObserverId)
import Procedure.VPack as VPack exposing (VPack)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { pageView : MainPageMemory
    , session : Session
    , toast : Toast.Memory
    }


{-| -}
init : Session -> Memory
init session =
    { pageView = initMainPage
    , session = session
    , toast = Toast.init
    }


{-| -}
type Event
    = ToastEvent Toast.Event
    | MainPageEvent MainPageEvent



-- View


{-| -}
view :
    VPack e Memory Event
    -> Html (Msg e)
view page =
    let
        param =
            VPack.memory page

        mainPage =
            VPack.inherit
                { get = .pageView
                , wrap = MainPageEvent
                }
                page

        toast =
            VPack.inherit
                { get = .toast
                , wrap = ToastEvent
                }
                page
    in
    Html.div
        [ localClass "page"
        ]
        [ mainPageView param.session mainPage
        , Toast.view toast
        ]



-- -- MainPage


type alias MainPageMemory =
    { editAccountForm : Maybe ( ObserverId, EditAccountFormMemory )
    }


initMainPage : MainPageMemory
initMainPage =
    { editAccountForm = Nothing
    }


type MainPageEvent
    = EditAccountFormEvent EditAccountFormEvent
    | ClickShowEditAccountForm


mainPageView :
    Session
    -> VPack e MainPageMemory MainPageEvent
    -> Html (Msg e)
mainPageView session mainPage =
    let
        param =
            VPack.memory mainPage
    in
    Html.div
        [ localClass "dashboard"
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
                VPack.child
                    mainPage
                    EditAccountFormEvent
                    (\_ -> editAccountFormView)
                    editAccountFormCore
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


type EditAccountFormEvent
    = ChangeAccountId String
    | ClickSubmitAccount
    | ClickCancel
    | ReceiveEditAccountResp (Result Http.Error EditAccount.Response)


editAccountFormView :
    VPack e EditAccountFormMemory EditAccountFormEvent
    -> Html (Msg e)
editAccountFormView editAccountForm =
    let
        param =
            VPack.memory editAccountForm

        errors =
            EditAccount.toFormErrors param.form
    in
    Html.div
        [ localClass "editAccountForm"
        , Mixin.boolAttribute "aria-invalid"
            (param.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "editAccountForm_idLabel"
            ]
            [ Html.text "New Account ID:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" param.form.id
                , Mixin.boolAttribute "disabled" param.isBusy
                , Events.onChange
                    (VPack.issue editAccountForm << ChangeAccountId)
                ]
                []
            ]
        , Html.div
            [ localClass "editAccountForm_buttonGroup"
            ]
            [ Html.node "button"
                [ localClass "editAccountForm_buttonGroup_button-cancel"
                , Events.onClick
                    (VPack.issue editAccountForm ClickCancel)
                , Mixin.boolAttribute "disabled" param.isBusy
                ]
                [ Html.text "Cancel"
                ]
            , Html.node "button"
                [ localClass "editAccountForm_buttonGroup_button-submit"
                , Events.onClick
                    (VPack.issue editAccountForm ClickSubmitAccount)
                , Mixin.boolAttribute "disabled" param.isBusy
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
    | EditAccountCommand (EditAccount.Command e)


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand

        EditAccountCommand editAccountCommand ->
            EditAccount.runCommand editAccountCommand


{-| -}
type alias Procedures m e =
    List (Procedure (Command e) m e)



-- -- Initialization


{-| -}
procedures :
    Key
    -> Observer m e Memory Event
    -> Procedures m e
procedures key page =
    let
        toast =
            Observer.inherit
                { get = .toast
                , set = \m2 m1 -> { m1 | toast = m2 }
                , unwrap =
                    \e1 ->
                        case e1 of
                            ToastEvent e2 ->
                                Just e2

                            _ ->
                                Nothing
                , wrap = ToastEvent
                }
                page

        mainPage =
            Observer.inherit
                { get = .pageView
                , set = \m2 m1 -> { m1 | pageView = m2 }
                , unwrap =
                    \e1 ->
                        case e1 of
                            MainPageEvent e2 ->
                                Just e2

                            _ ->
                                Nothing
                , wrap = MainPageEvent
                }
                page
    in
    [ Procedure.jump mainPage <|
        \_ -> mainPageProcedures key page toast mainPage
    ]


mainPageProcedures :
    Key
    -> Observer m e Memory Event
    -> Observer m e Toast.Memory Toast.Event
    -> Observer m e MainPageMemory MainPageEvent
    -> Procedures m e
mainPageProcedures key page toast mainPage =
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
