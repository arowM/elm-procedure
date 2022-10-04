module Page.Home exposing
    ( Command
    , Event
    , Memory
    , init
    , procedures
    , runCommand
    , scenario
    , view
    )

import App.Route as Route
import App.Session exposing (Session)
import Browser.Navigation exposing (Key)
import Expect.Builder as ExpBuilder
import Http
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Home.EditAccount as EditAccount
import Procedure.Advanced as Procedure exposing (Channel, Msg, Procedure)
import Procedure.Scenario as Scenario
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { session : Session
    , toast : Toast.Memory
    , editAccountForm : EditAccountFormMemory
    }


{-| -}
init : Session -> Memory
init session =
    { session = session
    , toast = Toast.init
    , editAccountForm = initEditAccountForm session
    }


{-| -}
type Event
    = ToastEvent Toast.Event
    | ChangeEditAccountFormAccountId String
    | ClickSubmitEditAccount
    | ReceiveEditAccountResp (Result Http.Error EditAccount.Response)



-- View


{-| -}
view : ( Channel, Memory ) -> Html (Msg Event)
view ( channel, memory ) =
    Html.div
        [ localClass "page"
        ]
        [ editAccountFormView (channel, memory.editAccountForm)
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
        , Toast.view memory.toast
            |> Html.map (Procedure.mapMsg ToastEvent)
        ]



-- -- EditAccountForm


type alias EditAccountFormMemory =
    { form : EditAccount.Form
    , isBusy : Bool
    , showError : Bool
    }


initEditAccountForm : Session -> EditAccountFormMemory
initEditAccountForm session =
    { form = EditAccount.initForm session.id
    , isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    }


editAccountFormView : (Channel, EditAccountFormMemory) -> Html (Msg Event)
editAccountFormView (channel, memory) =
    let
        publish =
            Procedure.publish channel

        errors =
            EditAccount.toFormErrors memory.form
    in
    Html.div
        [ localClass "editAccountForm"
        , Mixin.boolAttribute "aria-invalid"
            (memory.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "editAccountForm_label-id"
            ]
            [ Html.text "New Account ID:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" memory.form.id
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onChange (publish << ChangeEditAccountFormAccountId)
                ]
                []
            ]
        , Html.div
            [ localClass "editAccountForm_buttonGroup"
            ]
            [ Html.node "button"
                [ localClass "editAccountForm_buttonGroup_button-submit"
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onClick (publish ClickSubmitEditAccount)
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
type Command
    = ToastCommand Toast.Command
    | RequestEditAccount (Result Http.Error EditAccount.Response -> Msg Event) EditAccount.EditAccount


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand
                |> Cmd.map (Procedure.mapMsg ToastEvent)

        RequestEditAccount toMsg editAccount ->
            EditAccount.request editAccount toMsg


{-| -}
type alias Procedures =
    List (Procedure Command Memory Event)



-- -- Initialization


{-| -}
procedures : Key -> Procedures
procedures key =
    [ Procedure.async <|
        mainPageProcedures key
    ]


mainPageProcedures : Key -> Procedures
mainPageProcedures key =
    [ Procedure.async <|
        editAccountFormProcedures key
    ]


editAccountFormProcedures : Key -> Procedures
editAccountFormProcedures key =
    let
        modifyEditAccountFormFormMemory f =
            Procedure.modify <|
                \m ->
                    { m
                        | editAccountForm =
                            let
                                editAccountForm = m.editAccountForm
                            in
                            { editAccountForm
                                | form = f editAccountForm.form
                            }
                    }
    in
    [ Procedure.await <|
        \event _ ->
            case event of
                ChangeEditAccountFormAccountId str ->
                    [ modifyEditAccountFormFormMemory <|
                        \m -> { m | id = str }
                    , Procedure.jump <|
                        \_ -> editAccountFormProcedures key
                    ]

                ClickSubmitEditAccount ->
                    [ Procedure.jump <|
                        \_ ->
                            submitAccountProcedures key
                    ]

                _ ->
                    []
    ]


submitAccountProcedures : Key -> Procedures
submitAccountProcedures key =
    let
        modifyEditAccountFormMemory f =
            Procedure.modify <|
                \m ->
                    { m
                        | editAccountForm = f m.editAccountForm
                    }
    in
    [ modifyEditAccountFormMemory <|
        \m -> { m | isBusy = True }
    , Procedure.withMemory <|
        \curr ->
            case EditAccount.fromForm curr.editAccountForm.form of
                Err _ ->
                    [ modifyEditAccountFormMemory <|
                        \m ->
                            { m
                                | isBusy = False
                                , showError = True
                            }
                    , Procedure.jump <| \_ -> editAccountFormProcedures key
                    ]

                Ok editAccount ->
                    [ Procedure.push <|
                        \_ toMsg -> RequestEditAccount (ReceiveEditAccountResp >> toMsg) editAccount
                    , Procedure.await <|
                        \event _ ->
                            case event of
                                ReceiveEditAccountResp (Err err) ->
                                    [ Toast.pushHttpError err
                                        |> runToastProcedure
                                    , modifyEditAccountFormMemory <|
                                        \m ->
                                            { m | isBusy = False }
                                    , Procedure.jump <|
                                        \_ ->
                                            editAccountFormProcedures key
                                    ]

                                ReceiveEditAccountResp (Ok resp) ->
                                    [ Procedure.modify <|
                                        \m ->
                                            { m
                                                | session = resp.session
                                                , editAccountForm =
                                                    let
                                                        editAccountForm = m.editAccountForm
                                                    in
                                                    { editAccountForm
                                                        | isBusy = False
                                                    }
                                            }
                                    , Procedure.jump <|
                                        \_ -> editAccountFormProcedures key
                                    ]

                                _ ->
                                    []
                    ]
    ]


-- Toast


runToastProcedure :
    Procedure Toast.Command Toast.Memory Toast.Event
    -> Procedure Command Memory Event
runToastProcedure =
    Procedure.wrapEvent
        { wrap = ToastEvent
        , unwrap =
            \e ->
                case e of
                    ToastEvent e1 ->
                        Just e1

                    _ ->
                        Nothing
        }
        >> Procedure.mapCmd ToastCommand
        >> Procedure.liftMemory
            { get = .toast >> Just
            , modify = \f m -> { m | toast = f m.toast }
            }


-- Scenario


type alias Scenario =
    Scenario.Scenario Command Memory Event


scenario :
    Scenario.Session
    ->
        { user :
            { comment : String -> Scenario
            , changeEditAccountFormAccountId : String -> Scenario
            , clickSubmitEditAccount : Scenario
            }
        , system :
            { comment : String -> Scenario
            , requestEditAccount : Value -> Scenario
            }
        , external :
            { backend :
                { comment : String -> Scenario
                , respondSuccessToEditAccountIdRequest : EditAccount.Response -> Scenario
                }
            }
        }
scenario session =
    { user =
        { comment = Scenario.userComment session
        , changeEditAccountFormAccountId =
            \str ->
                Scenario.userEvent session
                    ("Type \"" ++ str ++ "\" for Account ID field")
                    (ChangeEditAccountFormAccountId str)
        , clickSubmitEditAccount =
            Scenario.userEvent session
                "Click submit button for edit account form."
                ClickSubmitEditAccount
        }
    , system =
        { comment = Scenario.systemComment session
        , requestEditAccount =
            \json ->
                Scenario.systemCommand session
                    "Request save new account to server"
                    (ExpBuilder.custom <| \command ->
                        case command of
                            RequestEditAccount _ editAccount ->
                                if EditAccount.toValue editAccount == json then
                                    ExpBuilder.pass
                                else
                                    ExpBuilder.fail "thought the request body is equal to the expected JSON."
                            _ ->
                                ExpBuilder.fail "thought the command is `RequestEditAccount`."
                    )
        }
    , external =
        { backend =
            { comment = Scenario.externalComment "backend" session
            , respondSuccessToEditAccountIdRequest =
                \resp ->
                    Scenario.externalEvent "backend"
                        session
                        "Respond \"Success\" to edit account request"
                        (ReceiveEditAccountResp (Ok resp))
            }
        }
    }




-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_home--" ++ name)
