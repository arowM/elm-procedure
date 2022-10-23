module Page.Home exposing
    ( Command
    , Event
    , Memory
    , init
    , procedure
    , runCommand
    -- , scenario
    , view
    )

import App.Route as Route
import App.Session exposing (Session)
import Expect.Builder as ExpBuilder
import Http
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Home.EditAccount as EditAccount
import Tepa exposing (Key, Layer, Msg, Void)
import Tepa.Scenario as Scenario
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { session : Session
    , toast : Maybe (Layer Toast.Memory)
    , editAccountForm : EditAccountFormMemory
    }


{-| -}
init : Session -> Memory
init session =
    { session = session
    , toast = Nothing
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
view : Layer Memory -> Html (Msg Event)
view =
    Tepa.layerView <|
        \memory ->
            Html.div
                [ localClass "page"
                ]
                [ editAccountFormView memory.editAccountForm
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
                , case memory.toast of
                    Nothing ->
                        Html.text ""
                    Just toast ->
                        Toast.view toast
                            |> Html.map (Tepa.mapMsg ToastEvent)
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


editAccountFormView : EditAccountFormMemory -> Html (Msg Event)
editAccountFormView memory =
    let
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
                , Events.onChange ChangeEditAccountFormAccountId
                    |> Tepa.eventMixin
                ]
                []
            ]
        , Html.div
            [ localClass "editAccountForm_buttonGroup"
            ]
            [ Html.node "button"
                [ localClass "editAccountForm_buttonGroup_button-submit"
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onClick ClickSubmitEditAccount
                    |> Tepa.eventMixin
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
    | RequestEditAccount EditAccount.EditAccount (Result Http.Error EditAccount.Response -> Msg Event)


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand
                |> Cmd.map (Tepa.mapMsg ToastEvent)

        RequestEditAccount editAccount toMsg ->
            EditAccount.request editAccount toMsg


{-| -}
type alias Promise a = Tepa.Promise Command Memory Event a


type alias Pointer m = Tepa.Pointer Memory m


type alias Bucket =
    { key : Key
    , toastPointer : Pointer Toast.Memory
    }

-- -- Initialization


{-| -}
procedure : Key -> Promise Void
procedure key =
    -- Initialize Widget
    Tepa.putMaybeLayer
        { get = .toast
        , set = \toast m -> { m | toast = toast }
        , init = Toast.init
        }
        |> Tepa.andThen
            (\toastPointer ->
                let
                    bucket =
                        { key = key
                        , toastPointer = toastPointer
                        }
                in
                -- Main Procedures
                Tepa.syncAll
                    [ editAccountFormProcedure bucket
                    ]
            )


editAccountFormProcedure : Bucket -> Promise Void
editAccountFormProcedure bucket =
    let
        modifyForm f =
            Tepa.modify <|
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
    Tepa.withLayerEvent <|
        \e ->
            case e of
                ChangeEditAccountFormAccountId str ->
                    [ modifyForm <|
                        \m -> { m | id = str }
                    , Tepa.lazy <|
                        \_ -> editAccountFormProcedure bucket
                    ]

                ClickSubmitEditAccount ->
                    [ Tepa.lazy <| \_ -> submitAccountProcedure bucket
                    ]

                _ ->
                    []


submitAccountProcedure : Bucket -> Promise Void
submitAccountProcedure bucket =
    let
        modifyEditAccountForm f =
            Tepa.modify <|
                \m ->
                    { m
                        | editAccountForm = f m.editAccountForm
                    }
    in
    Tepa.sequence
    [ modifyEditAccountForm <|
        \m -> { m | isBusy = True }
    , Tepa.currentState
        |> Tepa.andThen
            (\curr ->
                case EditAccount.fromForm curr.editAccountForm.form of
                    Err _ ->
                        Tepa.sequence
                            [ modifyEditAccountForm <|
                                \m ->
                                    { m
                                        | isBusy = False
                                        , showError = True
                                    }
                            , Tepa.lazy <| \_ -> editAccountFormProcedure bucket
                            ]

                    Ok editAccount ->
                        requestEditAccount editAccount
                            |> Tepa.andThenSequence
                                (\response ->
                                    case response of
                                        Err err ->
                                            [ Toast.pushHttpError err
                                                |> runToastPromise bucket.toastPointer
                                            , modifyEditAccountForm <|
                                                \m ->
                                                    { m | isBusy = False }
                                            , Tepa.lazy <|
                                                \_ ->
                                                    editAccountFormProcedure bucket
                                            ]

                                        Ok resp ->
                                            [ Tepa.modify <|
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
                                            , Tepa.lazy <|
                                                \_ -> editAccountFormProcedure bucket
                                            ]
                                )
            )
    ]


requestEditAccount : EditAccount.EditAccount -> Promise (Result Http.Error EditAccount.Response)
requestEditAccount editAccount =
    Tepa.customRequest
        { name = "requestEditAccount"
        , request = RequestEditAccount editAccount
        , wrap = ReceiveEditAccountResp
        , unwrap = \e ->
            case e of
                ReceiveEditAccountResp a -> Just a
                _ -> Nothing
        }


-- Toast


runToastPromise :
    Pointer Toast.Memory
    -> Tepa.Promise Toast.Command Toast.Memory Toast.Event a
    -> Promise a
runToastPromise pointer prom =
    Tepa.onLayer pointer prom
        |> Tepa.liftEvent
            { wrap = ToastEvent
            , unwrap =
                \e ->
                    case e of
                        ToastEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            }
        |> Tepa.mapCmd ToastCommand


-- Scenario


-- type alias Scenario =
--     Scenario.Scenario Command Memory Event
-- 
-- 
-- scenario :
--     Scenario.Session
--     ->
--         { user :
--             { comment : String -> Scenario
--             , changeEditAccountFormAccountId : String -> Scenario
--             , clickSubmitEditAccount : Scenario
--             }
--         , system :
--             { comment : String -> Scenario
--             , requestEditAccount : Value -> Scenario
--             }
--         , external :
--             { backend :
--                 { comment : String -> Scenario
--                 , respondSuccessToEditAccountIdRequest : EditAccount.Response -> Scenario
--                 }
--             }
--         }
-- scenario session =
--     { user =
--         { comment = Scenario.userComment session
--         , changeEditAccountFormAccountId =
--             \str ->
--                 Scenario.userEvent session
--                     ("Type \"" ++ str ++ "\" for Account ID field")
--                     (ChangeEditAccountFormAccountId str)
--         , clickSubmitEditAccount =
--             Scenario.userEvent session
--                 "Click submit button for edit account form."
--                 ClickSubmitEditAccount
--         }
--     , system =
--         { comment = Scenario.systemComment session
--         , requestEditAccount =
--             \json ->
--                 Scenario.systemCommand session
--                     "Request save new account to server"
--                     (ExpBuilder.custom <| \command ->
--                         case command of
--                             RequestEditAccount _ editAccount ->
--                                 if EditAccount.toValue editAccount == json then
--                                     ExpBuilder.pass
--                                 else
--                                     ExpBuilder.fail "thought the request body is equal to the expected JSON."
--                             _ ->
--                                 ExpBuilder.fail "thought the command is `RequestEditAccount`."
--                     )
--         }
--     , external =
--         { backend =
--             { comment = Scenario.externalComment "backend" session
--             , respondSuccessToEditAccountIdRequest =
--                 \resp ->
--                     Scenario.externalEvent "backend"
--                         session
--                         "Respond \"Success\" to edit account request"
--                         (ReceiveEditAccountResp (Ok resp))
--             }
--         }
--     }




-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_home--" ++ name)
