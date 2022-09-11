module Page.Login exposing
    ( Command(..)
    , Event(..)
    , Memory
    , init
    , mapCommand
    , procedures
    , runCommand
    , view
    , scenario
    )

import App.Session exposing (Session)
import Browser.Navigation as Nav exposing (Key)
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login exposing (Login)
import Procedure.Advanced as Procedure exposing (Channel, Msg, Procedure)
import Procedure.Modifier as Modifier exposing (Modifier)
import Procedure.Scenario as Scenario
import Url exposing (Url)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { loginForm : LoginFormMemory
    , msession : Maybe Session
    , toast : Toast.Memory
    }


{-| -}
init : Memory
init =
    { loginForm = initLoginForm
    , msession = Nothing
    , toast = Toast.init
    }


{-| -}
type Event
    = ToastEvent Toast.Event
    | ChangeLoginId String
    | ChangeLoginPass String
    | ClickSubmitLogin
    | ReceiveLoginResp (Result Http.Error Login.Response)



-- View


{-| -}
view : ( Channel, Memory ) -> Html (Msg Event)
view ( channel, param ) =
    Html.div
        [ localClass "page"
        ]
        [ loginFormView ( channel, param.loginForm )
        , Toast.view param.toast
            |> Html.map (Procedure.mapMsg ToastEvent)
        ]



-- -- LoginForm


type alias LoginFormMemory =
    { form : Login.Form
    , isBusy : Bool
    , showError : Bool
    }


initLoginForm : LoginFormMemory
initLoginForm =
    { form = Login.initForm
    , isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    }


loginFormView : ( Channel, LoginFormMemory ) -> Html (Msg Event)
loginFormView ( channel, param ) =
    let
        publish =
            Procedure.publish channel

        errors =
            Login.toFormErrors param.form
    in
    Html.div
        [ localClass "loginForm"
        , Mixin.boolAttribute "aria-invalid"
            (param.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "loginForm_label-id"
            ]
            [ Html.text "ID:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" param.form.id
                , Mixin.boolAttribute "disabled" param.isBusy
                , Events.onChange (publish << ChangeLoginId)
                ]
                []
            ]
        , Html.node "label"
            [ localClass "loginForm_label-password"
            ]
            [ Html.text "Password:"
            , Html.node "input"
                [ Mixin.attribute "type" "password"
                , Mixin.attribute "value" param.form.pass
                , Mixin.boolAttribute "disabled" param.isBusy
                , Events.onChange (publish << ChangeLoginPass)
                ]
                []
            ]
        , Html.node "button"
            [ localClass "loginForm_submitLogin"
            , Events.onClick (publish ClickSubmitLogin)
            , Mixin.boolAttribute "disabled" param.isBusy
            ]
            [ Html.text "Login"
            ]
        , Html.div
            [ localClass "loginForm_notes"
            ]
            [ Html.div
                [ localClass "loginForm_notes_head"
                ]
                [ Html.text "For guests:"
                ]
            , Html.div
                [ localClass "loginForm_notes_text"
                ]
                [ Html.text "ID: guest"
                ]
            , Html.div
                [ localClass "loginForm_notes_text"
                ]
                [ Html.text "Password: guest"
                ]
            ]
        , Html.div
            [ localClass "loginForm_errorField"
            ]
            (List.map
                (\err ->
                    Html.div
                        [ localClass "loginForm_errorField_error"
                        ]
                        [ Html.text <| Login.displayFormError err
                        ]
                )
                errors
            )
        ]



-- Procedures


{-| -}
type Command e
    = ToastCommand (Toast.Command e)
    | RequestLogin (Result Http.Error Login.Response -> Msg e) Login.Login
    | PushUrl Key String


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand

        RequestLogin toMsg login ->
            Login.request login toMsg

        PushUrl key url ->
            Nav.pushUrl key url


{-| -}
mapCommand : (e1 -> e0) -> Command e1 -> Command e0
mapCommand f cmd =
    case cmd of
        ToastCommand toastCommand ->
            ToastCommand <|
                Toast.mapCommand f toastCommand

        RequestLogin toMsg login ->
            RequestLogin (\res -> Procedure.mapMsg f (toMsg res)) login

        PushUrl key url ->
            PushUrl key url



-- -- Initialization


{-| -}
procedures :
    Url
    -> Key
    -> Modifier m Memory
    -> List (Procedure (Command Event) m Event)
procedures url key page =
    [ Procedure.async <|
        loginFormProcedures url key page
    ]


loginFormProcedures :
    Url
    -> Key
    -> Modifier m Memory
    -> List (Procedure (Command Event) m Event)
loginFormProcedures url key page =
    let
        loginForm : Modifier m LoginFormMemory
        loginForm =
            Modifier.dig
                { get = .loginForm
                , set = \a m -> { m | loginForm = a }
                }
                page

        form : Modifier m Login.Form
        form =
            Modifier.dig
                { get = .form
                , set = \a m -> { m | form = a }
                }
                loginForm
    in
    [ Procedure.await loginForm <|
        \event _ ->
            case event of
                ChangeLoginId str ->
                    [ Procedure.modify form <|
                        \m -> { m | id = str }
                    , Procedure.jump page <| \_ -> loginFormProcedures url key page
                    ]

                ChangeLoginPass str ->
                    [ Procedure.modify form <|
                        \m -> { m | pass = str }
                    , Procedure.jump page <| \_ -> loginFormProcedures url key page
                    ]

                ClickSubmitLogin ->
                    [ Procedure.jump form <|
                        \formState ->
                            submitLoginProcedures url key formState page
                    ]

                _ ->
                    []
    ]


submitLoginProcedures :
    Url
    -> Key
    -> Login.Form
    -> List (Procedure (Command Event) m Event)
submitLoginProcedures url key formState =
    [ Procedure.modify <|
        \({ loginForm } as m) ->
            { m
                | loginForm =
                    { loginForm | isBusy = True }
            }
    , case Login.fromForm formState of
        Err _ ->
            [ Procedure.modify <|
                \({ loginForm } as m) ->
                    { m
                        | loginForm =
                            { loginForm
                                | isBusy = False
                                , showError = True
                            }
                    }
            , Procedure.jump <| \_ -> loginFormProcedures url key page
            ]
                |> Procedure.batch

        Ok login ->
            [ Procedure.push <|
                \_ toMsg -> RequestLogin (ReceiveLoginResp >> toMsg) login
            , Procedure.await <|
                \event _ ->
                    case event of
                        ReceiveLoginResp (Err err) ->
                            [ Toast.pushHttpError err toast
                                |> runToastProcedure
                            , Procedure.modify loginForm <|
                                \m ->
                                    { m | isBusy = False }
                            , Procedure.jump page <|
                                \_ ->
                                    loginFormProcedures url key page
                            ]

                        ReceiveLoginResp (Ok resp) ->
                            [ Procedure.modify loginForm <|
                                \m -> { m | isBusy = False }
                            , Procedure.modify page <|
                                \m -> { m | msession = Just resp.session }
                            , Procedure.push page <|
                                \_ _ -> PushUrl key <| Url.toString url
                            , Procedure.quit
                            ]

                        _ ->
                            []
            ]
                |> Procedure.batch
    ]



-- Toast


runToastProcedure :
    Procedure (Toast.Command Toast.Event) m Toast.Event
    -> Procedure (Command Event) m Event
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
        >> Procedure.mapCmd (ToastCommand << Toast.mapCommand ToastEvent)


-- Scenario


type alias Scenario = Scenario.Scenario (Command Event) Memory Event

scenario : Scenario.Session (Command Event) Memory Event ->
    { user :
        { comment : String -> Scenario
        , changeLoginId : String -> Scenario
        , changePass : String -> Scenario
        , clickSubmitLogin : Scenario
        }
    , system :
        { comment : String -> Scenario
        , requestLogin : Login -> Scenario
        }
    , external :
        { backend :
            { comment : String -> Scenario
            , respondInvalidIdOrPasswordToLoginRequest : Scenario
            , respondSuccessToLoginRequest : Login.Response -> Scenario
            }
        }
    }
scenario session =
    { user =
        { comment = Scenario.userComment session
        , changeLoginId = \str ->
            Scenario.userEvent session
                ("Type \"" ++ str ++ "\" for Account ID field")
                (ChangeLoginId str)
        , changePass = \str ->
            Scenario.userEvent session
                ("Type \"" ++ str ++ "\" for Password field")
                (ChangeLoginPass str)
        , clickSubmitLogin =
            Scenario.userEvent session
                "Click \"Login\" submit button"
                ClickSubmitLogin
        }
    , system =
        { comment = Scenario.systemComment session
        , requestLogin = \login ->
            Scenario.systemCommand session
                "Request login to server"
                (\channel -> RequestLogin (ReceiveLoginResp >> Procedure.publish channel) login)
        }
    , external =
        { backend =
            { comment = Scenario.externalComment "backend" session
            , respondInvalidIdOrPasswordToLoginRequest =
                Scenario.externalEvent "backend" session
                    "Respond \"Invalid ID or Password\" error to login request"
                    (ReceiveLoginResp (Err (Http.BadStatus 401)))
            , respondSuccessToLoginRequest = \resp ->
                Scenario.externalEvent "backend" session
                    "Respond \"Success\" to login request"
                    (ReceiveLoginResp (Ok resp))
            }
        }
    }


-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_login--" ++ name)
