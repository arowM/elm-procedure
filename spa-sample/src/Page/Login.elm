module Page.Login exposing
    ( Command
    , Event
    , Memory
    , init
    , procedures
    , runCommand
    , scenario
    , view
    )

import App.Session exposing (Session)
import Browser.Navigation as Nav exposing (Key)
import Expect
import Http
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login
import Procedure.Advanced as Procedure exposing (Channel, Msg, Procedure)
import Procedure.Scenario as Scenario
import Url exposing (Url)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { msession : Maybe Session
    , toast : Toast.Memory
    , loginForm : LoginFormMemory
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
view ( channel, memory ) =
    Html.div
        [ localClass "page"
        ]
        [ loginFormView ( channel, memory.loginForm )
        , Toast.view memory.toast
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
loginFormView ( channel, memory ) =
    let
        publish =
            Procedure.publish channel

        errors =
            Login.toFormErrors memory.form
    in
    Html.div
        [ localClass "loginForm"
        , Mixin.boolAttribute "aria-invalid"
            (memory.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "loginForm_label-id"
            ]
            [ Html.text "ID:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" memory.form.id
                , Mixin.boolAttribute "disabled" memory.isBusy
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
                , Mixin.attribute "value" memory.form.pass
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onChange (publish << ChangeLoginPass)
                ]
                []
            ]
        , Html.node "button"
            [ localClass "loginForm_submitLogin"
            , Events.onClick (publish ClickSubmitLogin)
            , Mixin.boolAttribute "disabled" memory.isBusy
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
type Command
    = ToastCommand Toast.Command
    | RequestLogin (Result Http.Error Login.Response -> Msg Event) Login.Login
    | PushUrl Key String


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand
                |> Cmd.map (Procedure.mapMsg ToastEvent)

        RequestLogin toMsg login ->
            Login.request login toMsg

        PushUrl key url ->
            Nav.pushUrl key url


type alias Procedures =
    List (Procedure Command Memory Event)


-- -- Initialization


{-| -}
procedures : Url -> Key -> Procedures
procedures url key =
    [ Procedure.async <|
        loginFormProcedures url key
    ]


loginFormProcedures : Url -> Key -> Procedures
loginFormProcedures url key =
    let
        modifyLoginFormFormMemory f =
            Procedure.modify <|
                \m ->
                    { m
                        | loginForm =
                            let
                                loginForm =
                                    m.loginForm
                            in
                            { loginForm
                                | form = f loginForm.form
                            }
                    }
    in
    [ Procedure.await <|
        \event _ ->
            case event of
                ChangeLoginId str ->
                    [ modifyLoginFormFormMemory <|
                        \m -> { m | id = str }
                    , Procedure.jump <| \_ -> loginFormProcedures url key
                    ]

                ChangeLoginPass str ->
                    [ modifyLoginFormFormMemory <|
                        \m -> { m | pass = str }
                    , Procedure.jump <| \_ -> loginFormProcedures url key
                    ]

                ClickSubmitLogin ->
                    [ Procedure.jump <|
                        \_ ->
                            submitLoginProcedures url key
                    ]

                _ ->
                    []
    ]


submitLoginProcedures : Url -> Key -> Procedures
submitLoginProcedures url key =
    let
        modifyLoginFormMemory : (LoginFormMemory -> LoginFormMemory) -> Procedure Command Memory Event
        modifyLoginFormMemory f =
            Procedure.modify <|
                \({ loginForm } as m) ->
                    { m | loginForm = f loginForm }
    in
    [ modifyLoginFormMemory <|
        \m -> { m | isBusy = True }
    , Procedure.withMemory <|
        \curr ->
            case Login.fromForm curr.loginForm.form of
                Err _ ->
                    [ modifyLoginFormMemory <|
                        \m ->
                            { m
                                | isBusy = False
                                , showError = True
                            }
                    , Procedure.jump <| \_ -> loginFormProcedures url key
                    ]

                Ok login ->
                    [ Procedure.push <|
                        \_ toMsg -> RequestLogin (ReceiveLoginResp >> toMsg) login
                    , Procedure.await <|
                        \event _ ->
                            case event of
                                ReceiveLoginResp (Err err) ->
                                    [ Toast.pushHttpError err
                                        |> runToastProcedure
                                    , modifyLoginFormMemory <|
                                        \m ->
                                            { m | isBusy = False }
                                    , Procedure.jump <|
                                        \_ ->
                                            loginFormProcedures url key
                                    ]

                                ReceiveLoginResp (Ok resp) ->
                                    [ Procedure.modify <|
                                        \m ->
                                            { m
                                                | msession = Just resp.session
                                                , loginForm =
                                                    let
                                                        loginForm =
                                                            m.loginForm
                                                    in
                                                    { loginForm
                                                        | isBusy = False
                                                    }
                                            }
                                    , Procedure.push <|
                                        \_ _ -> PushUrl key <| Url.toString url
                                    , Procedure.quit
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
    Scenario.Session Command Memory Event
    ->
        { user :
            { comment : String -> Scenario
            , changeLoginId : String -> Scenario
            , changePass : String -> Scenario
            , clickSubmitLogin : Scenario
            }
        , system :
            { comment : String -> Scenario
            , requestLogin : Value -> Scenario
            }
        , external :
            { backend :
                { comment : String -> Scenario
                , respondToLoginRequest : Result Http.Error Login.Response -> Scenario
                }
            }
        }
scenario session =
    { user =
        { comment = Scenario.userComment session
        , changeLoginId =
            \str ->
                Scenario.userEvent session
                    ("Type \"" ++ str ++ "\" for Account ID field")
                    (ChangeLoginId str)
        , changePass =
            \str ->
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
        , requestLogin =
            \json ->
                Scenario.systemCommand session
                    "Request login to server"
                    (\command ->
                        case command of
                            RequestLogin _ login ->
                                if Login.toValue login == json then
                                    Expect.pass
                                else
                                    Expect.fail "thought the request body is equal to the expected JSON."
                            _ ->
                                Expect.fail "thought the command is `RequestLogin`."
                    )
        }
    , external =
        { backend =
            { comment = Scenario.externalComment "backend" session
            , respondToLoginRequest =
                \resp ->
                    Scenario.externalEvent "backend"
                        session
                        "Respond to login request"
                        (ReceiveLoginResp resp)
            }
        }
    }



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_login--" ++ name)
