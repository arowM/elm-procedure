module Page.Login exposing
    ( Command(..)
    , Event
    , Memory
    , init
    , procedures
    , mapCommand
    , runCommand
    , view
    )

import App.Session exposing (Session)
import Browser.Navigation as Nav exposing (Key)
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login
import Procedure.Advanced as Procedure exposing (Msg, ObserverId, Procedure)
import Procedure.Observer as Observer exposing (Observer)
import Url exposing (Url)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { loginForm : LoginFormMemory
    , msession : Maybe Session
    , toast : Toast.Memory
    }


{-| -}
init : ObserverId Event -> Memory
init oid =
    { loginForm = initLoginForm
    , msession = Nothing
    , toast = Toast.init (ObserverId.child ToastEvent oid)
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
view : (ObserverId Event, Memory) -> Html (Msg Event)
view (oid, param) =
    Html.div
        [ localClass "page"
        ]
        [ loginFormView (oid, param.loginForm)
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


loginFormView : (ObserverId Event, LoginFormMemory) -> Html (Msg Event)
loginFormView (oid, param) =
    let
        issue = Procedure.issue oid
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
                , Events.onChange (issue << ChangeLoginId)
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
                , Events.onChange (issue << ChangeLoginPass)
                ]
                []
            ]
        , Html.node "button"
            [ localClass "loginForm_submitLogin"
            , Events.onClick (issue ClickSubmitLogin)
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
    | LoginCommand (Login.Command e)
    | PushUrl Key String


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand

        LoginCommand loginCommand ->
            Login.runCommand loginCommand

        PushUrl key url ->
            Nav.pushUrl key url


{-| -}
mapCommand : (e1 -> e0) -> Command e1 -> Command e0
mapCommand f cmd =
    case cmd of
        ToastCommand toastCommand ->
            ToastCommand <|
                Toast.mapCommand f toastCommand

        LoginCommand loginCommand ->
            LoginCommand <|
                Login.mapCommand f loginCommand

        PushUrl key url ->
            PushUrl key url




-- -- Initialization


{-| -}
procedures :
    Url
    -> Key
    -> Observer m Event Memory
    -> List (Procedure (Command Event) m Event)
procedures url key page =
    [ Procedure.async <|
        loginFormProcedures url key page
    ]


loginFormProcedures :
    Url
    -> Key
    -> Observer m Event Memory
    -> List (Procedure (Command Event) m Event)
loginFormProcedures url key page =
    let
        loginForm : Observer m Event LoginFormMemory
        loginForm =
            Observer.inherit
                { get = .loginForm
                , set = \a m -> { m | loginForm = a }
                }
                page
        form : Observer m Event Login.Form
        form =
            Observer.inherit
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
    -> Observer m Event Memory
    -> List (Procedure (Command Event) m Event)
submitLoginProcedures url key formState page =
    let
        loginForm : Observer m Event LoginFormMemory
        loginForm =
            Observer.inherit
                { get = .loginForm
                , set = \a m -> { m | loginForm = a }
                }
                page

        toast : Observer m Toast.Memory
        toast =
            Observer.inherit
                { get = .toast
                , set = \a m -> { m | toast = a }
                }
                page
    in
    [ Procedure.modify loginForm <|
        \m -> { m | isBusy = True }
    , case Login.fromForm formState of
        Err _ ->
            [ Procedure.modify loginForm <|
                \m ->
                    { m
                        | isBusy = False
                        , showError = True
                    }
            , Procedure.jump page <| \_ -> loginFormProcedures url key page
            ]
                |> Procedure.batch

        Ok login ->
            [ Login.request login
                loginForm
                LoginCommand
                ReceiveLoginResp
            , Procedure.await loginForm <|
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
                                \_ -> PushUrl key <| Url.toString url
                            , Procedure.quit
                            ]

                        _ ->
                            []
            ]
                |> Procedure.batch
    ]



-- Toast


runToastProcedure : Procedure (Toast.Command Toast.Event) m Toast.Event
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


-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_login--" ++ name)
