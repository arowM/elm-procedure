module Page.Catalog exposing
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
import Browser.Navigation as Nav exposing (Key)
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer as Observer exposing (Observer)
import Procedure.VPack as VPack exposing (VPack)
import Url exposing (Url)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { pageView : MainPageMemory
    , msession : Maybe Session
    , toast : Toast.Memory
    }


{-| -}
init : Memory
init =
    { pageView = initMainPage
    , msession = Nothing
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
        [ mainPageView mainPage
        , Toast.view toast
        ]



-- -- MainPage


type alias MainPageMemory =
    { loginForm : LoginFormMemory
    }


initMainPage : MainPageMemory
initMainPage =
    { loginForm = initLoginForm
    }


type MainPageEvent
    = LoginFormEvent LoginFormEvent


mainPageView :
    VPack e MainPageMemory MainPageEvent
    -> Html (Msg e)
mainPageView mainPage =
    let
        loginForm =
            VPack.inherit
                { get = .loginForm
                , wrap = LoginFormEvent
                }
                mainPage
    in
    Html.div
        [ localClass "page"
        ]
        [ loginFormView loginForm
        ]



-- View Elements
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


type LoginFormEvent
    = ChangeLoginId String
    | ChangeLoginPass String
    | ClickSubmitLogin
    | ReceiveLoginResp (Result Http.Error Login.Response)


loginFormView :
    VPack e LoginFormMemory LoginFormEvent
    -> Html (Msg e)
loginFormView loginForm =
    let
        param =
            VPack.memory loginForm

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
                , Events.onChange
                    (VPack.issue loginForm << ChangeLoginId)
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
                , Events.onChange
                    (VPack.issue loginForm << ChangeLoginPass)
                ]
                []
            ]
        , Html.node "button"
            [ localClass "loginForm_submitLogin"
            , Events.onClick
                (VPack.issue loginForm ClickSubmitLogin)
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
type alias Procedures m e =
    List (Procedure (Command e) m e)



-- -- Initialization


{-| -}
procedures :
    Url
    -> Key
    -> Observer m e Memory Event
    -> Procedures m e
procedures url key page =
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
        \_ -> mainPageProcedures url key page toast mainPage
    ]


mainPageProcedures :
    Url
    -> Key
    -> Observer m e Memory Event
    -> Observer m e Toast.Memory Toast.Event
    -> Observer m e MainPageMemory MainPageEvent
    -> Procedures m e
mainPageProcedures url key page toast mainPage =
    let
        loginForm =
            Observer.inherit
                { get = .loginForm
                , set = \m2 m1 -> { m1 | loginForm = m2 }
                , unwrap =
                    \e1 ->
                        case e1 of
                            LoginFormEvent e2 ->
                                Just e2
                , wrap = LoginFormEvent
                }
                mainPage
    in
    [ Procedure.async <|
        loginFormProcedures url key page toast loginForm
    ]


loginFormProcedures :
    Url
    -> Key
    -> Observer m e Memory Event
    -> Observer m e Toast.Memory Toast.Event
    -> Observer m e LoginFormMemory LoginFormEvent
    -> Procedures m e
loginFormProcedures url key page toast loginForm =
    [ Procedure.await loginForm <|
        \event _ ->
            case event of
                ChangeLoginId str ->
                    [ modifyLoginForm loginForm <|
                        \form -> { form | id = str }
                    , Procedure.jump loginForm <| \_ -> loginFormProcedures url key page toast loginForm
                    ]

                ChangeLoginPass str ->
                    [ modifyLoginForm loginForm <|
                        \form -> { form | pass = str }
                    , Procedure.jump loginForm <| \_ -> loginFormProcedures url key page toast loginForm
                    ]

                ClickSubmitLogin ->
                    [ Procedure.jump loginForm <|
                        \memoryOnClick ->
                            submitLoginProcedures memoryOnClick url key page toast loginForm
                    ]

                _ ->
                    []
    ]


submitLoginProcedures :
    LoginFormMemory
    -> Url
    -> Key
    -> Observer m e Memory Event
    -> Observer m e Toast.Memory Toast.Event
    -> Observer m e LoginFormMemory LoginFormEvent
    -> Procedures m e
submitLoginProcedures memoryOnClick url key page toast loginForm =
    [ Procedure.modify loginForm <|
        \m -> { m | isBusy = True }
    , case Login.fromForm memoryOnClick.form of
        Err _ ->
            [ Procedure.modify loginForm <|
                \m ->
                    { m
                        | isBusy = False
                        , showError = True
                    }
            , Procedure.jump loginForm <| \_ -> loginFormProcedures url key page toast loginForm
            ]
                |> Procedure.batch

        Ok login ->
            [ Login.request login ReceiveLoginResp loginForm
                |> Procedure.mapCmd LoginCommand
            , Procedure.await loginForm <|
                \event _ ->
                    case event of
                        ReceiveLoginResp (Err err) ->
                            [ Toast.pushHttpError err toast
                                |> Procedure.mapCmd ToastCommand
                            , Procedure.modify loginForm <|
                                \m ->
                                    { m | isBusy = False }
                            , Procedure.jump loginForm <|
                                \_ ->
                                    loginFormProcedures url key page toast loginForm
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


modifyLoginForm :
    Observer m e LoginFormMemory LoginFormEvent
    -> (Login.Form -> Login.Form)
    -> Procedure (Command e) m e
modifyLoginForm loginForm f =
    Procedure.modify loginForm <|
        \memory ->
            { memory | form = f memory.form }



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_login--" ++ name)
