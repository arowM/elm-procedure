module Page.Login exposing
    ( Command
    , Event
    , Memory
    , currentSession
    , init
    , procedure
    ,  runCommand
       -- , scenario

    , view
    )

import App.Session exposing (Session)
import Browser.Navigation as Nav
import Expect.Builder as ExpBuilder
import Http
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login
import Tepa exposing (Key, Layer, Msg, Void)
import Tepa.Scenario as Scenario
import Url exposing (Url)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { msession : Maybe Session
    , toast : Maybe (Layer Toast.Memory)
    , loginForm : LoginFormMemory
    }


{-| -}
init : Memory
init =
    { loginForm = initLoginForm
    , msession = Nothing
    , toast = Nothing
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
view : Layer Memory -> Html (Msg Event)
view =
    Tepa.layerView <|
        \memory ->
            Html.div
                [ localClass "page"
                ]
                [ loginFormView memory.loginForm
                , case memory.toast of
                    Nothing ->
                        Html.text ""

                    Just toast ->
                        Toast.view toast
                            |> Html.map (Tepa.mapMsg ToastEvent)
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


loginFormView : LoginFormMemory -> Html (Msg Event)
loginFormView memory =
    let
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
                , Events.onChange ChangeLoginId
                    |> Tepa.eventMixin
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
                , Events.onChange ChangeLoginPass
                    |> Tepa.eventMixin
                ]
                []
            ]
        , Html.node "button"
            [ localClass "loginForm_submitLogin"
            , Events.onClick ClickSubmitLogin
                |> Tepa.eventMixin
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
    | RequestLogin Login.Login (Result Http.Error Login.Response -> Msg Event)
    | PushUrl Key String


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand
                |> Cmd.map (Tepa.mapMsg ToastEvent)

        RequestLogin login toMsg ->
            Login.request login toMsg

        PushUrl key url ->
            Tepa.runNavCmd
                (\navKey -> Nav.pushUrl navKey url)
                key


type alias Promise a =
    Tepa.Promise Command Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m


type alias Bucket =
    { key : Key
    , requestUrl : Url
    , toastPointer : Pointer Toast.Memory
    }


{-| -}
currentSession : Promise (Maybe Session)
currentSession =
    Tepa.currentState
        |> Tepa.map (\m -> m.msession)



-- -- Initialization


{-| -}
procedure : Url -> Key -> Promise Void
procedure url key =
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
                        , requestUrl = url
                        , toastPointer = toastPointer
                        }
                in
                -- Main Procedures
                Tepa.syncAll
                    [ loginFormProcedure bucket
                    ]
            )


loginFormProcedure : Bucket -> Promise Void
loginFormProcedure bucket =
    let
        modifyForm f =
            Tepa.modify <|
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
    Tepa.withLayerEvent <|
        \e ->
            case e of
                ChangeLoginId str ->
                    [ modifyForm <|
                        \m -> { m | id = str }
                    , Tepa.lazy <|
                        \_ -> loginFormProcedure bucket
                    ]

                ChangeLoginPass str ->
                    [ modifyForm <|
                        \m -> { m | pass = str }
                    , Tepa.lazy <| \_ -> loginFormProcedure bucket
                    ]

                ClickSubmitLogin ->
                    [ Tepa.lazy <|
                        \_ ->
                            submitLoginProcedure bucket
                    ]

                _ ->
                    []


submitLoginProcedure : Bucket -> Promise Void
submitLoginProcedure bucket =
    let
        modifyLoginForm f =
            Tepa.modify <|
                \m ->
                    { m | loginForm = f m.loginForm }
    in
    Tepa.sequence
        [ modifyLoginForm <|
            \m -> { m | isBusy = True }
        , Tepa.currentState
            |> Tepa.andThen
                (\curr ->
                    case Login.fromForm curr.loginForm.form of
                        Err _ ->
                            Tepa.sequence
                                [ modifyLoginForm <|
                                    \m ->
                                        { m
                                            | isBusy = False
                                            , showError = True
                                        }
                                , Tepa.lazy <| \_ -> loginFormProcedure bucket
                                ]

                        Ok login ->
                            requestLogin login
                                |> Tepa.andThenSequence
                                    (\response ->
                                        case response of
                                            Err err ->
                                                [ Toast.pushHttpError err
                                                    |> runToastPromise bucket.toastPointer
                                                , modifyLoginForm <|
                                                    \m ->
                                                        { m | isBusy = False }
                                                , Tepa.lazy <|
                                                    \_ ->
                                                        loginFormProcedure bucket
                                                ]

                                            Ok resp ->
                                                [ Tepa.modify <|
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
                                                , pushUrl bucket.key bucket.requestUrl
                                                ]
                                    )
                )
        ]


requestLogin : Login.Login -> Promise (Result Http.Error Login.Response)
requestLogin login =
    Tepa.customRequest
        { name = "requestLogin"
        , request = RequestLogin login
        , wrap = ReceiveLoginResp
        , unwrap =
            \e ->
                case e of
                    ReceiveLoginResp a ->
                        Just a

                    _ ->
                        Nothing
        }


pushUrl : Key -> Url -> Promise Void
pushUrl key url =
    Tepa.push <| \_ -> PushUrl key <| Url.toString url



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
--             , changeLoginId : String -> Scenario
--             , changePass : String -> Scenario
--             , clickSubmitLogin : Scenario
--             }
--         , system :
--             { comment : String -> Scenario
--             , requestLogin : Value -> Scenario
--             }
--         , external :
--             { backend :
--                 { comment : String -> Scenario
--                 , respondToLoginRequest : Result Http.Error Login.Response -> Scenario
--                 }
--             }
--         }
-- scenario session =
--     { user =
--         { comment = Scenario.userComment session
--         , changeLoginId =
--             \str ->
--                 Scenario.userEvent session
--                     ("Type \"" ++ str ++ "\" for Account ID field")
--                     (ChangeLoginId str)
--         , changePass =
--             \str ->
--                 Scenario.userEvent session
--                     ("Type \"" ++ str ++ "\" for Password field")
--                     (ChangeLoginPass str)
--         , clickSubmitLogin =
--             Scenario.userEvent session
--                 "Click \"Login\" submit button"
--                 ClickSubmitLogin
--         }
--     , system =
--         { comment = Scenario.systemComment session
--         , requestLogin =
--             \json ->
--                 Scenario.systemCommand session
--                     "Request login to server"
--                     (ExpBuilder.custom <| \command ->
--                         case command of
--                             RequestLogin _ login ->
--                                 if Login.toValue login == json then
--                                     ExpBuilder.pass
--                                 else
--                                     ExpBuilder.fail "thought the request body is equal to the expected JSON."
--                             _ ->
--                                 ExpBuilder.fail "thought the command is `RequestLogin`."
--                     )
--         }
--     , external =
--         { backend =
--             { comment = Scenario.externalComment "backend" session
--             , respondToLoginRequest =
--                 \resp ->
--                     Scenario.externalEvent "backend"
--                         session
--                         "Respond to login request"
--                         (ReceiveLoginResp resp)
--             }
--         }
--     }
-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("page_login--" ++ name)
