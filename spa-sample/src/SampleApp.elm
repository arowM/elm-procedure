module SampleApp exposing
    ( Command(..)
    , Event(..)
    , Memory
    , Page(..)
    , init
    , main
    , procedures
    )

import App.Route as Route
import App.Session as Session exposing (Session)
import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Http
import Json.Encode exposing (Value)
import Mixin.Html as Html exposing (Html)
-- import Page.Home as PageHome
import Page.Login as PageLogin
-- import Page.Users as PageUsers
import Procedure as AppProcedure
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer as Observer exposing (Observer)
import Procedure.ObserverId exposing (ObserverId)
import Url exposing (Url)



-- App


{-| -}
main : AppProcedure.Program Value Memory Event
main =
    AppProcedure.application
        { init = init
        , procedures =
            \flags url key ->
                procedures flags url key Observer.root
                    |> Procedure.mapCmds runCommand
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- Memory


type alias Memory =
    { page : Page
    }


init : Memory
init =
    { page = PageLoading
    }



-- Page


type Page
    = PageLoading
    | PageNotFound
    | PageLogin ( ObserverId, PageLogin.Memory )
    -- | PageHome ( ObserverId, PageHome.Memory )
    -- | PageUsers ( ObserverId, PageUsers.Memory )



-- Subscriptions


subscriptions : memory -> Sub msg
subscriptions _ =
    Sub.none



-- View


view : Memory -> Document (Msg Event)
view memory =
    { title = "Sample App"
    , body =
        [ case memory.page of
            PageLoading ->
                pageLoadingView

            PageNotFound ->
                pageNotFoundView

            PageLogin pageLogin ->
                PageLogin.view pageLogin
                    |> Html.map (Procedure.mapMsg PageLoginEvent)

            {-
            PageHome pageHome ->
                VPack.child
                    app
                    PageHomeEvent
                    (\_ -> PageHome.view)
                    pageHome

            PageUsers pageUsers ->
                VPack.child
                    app
                    PageUsersEvent
                    (\_ -> PageUsers.view)
                    pageUsers
            -}
        ]
    }



-- -- PageLoading


pageLoadingView : Html msg
pageLoadingView =
    Html.text "Loading..."



-- -- PageNotFound


pageNotFoundView : Html msg
pageNotFoundView =
    Html.text "Not found"



-- Procedures


{-| -}
type Event
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | PageLoginEvent PageLogin.Event
    -- | PageHomeEvent PageHome.Event
    -- | PageUsersEvent PageUsers.Event
    | ReceiveSession (Result Http.Error Session)


{-| Abstructed Commands, which enables dependency injection.
-}
type Command e
    = PageLoginCommand (PageLogin.Command e)
    -- | PageHomeCommand (PageHome.Command e)
    -- | PageUsersCommand (PageUsers.Command e)
    | SessionCommand (Session.Command e)
    | PushUrl Key String
    | Load String


{-| Run abstructed Commands as actual application Commands.
-}
runCommand : Command Event -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        PageLoginCommand c ->
            PageLogin.runCommand c

        -- PageHomeCommand c ->
        --     PageHome.runCommand c

        -- PageUsersCommand c ->
        --     PageUsers.runCommand c

        SessionCommand c ->
            Session.runCommand c

        PushUrl key url ->
            Nav.pushUrl key url

        Load url ->
            Nav.load url



-- -- Initialization


{-| -}
procedures : Value -> Url -> Key -> Observer m Memory -> List (Procedure (Command Event) m Event)
procedures _ url key app =
    [ Procedure.async <| linkControllProcedures key app
    , Procedure.async <| pageControllProcedures url key Nothing app
    ]



-- -- Link Controller


{-| Handle link-click events.
-}
linkControllProcedures :
    Key
    -> Observer m Memory
    -> List (Procedure (Command Event) m Event)
linkControllProcedures key app =
    [ Procedure.await app <|
        \event _ ->
            case event of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            [ Procedure.push app <|
                                \_ ->
                                    Url.toString url
                                        |> PushUrl key
                            ]

                        Browser.External href ->
                            [ Procedure.push app <|
                                \_ ->
                                    Load href
                            ]

                _ ->
                    -- Ignore other events
                    []

    -- Call self recursively
    , Procedure.jump app <| \_ -> linkControllProcedures key app
    ]



-- -- Page Controller


pageControllProcedures :
    Url
    -> Key
    -> Maybe Session
    -> Observer m Memory
    -> List (Procedure (Command Event) m Event)
pageControllProcedures url key msession app =
    case ( Route.fromUrl url, msession ) of
        ( Route.NotFound, _ ) ->
            [ Procedure.modify app <|
                \m ->
                    { m | page = PageNotFound }
            , Procedure.await app <|
                \event _ ->
                    case event of
                        UrlChanged newUrl ->
                            [ Procedure.jump app <|
                                \_ ->
                                    pageControllProcedures newUrl key msession app
                            ]

                        _ ->
                            []
            ]

        ( _, Nothing ) ->
            [ Session.fetch app
                SessionCommand
                ReceiveSession
            , Procedure.await app <|
                \event _ ->
                    case event of
                        ReceiveSession (Err _) ->
                            [ Procedure.observe PageLogin.init <|
                                \pageLoginCore ->
                                    let
                                        pageLogin =
                                            pageLoginObserver
                                                (Tuple.first pageLoginCore)
                                                app
                                    in
                                    [ Procedure.modify app <|
                                        \m -> { m | page = PageLogin pageLoginCore }
                                    , Procedure.async
                                        (PageLogin.procedures url key pageLogin)
                                            |> runPageLoginProcedure
                                    , Procedure.await app <|
                                        \event2 appMemory ->
                                            case event2 of
                                                UrlChanged newUrl ->
                                                    [ Procedure.jump app <|
                                                        \_ ->
                                                            pageControllProcedures newUrl key (extractSession appMemory) app
                                                    ]

                                                _ ->
                                                    []
                                    ]
                            ]

                        ReceiveSession (Ok session) ->
                            [ Procedure.jump app <|
                                \_ ->
                                    pageControllProcedures url key (Just session) app
                            ]

                        _ ->
                            []
            ]

        _ -> Debug.todo ""
        {-
        ( Route.Home, Just session ) ->
            [ Procedure.observe (PageHome.init session) <|
                \pageHomeCore ->
                    let
                        pageHome =
                            pageHomeObserver
                                (Tuple.first pageHomeCore)
                                app
                    in
                    [ Procedure.modify app <|
                        \m -> { m | page = PageHome pageHomeCore }
                    , Procedure.async
                        (PageHome.procedures key pageHome
                            |> Procedure.mapCmds PageHomeCommand
                        )
                    , Procedure.await app <|
                        \event _ ->
                            case event of
                                UrlChanged newUrl ->
                                    [ Procedure.jump app <|
                                        \appMemory ->
                                            pageControllProcedures newUrl key (extractSession appMemory) app
                                    ]

                                _ ->
                                    []
                    ]
            ]

        ( Route.Users, Just session ) ->
            [ Procedure.observe (PageUsers.init session) <|
                \pageUsersCore ->
                    let
                        pageUsers =
                            pageUsersObserver
                                (Tuple.first pageUsersCore)
                                app
                    in
                    [ Procedure.modify app <|
                        \m -> { m | page = PageUsers pageUsersCore }
                    , Procedure.async
                        (PageUsers.procedures key pageUsers
                            |> Procedure.mapCmds PageUsersCommand
                        )
                    , Procedure.await app <|
                        \event _ ->
                            case event of
                                UrlChanged newUrl ->
                                    [ Procedure.jump app <|
                                        \appMemory ->
                                            pageControllProcedures newUrl key (extractSession appMemory) app
                                    ]

                                _ ->
                                    []
                    ]
            ]
            -}


runPageLoginProcedure : Procedure (PageLogin.Command PageLogin.Event) m PageLogin.Event
    -> Procedure (Command Event) m Event
runPageLoginProcedure =
    Procedure.wrapEvent
        { wrap = PageLoginEvent
        , unwrap =
            \e ->
                case e of
                    PageLoginEvent e1 ->
                        Just e1
                    _ ->
                        Nothing
        }
        >> Procedure.mapCmd (PageLoginCommand << PageLogin.mapCommand PageLoginEvent)


pageLoginObserver :
    ObserverId
    -> Observer m Memory
    -> Observer m PageLogin.Memory
pageLoginObserver oid =
    Observer.child
        { mget =
            \m1 ->
                case m1.page of
                    PageLogin m2 ->
                        Just m2

                    _ ->
                        Nothing
        , set =
            \m2 m1 ->
                case m1.page of
                    PageLogin _ ->
                        { m1 | page = PageLogin m2 }

                    _ ->
                        m1
        }
        oid


{-
pageHomeObserver :
    ObserverId
    -> Observer m e Memory Event
    -> Observer m e PageHome.Memory PageHome.Event
pageHomeObserver oid =
    Observer.dig
        { get =
            \m1 ->
                case m1.page of
                    PageHome m2 ->
                        Just m2

                    _ ->
                        Nothing
        , set =
            \m2 m1 ->
                case m1.page of
                    PageHome _ ->
                        { m1
                            | page = PageHome m2
                        }

                    _ ->
                        m1
        , unwrap =
            \e1 ->
                case e1 of
                    PageHomeEvent e2 ->
                        Just e2

                    _ ->
                        Nothing
        , wrap = PageHomeEvent
        , id = oid
        }


pageUsersObserver :
    ObserverId
    -> Observer m e Memory Event
    -> Observer m e PageUsers.Memory PageUsers.Event
pageUsersObserver oid =
    Observer.dig
        { get =
            \m1 ->
                case m1.page of
                    PageUsers m2 ->
                        Just m2

                    _ ->
                        Nothing
        , set =
            \m2 m1 ->
                case m1.page of
                    PageUsers _ ->
                        { m1
                            | page = PageUsers m2
                        }

                    _ ->
                        m1
        , unwrap =
            \e1 ->
                case e1 of
                    PageUsersEvent e2 ->
                        Just e2

                    _ ->
                        Nothing
        , wrap = PageUsersEvent
        , id = oid
        }
-}


extractSession : Memory -> Maybe Session
extractSession memory =
    case memory.page of
        PageLoading ->
            Nothing

        PageNotFound ->
            Nothing

        PageLogin ( _, { msession } ) ->
            msession

        -- PageHome ( _, { session } ) ->
        --     Just session

        -- PageUsers ( _, { session } ) ->
        --     Just session
