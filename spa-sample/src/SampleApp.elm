module SampleApp exposing
    ( Command(..)
    , Event(..)
    , Memory
    , Page(..)
    , Procedures
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
import Page.Catalog as PageCatalog
import Page.Home as PageHome
import Page.Login as PageLogin
import Page.Users as PageUsers
import Procedure as AppProcedure
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer as Observer exposing (Observer)
import Procedure.ObserverId exposing (ObserverId)
import Procedure.VPack as VPack
import Url exposing (Url)



-- App


{-| -}
main : AppProcedure.Program Value Memory Event
main =
    AppProcedure.application
        { init = init
        , procedures =
            \flags url key ->
                procedures flags url key
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
    | PageCatalog ( ObserverId, PageCatalog.Memory )
    | PageHome ( ObserverId, PageHome.Memory )
    | PageUsers ( ObserverId, PageUsers.Memory )



-- Subscriptions


subscriptions : memory -> Sub msg
subscriptions _ =
    Sub.none



-- View


view : Memory -> Document (Msg Event)
view memory =
    let
        app =
            VPack.root memory
    in
    { title = "Sample App"
    , body =
        [ case memory.page of
            PageLoading ->
                pageLoadingView

            PageNotFound ->
                pageNotFoundView

            PageLogin pageLogin ->
                VPack.child
                    app
                    PageLoginEvent
                    (\_ -> PageLogin.view)
                    pageLogin

            PageCatalog pageCatalog ->
                VPack.child
                    app
                    PageCatalogEvent
                    (\_ -> PageCatalog.view)
                    pageCatalog

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
type alias Procedures m e =
    List (Procedure (Command e) m e)


{-| -}
type Event
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | PageLoginEvent PageLogin.Event
    | PageCatalogEvent PageCatalog.Event
    | PageHomeEvent PageHome.Event
    | PageUsersEvent PageUsers.Event
    | ReceiveSession (Result Http.Error Session)


{-| Abstructed Commands, which enables dependency injection.
-}
type Command e
    = PageLoginCommand (PageLogin.Command e)
    | PageCatalogCommand (PageCatalog.Command e)
    | PageHomeCommand (PageHome.Command e)
    | PageUsersCommand (PageUsers.Command e)
    | SessionCommand (Session.Command e)
    | PushUrl Key String
    | Load String


{-| Run abstructed Commands as actual application Commands.
-}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        PageLoginCommand c ->
            PageLogin.runCommand c

        PageCatalogCommand c ->
            PageCatalog.runCommand c

        PageHomeCommand c ->
            PageHome.runCommand c

        PageUsersCommand c ->
            PageUsers.runCommand c

        SessionCommand c ->
            Session.runCommand c

        PushUrl key url ->
            Nav.pushUrl key url

        Load url ->
            Nav.load url



-- -- Initialization


{-| -}
procedures : Value -> Url -> Key -> Procedures Memory Event
procedures _ url key =
    let
        app =
            Observer.root
    in
    [ Procedure.async <| linkControllProcedures key app
    , Procedure.async <| pageControllProcedures url key Nothing app
    ]



-- -- Link Controller


{-| Handle link-click events.
-}
linkControllProcedures :
    Key
    -> Observer m e Memory Event
    -> Procedures m e
linkControllProcedures key app =
    [ Procedure.await app <|
        \event _ ->
            case event of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            [ Procedure.push app <|
                                \_ _ ->
                                    Url.toString url
                                        |> PushUrl key
                            ]

                        Browser.External href ->
                            [ Procedure.push app <|
                                \_ _ ->
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
    -> Observer m e Memory Event
    -> Procedures m e
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

        ( Route.Catalog, _ ) ->
            [ Procedure.observe PageCatalog.init <|
                \pageCatalogCore ->
                    let
                        pageCatalog =
                            pageCatalogObserver
                                (Tuple.first pageCatalogCore)
                                app
                    in
                    [ Procedure.modify app <|
                        \m -> { m | page = PageCatalog pageCatalogCore }
                    , Procedure.async
                        (PageCatalog.procedures key pageCatalog
                            |> Procedure.mapCmds PageCatalogCommand
                        )
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
            ]

        ( _, Nothing ) ->
            [ Session.fetch ReceiveSession app
                |> Procedure.mapCmd SessionCommand
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
                                        (PageLogin.procedures url key pageLogin
                                            |> Procedure.mapCmds PageLoginCommand
                                        )
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


pageLoginObserver :
    ObserverId
    -> Observer m e Memory Event
    -> Observer m e PageLogin.Memory PageLogin.Event
pageLoginObserver oid =
    Observer.dig
        { get =
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
                        { m1
                            | page = PageLogin m2
                        }

                    _ ->
                        m1
        , unwrap =
            \e1 ->
                case e1 of
                    PageLoginEvent e2 ->
                        Just e2

                    _ ->
                        Nothing
        , wrap = PageLoginEvent
        , id = oid
        }


pageCatalogObserver :
    ObserverId
    -> Observer m e Memory Event
    -> Observer m e PageCatalog.Memory PageCatalog.Event
pageCatalogObserver oid =
    Observer.dig
        { get =
            \m1 ->
                case m1.page of
                    PageCatalog m2 ->
                        Just m2

                    _ ->
                        Nothing
        , set =
            \m2 m1 ->
                case m1.page of
                    PageCatalog _ ->
                        { m1
                            | page = PageCatalog m2
                        }

                    _ ->
                        m1
        , unwrap =
            \e1 ->
                case e1 of
                    PageCatalogEvent e2 ->
                        Just e2

                    _ ->
                        Nothing
        , wrap = PageCatalogEvent
        , id = oid
        }


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


extractSession : Memory -> Maybe Session
extractSession memory =
    case memory.page of
        PageLoading ->
            Nothing

        PageNotFound ->
            Nothing

        PageLogin ( _, { msession } ) ->
            msession

        PageCatalog _ ->
            Nothing

        PageHome ( _, { session } ) ->
            Just session

        PageUsers ( _, { session } ) ->
            Just session
