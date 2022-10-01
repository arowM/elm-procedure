module App exposing
    ( Command(..)
    , Event
    , Memory
    , Page(..)
    , init
    , main
    , procedures
    , scenario
    , page
    )

-- import Page.Users as PageUsers

import App.Route as Route
import App.Session as Session exposing (Session)
import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Http
import Json.Encode exposing (Value)
import Mixin.Html as Html exposing (Html)
import Page.Login as PageLogin
import Page.Home as PageHome
import Procedure as AppProcedure
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Channel exposing (Channel)
import Procedure.Scenario as Scenario
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
    | PageLogin ( Channel, PageLogin.Memory )
    | PageHome ( Channel, PageHome.Memory )


-- | PageUsers ( Channel, PageUsers.Memory )
-- View


view : (Channel, Memory) -> Document (Msg Event)
view (_, memory) =
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

            PageHome pageHome ->
                PageHome.view pageHome
                    |> Html.map (Procedure.mapMsg PageHomeEvent)

        {-
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
    | PageHomeEvent PageHome.Event
      -- | PageUsersEvent PageUsers.Event
    | ReceiveSession (Result Http.Error Session)


{-| Abstructed Commands, which enables dependency injection.
-}
type Command
    = PageLoginCommand PageLogin.Command
    | PageHomeCommand PageHome.Command
      -- | PageUsersCommand PageUsers.Command
    | FetchSession (Result Http.Error Session -> Msg Event)
    | PushUrl Key String
    | Load String


{-| Run abstructed Commands as actual application Commands.
-}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        PageLoginCommand c ->
            PageLogin.runCommand c
                |> Cmd.map (Procedure.mapMsg PageLoginEvent)

        PageHomeCommand c ->
            PageHome.runCommand c
                |> Cmd.map (Procedure.mapMsg PageHomeEvent)
        -- PageUsersCommand c ->
        --     PageUsers.runCommand c

        FetchSession toMsg ->
            Session.fetch toMsg

        PushUrl key url ->
            Nav.pushUrl key url

        Load url ->
            Nav.load url



-- -- Initialization


{-| -}
procedures : Value -> Url -> Key -> List (Procedure Command Memory Event)
procedures _ url key =
    [ Procedure.async <| linkControllProcedures key
    , Procedure.async <| pageControllProcedures url key Nothing
    ]



-- -- Link Controller


{-| Handle link-click events.
-}
linkControllProcedures :
    Key
    -> List (Procedure Command Memory Event)
linkControllProcedures key =
    [ Procedure.await <|
        \event _ ->
            case event of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            [ Procedure.push <|
                                \_ _ ->
                                    Url.toString url
                                        |> PushUrl key
                            ]

                        Browser.External href ->
                            [ Procedure.push <|
                                \_ _ ->
                                    Load href
                            ]

                _ ->
                    -- Ignore other events
                    []

    -- Call self recursively
    , Procedure.jump <| \_ -> linkControllProcedures key
    ]



-- -- Page Controller


pageControllProcedures :
    Url
    -> Key
    -> Maybe Session
    -> List (Procedure Command Memory Event)
pageControllProcedures url key msession =
    case ( Route.fromUrl url, msession ) of
        ( Route.NotFound, _ ) ->
            [ Procedure.modify <|
                \m ->
                    { m | page = PageNotFound }
            , Procedure.await <|
                \event _ ->
                    case event of
                        UrlChanged newUrl ->
                            [ Procedure.jump <|
                                \_ ->
                                    pageControllProcedures newUrl key msession
                            ]

                        _ ->
                            []
            ]

        ( _, Nothing ) ->
            [ Procedure.push <|
                \_ toMsg -> FetchSession (toMsg << ReceiveSession)
            , Procedure.await <|
                \event _ ->
                    case event of
                        ReceiveSession (Err _) ->
                            [ Procedure.observe PageLogin.init <|
                                \pageLoginCore ->
                                    [ Procedure.modify <|
                                        \m -> { m | page = PageLogin pageLoginCore }
                                    , Procedure.async
                                        ( PageLogin.procedures url key
                                            |> List.map runPageLoginProcedure
                                        )
                                    , Procedure.await <|
                                        \event2 appMemory ->
                                            case event2 of
                                                UrlChanged newUrl ->
                                                    [ Procedure.jump <|
                                                        \_ ->
                                                            pageControllProcedures newUrl key (extractSession appMemory)
                                                    ]

                                                _ ->
                                                    []
                                    ]
                            ]

                        ReceiveSession (Ok session) ->
                            [ Procedure.jump <|
                                \_ ->
                                    pageControllProcedures url key (Just session)
                            ]

                        _ ->
                            []
            ]

        _ ->
            Debug.todo ""



{-
   ( Route.Home, Just session ) ->
       [ Procedure.observe (PageHome.init session) <|
           \pageHomeCore ->
               let
                   pageHome =
                       pageHomeModifier
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
                       pageUsersModifier
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


runPageLoginProcedure :
    Procedure PageLogin.Command PageLogin.Memory PageLogin.Event
    -> Procedure Command Memory Event
runPageLoginProcedure =
    Procedure.wrapEvent
        { wrap = PageLoginEvent
        , unwrap = \e ->
            case e of
                PageLoginEvent e1 -> Just e1
                _ -> Nothing
        }
        >> Procedure.mapCmd PageLoginCommand
            >> Procedure.liftMemory
                { get = \m ->
                    case m.page of
                        PageLogin (_, m1) -> Just m1
                        _ -> Nothing
                , modify = \f m ->
                    case m.page of
                        PageLogin (c, m1) ->
                            { m | page = PageLogin (c, f m1) }
                        _ ->
                            m
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

        PageHome ( _, { session } ) ->
            Just session

-- PageUsers ( _, { session } ) ->
--     Just session


-- Scenario

type alias Scenario =
    Scenario.Scenario Command Memory Event

scenario : Scenario.Session Command Memory Event ->
    { user :
        { comment : String -> Scenario
        , setUrl : Url -> Scenario
        }
    , system :
        { comment : String -> Scenario
        }
    , external :
        {}
    }
scenario session =
    { user =
        { comment = Scenario.userComment session
        , setUrl = \url ->
            Scenario.userEvent session
                ("Set URL: " ++ Url.toString url)
                (UrlChanged url)
        }
    , system =
        { comment = Scenario.systemComment session
        }
    , external =
        {}
    }


page :
    { login : Scenario.Page Command Memory Event PageLogin.Command PageLogin.Memory PageLogin.Event
    , home : Scenario.Page Command Memory Event PageHome.Command PageHome.Memory PageHome.Event
    }
page =
    { login =
        { memory =
            { get =
                \m ->
                    case m.page of
                        PageLogin a ->
                            Just a

                        _ ->
                            Nothing
            , set = \m1 m ->
                case m.page of
                    PageLogin (c, _) ->
                        { m | page = PageLogin (c, m1) }
                    _ ->
                        m
            }
        , event =
            { unwrap =
                \e ->
                    case e of
                        PageLoginEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            , wrap = PageLoginEvent
            }
        , command =
            { unwrap =
                \c ->
                    case c of
                        PageLoginCommand c1 ->
                            Just c1
                        _ ->
                            Nothing
            , wrap = PageLoginCommand
            }
        }
    , home =
        { memory =
            { get =
                \m ->
                    case m.page of
                        PageHome a ->
                            Just a

                        _ ->
                            Nothing
            , set = \m1 m ->
                case m.page of
                    PageHome (c, _) ->
                        { m | page = PageHome (c, m1) }
                    _ ->
                        m
            }
        , event =
            { unwrap =
                \e ->
                    case e of
                        PageHomeEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            , wrap = PageHomeEvent
            }
        , command =
            { unwrap =
                \c ->
                    case c of
                        PageHomeCommand c1 ->
                            Just c1
                        _ ->
                            Nothing
            , wrap = PageHomeCommand
            }
        }
    }
