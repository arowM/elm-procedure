module App exposing
    ( Command(..)
    , Event
    , Memory
    , Page(..)
    , init
    , main
    , procedure
    , scenario
    )

-- import Page.Users as PageUsers

import App.Route as Route
import App.Session exposing (Session)
import Browser exposing (Document)
import Browser.Navigation as Nav exposing (Key)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Mixin.Html as Html exposing (Html)
import Page.Home as PageHome
import Page.Login as PageLogin
import Tepa exposing (Key, Layer, Msg, Void)
import Tepa.Scenario.LayerQuery as LayerQuery
import Url exposing (Url)
import Url.Builder as Url



-- App


{-| -}
main : Tepa.Program Value Memory Event
main =
    Tepa.application
        { init = init
        , procedure =
            \flags url key ->
                procedure flags url key
                    |> Tepa.mapCmd runCommand
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
    | PageLogin (Layer PageLogin.Memory)
    | PageHome (Layer PageHome.Memory)



-- View


view : Layer Memory -> Document (Msg Event)
view =
    Tepa.layerDocument <|
        \memory ->
            { title = "Sample App"
            , body =
                [ case memory.page of
                    PageLoading ->
                        pageLoadingView

                    PageNotFound ->
                        pageNotFoundView

                    PageLogin pageLogin ->
                        PageLogin.view pageLogin
                            |> Html.map (Tepa.mapMsg PageLoginEvent)

                    PageHome pageHome ->
                        PageHome.view pageHome
                            |> Html.map (Tepa.mapMsg PageHomeEvent)
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


{-| Abstructed Commands, which enables dependency injection.
-}
type Command
    = PageLoginCommand PageLogin.Command
    | PageHomeCommand PageHome.Command
      -- | PageUsersCommand PageUsers.Command
    | FetchSession (Result Http.Error Value -> Msg Event)
    | PushUrl Key String
    | LoadPage String


{-| Run abstructed Commands as actual application Commands.
-}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        PageLoginCommand c ->
            PageLogin.runCommand c
                |> Cmd.map (Tepa.mapMsg PageLoginEvent)

        PageHomeCommand c ->
            PageHome.runCommand c
                |> Cmd.map (Tepa.mapMsg PageHomeEvent)

        FetchSession toMsg ->
            fetchSession toMsg

        PushUrl key url ->
            Tepa.runNavCmd
                (\k -> Nav.pushUrl k url)
                key

        LoadPage url ->
            Nav.load url


{-| Fetch user information from the server.
-}
fetchSession : (Result Http.Error Value -> msg) -> Cmd msg
fetchSession toMsg =
    Http.post
        { url =
            Url.absolute
                [ "api"
                , "profile"
                ]
                []
        , body =
            Http.jsonBody <|
                JE.object
                    []
        , expect =
            Http.expectJson toMsg JD.value
        }


type alias FetchSessionResponse =
    { session : Session
    }


fetchSessionResponseDecoder : Decoder FetchSessionResponse
fetchSessionResponseDecoder =
    let
        sessionDecoder : JD.Decoder Session
        sessionDecoder =
            JD.succeed Session
                |> JDP.required "id" JD.string
    in
    JD.succeed FetchSessionResponse
        |> JDP.required "profile" sessionDecoder


{-| -}
type alias Promise a =
    Tepa.Promise Command Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m



-- -- Initialization


{-| -}
procedure : Value -> Url -> Key -> Promise Void
procedure _ url key =
    Tepa.syncAll
        [ linkControllProcedure key
        , pageControllProcedure url key Nothing
        ]



-- -- Link Controller


{-| Handle link-click events.
-}
linkControllProcedure : Key -> Promise Void
linkControllProcedure key =
    Tepa.withLayerEvent <|
        \e ->
            case e of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            [ pushUrl key <| Url.toString url
                            , Tepa.lazy <|
                                \_ -> linkControllProcedure key
                            ]

                        Browser.External href ->
                            [ loadPage href
                            , Tepa.lazy <|
                                \_ -> linkControllProcedure key
                            ]

                _ ->
                    -- Await again when receive other events
                    []


pushUrl : Key -> String -> Promise Void
pushUrl key url =
    Tepa.push <| \_ -> PushUrl key url


loadPage : String -> Promise Void
loadPage url =
    Tepa.push <| \_ -> LoadPage url



-- -- Page Controller


pageControllProcedure :
    Url
    -> Key
    -> Maybe Session
    -> Promise Void
pageControllProcedure url key msession =
    case ( Route.fromUrl url, msession ) of
        ( Route.NotFound, _ ) ->
            Tepa.sequence
                [ Tepa.modify <|
                    \m ->
                        { m | page = PageNotFound }
                , Tepa.withLayerEvent <|
                    \e ->
                        case e of
                            UrlChanged newUrl ->
                                [ Tepa.lazy <|
                                    \_ ->
                                        pageControllProcedure newUrl key msession
                                ]

                            _ ->
                                []
                ]

        ( _, Nothing ) ->
            requestSession
                |> Tepa.andThen
                    (\response ->
                        case response of
                            Err _ ->
                                Tepa.putVariantLayer
                                    { get = .page
                                    , set = \v m -> { m | page = v }
                                    , wrap = PageLogin
                                    , unwrap =
                                        \m ->
                                            case m of
                                                PageLogin a ->
                                                    Just a

                                                _ ->
                                                    Nothing
                                    , init = PageLogin.init
                                    }
                                    |> Tepa.andThen
                                        (\pageLoginPointer ->
                                            Tepa.syncAll
                                                [ PageLogin.procedure url key
                                                    |> runPageLoginPromise pageLoginPointer
                                                , Tepa.withLayerEvent <|
                                                    \e2 ->
                                                        case e2 of
                                                            UrlChanged newUrl ->
                                                                [ runPageLoginPromise pageLoginPointer PageLogin.currentSession
                                                                    |> Tepa.andThen (pageControllProcedure newUrl key)
                                                                ]

                                                            _ ->
                                                                []
                                                ]
                                        )

                            Ok { session } ->
                                Tepa.lazy <|
                                    \_ ->
                                        pageControllProcedure url key (Just session)
                    )

        ( Route.Home, Just session ) ->
            Tepa.putVariantLayer
                { get = .page
                , set = \a m -> { m | page = a }
                , wrap = PageHome
                , unwrap =
                    \m ->
                        case m of
                            PageHome a ->
                                Just a

                            _ ->
                                Nothing
                , init = PageHome.init session
                }
                |> Tepa.andThen
                    (\pageHomePointer ->
                        Tepa.syncAll
                            [ PageHome.procedure key
                                |> runPageHomePromise pageHomePointer
                            , Tepa.withLayerEvent <|
                                \e ->
                                    case e of
                                        UrlChanged newUrl ->
                                            [ runPageHomePromise pageHomePointer PageHome.currentSession
                                                |> Tepa.andThen
                                                    (pageControllProcedure newUrl key << Just)
                                            ]

                                        _ ->
                                            []
                            ]
                    )

        _ ->
            Debug.todo ""


requestSession : Promise (Result Http.Error FetchSessionResponse)
requestSession =
    Tepa.httpRequest
        { name = "requestSession"
        , request = FetchSession
        , decoder = fetchSessionResponseDecoder
        }


runPageLoginPromise :
    Pointer PageLogin.Memory
    -> Tepa.Promise PageLogin.Command PageLogin.Memory PageLogin.Event a
    -> Promise a
runPageLoginPromise pointer prom =
    Tepa.onLayer pointer prom
        |> Tepa.liftEvent
            { wrap = PageLoginEvent
            , unwrap =
                \e ->
                    case e of
                        PageLoginEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            }
        |> Tepa.mapCmd PageLoginCommand


runPageHomePromise :
    Pointer PageHome.Memory
    -> Tepa.Promise PageHome.Command PageHome.Memory PageHome.Event a
    -> Promise a
runPageHomePromise pointer prom =
    Tepa.onLayer pointer prom
        |> Tepa.liftEvent
            { wrap = PageHomeEvent
            , unwrap =
                \e ->
                    case e of
                        PageHomeEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            }
        |> Tepa.mapCmd PageHomeCommand



-- Scenario


{-| -}
type alias ScenarioSet flags =
    { home : PageHome.ScenarioSet flags Command Memory Event
    }


{-| -}
scenario : ScenarioSet flags
scenario =
    { home =
        PageHome.scenario
            { querySelf =
                LayerQuery.self
                    |> LayerQuery.child
                        (\m ->
                            case m.page of
                                PageHome l ->
                                    Just l

                                _ ->
                                    Nothing
                        )
            , wrapEvent = PageHomeEvent
            , unwrapCommand =
                \c ->
                    case c of
                        PageHomeCommand c1 ->
                            Just c1

                        _ ->
                            Nothing
            }
    }
