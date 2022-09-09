module Scenario exposing (..)

import App as App exposing (Memory, Event, Command)
import Html exposing (Html)
import Page.Login as PageLogin
import Procedure.Scenario as Scenario exposing (Scenario)
import Test exposing (Test)
import Url


{-| Generate document for `scenarios`.
-}
main : Html msg
main =
    Scenario.toHtml
        { title = "Sample scenario"
        , sections = sections
        }


{-| Test for `scenarios`.
-}
test : Test
test =
    Scenario.toTest
        { init = App.init
        , sections = sections
        }


{-| -}
sections : List (Scenario.Section (Command Event) Memory Event)
sections =
        [ Scenario.section "introduction"
            [ Scenario.withNewSession introduction
            ]
        ]


introduction : Scenario.Session (Command Event) Memory Event -> List (Scenario (Command Event) Memory Event)
introduction session =
    let
        app = App.scenario session
    in
    [ app.user.comment "Hi. I'm Sakura-chan, the cutest goat girl in the world."
    , app.user.comment "Today I'll try a goat management service."
    , Scenario.fromJust "URL for home page" (Url.fromString "https://example.com/") <|
        \url ->
            [ app.user.comment "I'll try to access the URL given to me in advance."
            , app.system.comment "Load home page."
            , app.user.setUrl url
            ]
    , app.system.comment "Redirect to login page immediately."
    , Scenario.onPage session App.page.login <|
        \pageLoginSession ->
            let
                pageLogin = PageLogin.scenario pageLoginSession
            in
            [ pageLogin.user.comment "I see I need to log in! I'm getting my account info beforehand, too."
            , pageLogin.user.changeLoginId "guest"
            , pageLogin.user.changeLoginPass "guestpass"
            , pageLogin.user.clickSubmitLogin
            , pageLogin.system.requestBackendToLogin
            , pageLogin.external.backend.respondInvalidToLogin
            , pageLogin.system.comment "Toast popup: \"Invalid user name or password.\"."
            , pageLogin.user.comment "Oops!"
            , pageLogin.user.changeLoginPass "guestPass"
            , pageLogin.user.clickSubmitLogin
            , pageLogin.system.requestBackendToLogin
            , pageLogin.external.backend.respondValidToLogin
            ]
    , app.system.comment "Redirect to home page."
    , App.scenario.pageHome <| \_ -> [] -- Just ensure that current page is Home.
    , Scenario.cases
        [ Scenario.section "Home page #1" <|
            pageHomeCase1 session
        , Scenario.section "Home page #2" <|
            pageHomeCase2 session
        ]
    ]


pageHomeCase1 : Scenario.Session -> List (Scenario (Command Event) Memory Event)
pageHomeCase1 session =
    App.scenario.pageHome session <|
        \_ ->
            []


pageHomeCase2 : Scenario.Session -> List (Scenario (Command Event) Memory Event)
pageHomeCase2 session =
    App.scenario.app session <|
        \_ ->
            []
