module Scenario exposing (..)

import App as App exposing (Memory, Event, Command)
import Html exposing (Html)
import Http
import Json.Encode as JE
import Page.Login as PageLogin
import Page.Home as PageHome
import Procedure.Scenario as Scenario exposing (Scenario)
import Test exposing (Test)
import Url


{-| Generate document for `scenarios`.
-}
main : Html ()
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
sections : List (Scenario.Section Command Memory Event)
sections =
        [ Scenario.section "Introduction"
            [ Scenario.withNewSession introduction
            ]
        ]


introduction : Scenario.Session -> List (Scenario Command Memory Event)
introduction session =
    let
        app = App.scenario session
        pageLogin = PageLogin.scenario session
    in
    [ app.user.comment "Hi. I'm Sakura-chan, the cutest goat girl in the world."
    , app.user.comment "Today I'll try a goat management service."
    , Scenario.fromJust "URL for home page" (Url.fromString "https://example.com/") <|
        \url ->
            [ app.user.comment "I'll try to access the URL."
            , app.system.comment "Load home page."
            , app.user.setUrl url
            ]
    , app.system.comment "Redirect to login page immediately."
    , Scenario.onPage App.page.login
            [ pageLogin.user.comment "I see I need to log in! I remember my dad gave me the account information in advance."
            , pageLogin.user.changeLoginId "guest"
            , pageLogin.user.changePass "guestpass"
            , pageLogin.user.clickSubmitLogin
            , pageLogin.system.requestLogin <|
                JE.object
                    [ ("id", JE.string "guest" )
                    , ("pass", JE.string "guestpass" )
                    ]
            , pageLogin.external.backend.respondToLoginRequest <|
                Err (Http.BadStatus 401)
            , pageLogin.system.comment "Toast popup: \"Invalid user name or password.\"."
            , pageLogin.user.comment "Oops! It's hard to type with my two-fingered hooves..."
            , pageLogin.user.changePass "guestPass"
            , pageLogin.user.clickSubmitLogin
            , pageLogin.system.requestLogin <|
                JE.object
                    [ ("id", JE.string "guest" )
                    , ("pass", JE.string "guestPass" )
                    ]
            , pageLogin.external.backend.respondToLoginRequest <|
                Ok
                    { session =
                        { id = "guest"
                        }
                    }
            ]
    , app.system.comment "Redirect to home page."
    , Scenario.onPage App.page.home [] -- Just ensure that current page is Home.
    , Scenario.cases
        [ Scenario.section "Home page #1"
            [ Scenario.onPage App.page.home <|
                pageHomeCase1 session
            ]
        , Scenario.section "Home page #2"
            [ Scenario.onPage App.page.home <|
                pageHomeCase2 session
            ]
        ]
    ]


pageHomeCase1 : Scenario.Session -> List (Scenario PageHome.Command PageHome.Memory PageHome.Event)
pageHomeCase1 _ =
    [
    ]


pageHomeCase2 : Scenario.Session -> List (Scenario PageHome.Command PageHome.Memory PageHome.Event)
pageHomeCase2 _ =
    [
    ]
