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
            [ Scenario.withNewSession
                { userName = "Sakura-chan"
                }
                introduction
            ]
        ]


introduction : Session -> List (Scenario Command Memory Event)
introduction user1 =
    let
        user1Comment = Scenario.userComment user1
    in
    [ user1Comment "Hi. I'm Sakura-chan, the cutest goat girl in the world."
    , user1Comment "Today I'll try a goat management service."
    , user1Comment "I'll try to access the URL."
    , Scenario.loadApp
        { user = user1
        , route = routeHome
        }
        <| \appLayer1 ->
            let
                app1 = App.scenario appLayer1
            in
            [ app1.comment "Redirect to login page immediately."
            , user1Comment
                "I see I need to log in! I remember my dad gave me the account information in advance."
            , app1.pageLogin.user.changeLoginId "guest"
            , app1.pageLogin.user.changePass "guestpass"
            , app1.pageLogin.user.clickSubmitLogin
            , app1.pageLogin.system.requestLogin """{
  "id": "guest",
  "pass": "guestpass"
}"""
                (Err (Http.BadStatus 401))
                <| \_ -> []
            , app1.pageLogin.system.toastPopup "\"Invalid user name or password.\"."
            , user1Comment "Oops! It's hard to type with my two-fingered hooves..."
            , app1.pageLogin.user.changePass "guestPass"
            , app1.pageLogin.user.clickSubmitLogin
            , app1.pageLogin.system.requestLogin """{
  "id": "guest",
  "pass": "guestPass"
}"""
                (Ok """{
  "session": {
    "id": "guest"
  }
}""")
                <| \_ -> []
            ]
    , app1.system.comment "Redirect to home page."
    , Scenario.cases
        [ Scenario.section "Home page #1"
            [ pageHomeCase1 user1 app1
            ]
        , Scenario.section "Home page #2"
            [ pageHomeCase2 user1 app1
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
