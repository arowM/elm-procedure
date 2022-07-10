module Scenario exposing (..)

import App as App exposing (Memory, Event, Command)
import Page.Login as PageLogin
import Procedure.Scenario as Scenario exposing (..)
import Url


scenarios : List (String, Scenario (Command Event) Memory Event)
scenarios =
    [ ( "Successfull scenario", introduction )
    ]


introduction = Scenario.batch
    [ userComment "Hi. I'm Sakura-chan, the cutest goat girl in the world."
    , userComment "Today I'll try a goat management service."
    , assertJust "URL for home page" (Url.fromString "https://example.com/") <|
        \url ->
            [ pushOperation (App.operation.loadPage url)
            ]
    , expectState App.state.onLoginPage
    , pushPageLoginEvent
        (PageLogin.operation.changeLoginId "guest")
    , pushPageLoginEvent
        (PageLogin.operation.changePass "guest")
    , pushPageLoginEvent
        (PageLogin.operation.clickSubmitLogin)
    , systemComment "ホーム画面にリダイレクト"
    ]


pushPageLoginEvent : Operation PageLogin.Event -> Scenario (Command Event) Memory Event
pushPageLoginEvent op =
    pushOperation (App.operation.pageLoginEvent op)
