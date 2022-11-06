module Widget.Toast exposing
    ( Memory
    , init
    , view
    , Event
    , Command
    , runCommand
    , pushWarning
    , pushError
    , pushHttpError
    , scenario
    , ScenarioSet
    )

{-| Widget for toast popup.


# Core

@docs Memory
@docs init
@docs view
@docs Event
@docs Command
@docs runCommand


# Methods

@docs pushWarning
@docs pushError


# Utilities

@docs pushHttpError


# Scenario

@docs scenario
@docs ScenarioSet

-}

import App.ZIndex as ZIndex
import Expect
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Process
import Task
import Tepa exposing (Layer, Msg, Promise, Void)
import Tepa.ResponseType as ResponseType
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Scenario.LayerQuery as LayerQuery exposing (LayerQuery)
import Test.Html.Query as HtmlQuery
import Test.Html.Selector as Selector



-- Constants


{-| How long it takes for the toast pop-up to disappear spontaneously in normal toasts.
-}
toastTimeout : Float
toastTimeout =
    10000


{-| The duration of the effect for disappearing a toast item.
-}
toastFadeOutDuration : Float
toastFadeOutDuration =
    250



-- Memory


{-| -}
type Memory
    = Memory Memory_


type alias Memory_ =
    { items : List (Layer ToastItemMemory)
    }


type MessageType
    = ErrorMessage
    | WarningMessage


messageTypeCode : MessageType -> String
messageTypeCode type_ =
    case type_ of
        ErrorMessage ->
            "error"

        WarningMessage ->
            "warning"


{-| -}
init : Memory
init =
    Memory
        { items = []
        }



-- Event


{-| -}
type Event
    = CloseToastItem


{-| -}
type Command
    = SetTimeoutOnItem (() -> Msg Event)
    | FadeOutItem (() -> Msg Event)


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        SetTimeoutOnItem toMsg ->
            Process.sleep toastTimeout
                |> Task.perform toMsg

        FadeOutItem toMsg ->
            Process.sleep toastFadeOutDuration
                |> Task.perform toMsg



-- Methods


{-| Show warning message.
-}
pushWarning : String -> Promise Command Memory Event Void
pushWarning =
    pushItem WarningMessage


{-| Show error message.
-}
pushError : String -> Promise Command Memory Event Void
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Promise Command Memory Event Void
pushItem type_ str =
    let
        newItem =
            { isHidden = False
            , messageType = type_
            , content = str
            }
    in
    Tepa.newLayer
        { get =
            \getter (Memory m) ->
                List.filterMap getter m.items
                    |> List.head
        , modify =
            \modifier (Memory m) ->
                Memory
                    { m
                        | items = List.map modifier m.items
                    }
        }
        newItem
        |> Tepa.andThenSequence
            (\( newItemLayer, itemPointer ) ->
                [ Tepa.modify <|
                    \(Memory m) ->
                        Memory { m | items = m.items ++ [ newItemLayer ] }
                , toastItemProcedure
                    |> Tepa.onLayer itemPointer
                , Tepa.modify <|
                    \(Memory m) ->
                        Memory
                            { m
                                | items =
                                    List.filter
                                        (not << Tepa.isPointedBy itemPointer)
                                        m.items
                            }
                ]
            )



-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedure : Promise Command ToastItemMemory Event Void
toastItemProcedure =
    Tepa.sequence
        [ Tepa.withLayerEvent
            (\e ->
                case e of
                    CloseToastItem ->
                        [ Tepa.none
                        ]
            )
            |> Tepa.andRace
                (Tepa.customRequest
                    { name = "Set time out"
                    , request = SetTimeoutOnItem
                    , responseType = ResponseType.unit
                    }
                    |> Tepa.void
                )
        , Tepa.modify
            (\m -> { m | isHidden = True })
        , Tepa.customRequest
            { name = "fade out item"
            , request = FadeOutItem
            , responseType = ResponseType.unit
            }
            |> Tepa.void
        ]



-- Utilities


{-| Helper function to show HTTP errors.
-}
pushHttpError :
    Http.Error
    -> Promise Command Memory Event Void
pushHttpError err =
    case err of
        Http.BadStatus 401 ->
            pushError """Login required."""

        Http.BadStatus 403 ->
            pushError """Operation not permitted."""

        Http.Timeout ->
            pushError """Network error, please try again."""

        Http.NetworkError ->
            pushError """Network error, please try again."""

        _ ->
            pushError """Internal error, please contact our support team."""



-- View


{-| -}
view : Layer Memory -> Html (Msg Event)
view =
    Tepa.layerView <|
        \(Memory memory) ->
            Html.keyed "div"
                [ localClass "toast"
                , Mixin.style "--zindex" <| String.fromInt ZIndex.toast
                ]
                (List.map (Tepa.keyedLayerView toastItemView) memory.items)


toastItemView : ToastItemMemory -> Html (Msg Event)
toastItemView memory =
    Html.div
        [ localClass "toast_item"
        , localClass <| "toast_item-" ++ messageTypeCode memory.messageType
        , Mixin.attribute "role" "dialog"
        , Mixin.boolAttribute "aria-hidden" memory.isHidden
        , Mixin.style "--disappearing-duration" (String.fromFloat toastFadeOutDuration ++ "ms")
        ]
        [ Html.div
            [ localClass "toast_item_body"
            ]
            [ Html.text memory.content
            ]
        , Html.div
            [ localClass "toast_item_close"
            , Events.onClick CloseToastItem
                |> Tepa.eventMixin
            ]
            [ Html.text "×"
            ]
        ]



-- Scenario


{-| -}
type alias ScenarioSet flags c m e =
    { expectWarningMessage : Scenario.Session -> String -> Scenario flags c m e
    , expectErrorMessage : Scenario.Session -> String -> Scenario flags c m e
    , expectNoWarningMessages : Scenario.Session -> Scenario flags c m e
    , expectNoErrorMessages : Scenario.Session -> Scenario flags c m e
    , expectNoMessages : Scenario.Session -> Scenario flags c m e
    , closeWarningsByMessage : Scenario.Session -> String -> Scenario flags c m e
    , closeErrorsByMessage : Scenario.Session -> String -> Scenario flags c m e
    , awaitAllToDisappear : Scenario.Session -> Scenario flags c m e
    }


type alias ScenarioProps c m e =
    { querySelf : LayerQuery m Memory
    , wrapEvent : Event -> e
    , unwrapCommand : c -> Maybe Command
    }


{-| -}
scenario : ScenarioProps c m e -> ScenarioSet flags c m e
scenario props =
    { expectWarningMessage =
        expectMessage WarningMessage
            "System shows toast popup warning message: "
    , expectErrorMessage =
        expectMessage ErrorMessage
            "System shows toast popup error message: "
    , expectNoWarningMessages =
        expectNoMessages
            ("toast_item-" ++ messageTypeCode WarningMessage)
            "No toast popup warning messages now."
    , expectNoErrorMessages =
        expectNoMessages
            ("toast_item-" ++ messageTypeCode ErrorMessage)
            "No toast popup error messages now."
    , expectNoMessages =
        expectNoMessages
            "toast_item"
            "No toast popup messages now."
    , closeWarningsByMessage =
        closeByMessage props
            WarningMessage
            "Click close button on toast popup with warning message: "
    , closeErrorsByMessage =
        closeByMessage props
            ErrorMessage
            "Click close button on toast popup with error message: "
    , awaitAllToDisappear = awaitAllToDisappear props
    }


expectMessage : MessageType -> String -> Scenario.Session -> String -> Scenario flags c m e
expectMessage messageType descPrefix session str =
    Scenario.expectAppView session
        (descPrefix ++ str)
        { expectation =
            \html ->
                HtmlQuery.fromHtml html
                    |> HtmlQuery.findAll
                        [ localClassSelector <| "toast_item-" ++ messageTypeCode messageType
                        ]
                    |> HtmlQuery.keep
                        (Selector.all
                            [ localClassSelector "toast_item_body"
                            , Selector.text str
                            ]
                        )
                    |> HtmlQuery.count (Expect.greaterThan 0)
        }


expectNoMessages : String -> String -> Scenario.Session -> Scenario flags c m e
expectNoMessages itemClassname desc session =
    Scenario.expectAppView session
        desc
        { expectation =
            \html ->
                HtmlQuery.fromHtml html
                    |> HtmlQuery.findAll
                        [ localClassSelector itemClassname
                        ]
                    |> HtmlQuery.count (Expect.equal 0)
        }


closeByMessage : ScenarioProps c m e -> MessageType -> String -> Scenario.Session -> String -> Scenario flags c m e
closeByMessage props messageType descPrefix session str =
    let
        target =
            props.querySelf
                |> LayerQuery.children
                    (\(Memory m) -> m.items)
                |> LayerQuery.filter
                    (\m ->
                        m.messageType
                            == messageType
                            && m.content
                            == str
                    )
                |> LayerQuery.index 0
    in
    Scenario.concat
        [ Scenario.userEvent session
            (descPrefix ++ str)
            { target = target
            , event = props.wrapEvent CloseToastItem
            }
        , Scenario.customResponse session
            "The popup is gradually fading away."
            { target = target
            , response =
                \cmd ->
                    case props.unwrapCommand cmd of
                        Just (FadeOutItem toMsg) ->
                            toMsg ()
                                |> Tepa.mapMsg props.wrapEvent
                                |> Just

                        _ ->
                            Nothing
            }
        ]


awaitAllToDisappear : ScenarioProps c m e -> Scenario.Session -> Scenario flags c m e
awaitAllToDisappear props session =
    let
        targets =
            props.querySelf
                |> LayerQuery.children
                    (\(Memory m) -> m.items)
    in
    Scenario.concat
        [ Scenario.customResponse session
            "After a period of time, each popup are automatically removed."
            { target = targets
            , response =
                \cmd ->
                    case props.unwrapCommand cmd of
                        Just (SetTimeoutOnItem toMsg) ->
                            toMsg ()
                                |> Tepa.mapMsg props.wrapEvent
                                |> Just

                        _ ->
                            Nothing
            }
        , Scenario.customResponse session
            "Popups gradually fade away when removed."
            { target = targets
            , response =
                \cmd ->
                    case props.unwrapCommand cmd of
                        Just (FadeOutItem toMsg) ->
                            toMsg ()
                                |> Tepa.mapMsg props.wrapEvent
                                |> Just

                        _ ->
                            Nothing
            }
        ]



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class (classPrefix ++ name)


localClassSelector : String -> Selector.Selector
localClassSelector name =
    Selector.class (classPrefix ++ name)


classPrefix : String
classPrefix =
    "widget_toast--"
