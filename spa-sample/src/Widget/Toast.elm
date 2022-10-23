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

-}

import App.ZIndex as ZIndex
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Tepa exposing (Layer, Msg, Promise, Void)
import Process
import Task



-- Constants


{-| How long it takes for the toast pop-up to disappear spontaneously in normal toasts.
-}
toastTimeout : Float
toastTimeout =
    10000


{-| The duration of the effect for disappearing a toast item.
-}
toastDisappearingDuration : Float
toastDisappearingDuration =
    250



-- Memory


{-| -}
type Memory
    = Memory Memory_


type alias Memory_ =
    { items : List ( Layer ToastItemMemory )
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
    | WakeUp ()


{-| -}
type Command
    = Sleep Float (() -> Msg Event)


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        Sleep msec toMsg ->
            Process.sleep msec
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
        { get = \getter (Memory m) ->
            List.filterMap getter m.items
                |> List.head
        , modify = \modifier (Memory m) ->
            Memory
                { m
                    | items = List.map modifier m.items
                }
        }
        newItem
        |> Tepa.andThenSequence
            (\(newItemLayer, itemPointer) ->
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
            ( \e ->
                case e of
                    CloseToastItem ->
                        [ Tepa.none
                        ]

                    _ ->
                        []
            )
            |> Tepa.andRace (sleep toastTimeout)
        , Tepa.modify
            (\m -> { m | isHidden = True })
        , sleep toastDisappearingDuration
        ]


sleep :
    Float
    -> Promise Command m Event Void
sleep msec =
    Tepa.customRequest
        { name = "sleep"
        , request = Sleep msec
        , wrap = WakeUp
        , unwrap = \e ->
            case e of
                WakeUp () -> Just ()
                _ -> Nothing
        }
        |> Tepa.void



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
        , Mixin.style "--disappearing-duration" (String.fromFloat toastDisappearingDuration ++ "ms")
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



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("widget_toast--" ++ name)
