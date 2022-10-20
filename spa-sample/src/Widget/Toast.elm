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
import Procedure as Procedure exposing (LayerId, Msg, Procedure)
import Procedure.Promise as Promise exposing (Promise)
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
    { items : List ( LayerId, ToastItemMemory )
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
    | WakeUp


{-| -}
type Command
    = Sleep Float (Msg Event)


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        Sleep msec msg ->
            Process.sleep msec
                |> Task.perform (\() -> msg)



-- Methods


{-| Show warning message.
-}
pushWarning : String -> Procedure Command Memory Event
pushWarning =
    pushItem WarningMessage


{-| Show error message.
-}
pushError : String -> Procedure Command Memory Event
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Procedure Command Memory Event
pushItem type_ str =
    let
        newItem =
            { isHidden = False
            , messageType = type_
            , content = str
            }
    in
    Procedure.observe newItem <|
        \( channel, newItemMemory ) ->
            [ Procedure.modify <|
                \(Memory m) ->
                    Memory
                        { m
                            | items = m.items ++ [ ( channel, newItemMemory ) ]
                        }
            , Procedure.asyncOn
                { get = \(Memory m) ->
                    List.filter (\(c, _) -> c == channel) m.items
                        |> List.head
                , set = \m1 (Memory m) ->
                    Memory
                        { m
                            | items =
                                List.map
                                    (\(c, a) ->
                                        if (c == channel) then
                                            (c, m1)
                                        else
                                            (c, a)
                                    )
                                    m.items
                        }
                }
                channel
                <|
                    \pointer ->
                        [ toastItemProcedures
                            |> Procedure.batch
                            |> Procedure.liftMemory pointer
                        , Procedure.modify <|
                            \(Memory m) ->
                                Memory
                                    { m
                                        | items = List.filter (\( c, _ ) -> c /= channel) m.items
                                    }
                        ]
            ]


-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedures : List (Procedure Command ToastItemMemory Event)
toastItemProcedures =
    [ Procedure.race
        [ sleep toastTimeout
        , Procedure.await <|
            \event _ ->
                case event of
                    CloseToastItem ->
                        [ Procedure.none
                        ]

                    _ ->
                        []
        ]
    , Procedure.modify <|
        \m -> { m | isHidden = True }
    , sleep toastDisappearingDuration
    ]


sleep :
    Float
    -> Procedure Command m Event
sleep msec =
    -- Procedure.protected creates sandbox,
    -- in which procedures pub/sub Events to/from their private Channel.
    Procedure.protected
        [ Procedure.push <|
            \_ toMsg -> Sleep msec (toMsg WakeUp)
        , Procedure.await <|
            \event _ ->
                case event of
                    WakeUp ->
                        [ Procedure.none
                        ]

                    _ ->
                        []
        ]



-- Utilities


{-| Helper function to show HTTP errors.
-}
pushHttpError :
    Http.Error
    -> Procedure Command Memory Event
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
view : Memory -> Html (Msg Event)
view (Memory param) =
    Html.keyed "div"
        [ localClass "toast"
        , Mixin.style "--zindex" <| String.fromInt ZIndex.toast
        ]
        (List.map toastItemView param.items)


toastItemView :
    ( Channel, ToastItemMemory )
    -> ( String, Html (Msg Event) )
toastItemView ( channel, param ) =
    ( Channel.toString channel
    , Html.div
        [ localClass "toast_item"
        , localClass <| "toast_item-" ++ messageTypeCode param.messageType
        , Mixin.attribute "role" "dialog"
        , Mixin.boolAttribute "aria-hidden" param.isHidden
        , Mixin.style "--disappearing-duration" (String.fromFloat toastDisappearingDuration ++ "ms")
        ]
        [ Html.div
            [ localClass "toast_item_body"
            ]
            [ Html.text param.content
            ]
        , Html.div
            [ localClass "toast_item_close"
            , Events.onClick (Procedure.publish channel CloseToastItem)
            ]
            [ Html.text "×"
            ]
        ]
    )



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("widget_ toast--" ++ name)
