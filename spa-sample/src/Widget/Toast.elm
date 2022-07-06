module Widget.Toast exposing
    ( Memory
    , init
    , view
    , Event
    , Command(..)
    , mapCommand
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
@docs mapCommand
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
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Channel as Channel exposing (Channel)
import Procedure.Modifier as Modifier exposing (Modifier)
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
    { items : List ( Channel, ToastItemMemory )
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
type Command e
    = Sleep Float (Msg e)


{-| -}
mapCommand : (e1 -> e0) -> Command e1 -> Command e0
mapCommand f cmd =
    case cmd of
        Sleep msec msg ->
            Sleep msec (Procedure.mapMsg f msg)


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        Sleep msec msg ->
            Process.sleep msec
                |> Task.perform (\() -> msg)



-- Methods


{-| Show warning message.
-}
pushWarning : String -> Modifier m Memory -> Procedure (Command Event) m Event
pushWarning =
    pushItem WarningMessage


{-| Show error message.
-}
pushError : String -> Modifier m Memory -> Procedure (Command Event) m Event
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Modifier m Memory -> Procedure (Command Event) m Event
pushItem type_ str widget =
    let
        newItem =
            { isHidden = False
            , messageType = type_
            , content = str
            }
    in
    Procedure.observe newItem <|
        \( channel, newItemMemory ) ->
            let
                toastItem =
                    toastItemModifier channel widget
            in
            [ Procedure.modify widget <|
                \(Memory m) ->
                    Memory
                        { m
                            | items = m.items ++ [ ( channel, newItemMemory ) ]
                        }
            , Procedure.jump toastItem <|
                \_ ->
                    toastItemProcedures channel widget toastItem
            ]


toastItemModifier :
    Channel
    -> Modifier m Memory
    -> Modifier m ToastItemMemory
toastItemModifier expected =
    Modifier.listItem
        { get = \(Memory m) -> m.items
        , set = \items (Memory m) -> Memory { m | items = items }
        }
        expected



-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedures :
    Channel
    -> Modifier m Memory
    -> Modifier m ToastItemMemory
    -> List (Procedure (Command Event) m Event)
toastItemProcedures channel widget toastItem =
    [ Procedure.race
        [ sleep toastTimeout widget
        , Procedure.await toastItem <|
            \event _ ->
                case event of
                    CloseToastItem ->
                        [ Procedure.none
                        ]

                    _ ->
                        []
        ]
    , Procedure.modify toastItem <|
        \m -> { m | isHidden = True }
    , sleep toastDisappearingDuration widget
    , Procedure.modify widget <|
        \(Memory m) ->
            Memory
                { m
                    | items = List.filter (\( id, _ ) -> id /= channel) m.items
                }
    ]


sleep :
    Float
    -> Modifier m Memory
    -> Procedure (Command Event) m Event
sleep msec widget =
    -- Procedure.protected creates sandbox,
    -- in which procedures pub/sub Events to/from their private Channel.
    Procedure.protected
        [ Procedure.push widget <|
            \( channel, _ ) -> Sleep msec (Procedure.publish channel WakeUp)
        , Procedure.await widget <|
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
    -> Modifier m Memory
    -> Procedure (Command Event) m Event
pushHttpError err widget =
    case err of
        Http.BadStatus 401 ->
            pushError """Login required."""
                widget

        Http.BadStatus 403 ->
            pushError """Operation not permitted."""
                widget

        Http.Timeout ->
            pushError """Network error, please try again."""
                widget

        Http.NetworkError ->
            pushError """Network error, please try again."""
                widget

        _ ->
            pushError """Internal error, please contact our support team."""
                widget



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
