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
import Html.Attributes exposing (target)
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer as Observer exposing (Observer)
import Procedure.ObserverId as ObserverId exposing (ObserverId)
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
    { items : List ( ObserverId, ToastItemMemory )
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
pushWarning : String -> Observer m Memory -> Procedure (Command Event) m Event
pushWarning =
    pushItem WarningMessage


{-| Show error message.
-}
pushError : String -> Observer m Memory -> Procedure (Command Event) m Event
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Observer m Memory -> Procedure (Command Event) m Event
pushItem type_ str widget =
    let
        newItem =
            { isHidden = False
            , messageType = type_
            , content = str
            }
    in
    Procedure.observe newItem <|
        \( oid, newItemMemory ) ->
            let
                toastItem =
                    toastItemObserver oid widget
            in
            [ Procedure.modify widget <|
                \(Memory m) ->
                    Memory
                        { m
                            | items = m.items ++ [ ( oid, newItemMemory ) ]
                        }
            , Procedure.jump toastItem <|
                \_ ->
                    toastItemProcedures oid widget toastItem
            ]


toastItemObserver :
    ObserverId
    -> Observer m Memory
    -> Observer m ToastItemMemory
toastItemObserver target =
    Observer.digListElem
        { id = target
        , get = \(Memory m) -> m.items
        , set = \items (Memory m) -> Memory { m | items = items }
        }



-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedures :
    ObserverId
    -> Observer m Memory
    -> Observer m ToastItemMemory
    -> List (Procedure (Command Event) m Event)
toastItemProcedures oid widget toastItem =
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
                    | items = List.filter (\( id, _ ) -> id /= oid) m.items
                }
    ]


sleep :
    Float
    -> Observer m Memory
    -> Procedure (Command Event) m Event
sleep msec =
    Procedure.protected <|
        \priv ->
            [ Procedure.push priv <|
                \(oid, _) -> Sleep msec (Procedure.issue oid WakeUp)
            , Procedure.await priv <|
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
    -> Observer m Memory
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
    ( ObserverId, ToastItemMemory )
    -> ( String, Html (Msg Event) )
toastItemView ( oid, param ) =
    ( ObserverId.toString oid
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
            , Events.onClick (Procedure.issue oid CloseToastItem)
            ]
            [ Html.text "×"
            ]
        ]
    )



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class ("widget_ toast--" ++ name)
