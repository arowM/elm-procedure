module Widget.Toast exposing
    ( Memory
    , init
    , view
    , Event
    , Command(..)
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
import Mixin.Html as Html exposing (Html)
import Mixin.Events as Events
import Procedure.Advanced as Procedure exposing (Procedure, Msg)
import Procedure.Observer as Observer exposing (Observer)
import Procedure.ObserverId as ObserverId exposing (ObserverId)
import Procedure.VPack as VPack exposing (VPack)
import Process
import Task
import Html.Attributes exposing (target)



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
    = ToastItemEvent ToastItemEvent
    | WakeUp


{-| -}
type Command e
    = Sleep Float (Msg e)


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        Sleep msec msg ->
            Process.sleep msec
                |> Task.perform (\() -> msg)


-- Methods


{-| -}
pushWarning : String -> Observer m e Memory Event -> Procedure (Command e) m e
pushWarning =
    pushItem WarningMessage


{-| -}
pushError : String -> Observer m e Memory Event -> Procedure (Command e) m e
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Observer m e Memory Event -> Procedure (Command e) m e
pushItem type_ str widget =
    let
        newItem =
            { isHidden = False
            , messageType = type_
            , content = str
            }
    in
    Procedure.observe newItem <|
        \(oid, newItemMemory) ->
            let
                toastItem = toastItemObserver oid widget
            in
            [ Procedure.modify widget <|
                \(Memory m) ->
                    Memory
                        { m
                            | items = m.items ++ [ (oid, newItemMemory) ]
                        }
            , Procedure.jump toastItem <| \_ ->
                toastItemProcedures oid widget toastItem
            ]


toastItemObserver :
    ObserverId
    -> Observer m e Memory Event
    -> Observer m e ToastItemMemory ToastItemEvent
toastItemObserver target =
    Observer.digListElem
        { id = target
        , get = \(Memory m) -> m.items
        , set = \items (Memory m) -> Memory { m | items = items }
        , unwrap = \e1 ->
            case e1 of
                ToastItemEvent e2 ->
                    Just e2
                _ ->
                    Nothing
        , wrap = ToastItemEvent
        }


-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


type ToastItemEvent
    = CloseItem


toastItemProcedures :
    ObserverId
    -> Observer m e Memory Event
    -> Observer m e ToastItemMemory ToastItemEvent
    -> List (Procedure (Command e) m e)
toastItemProcedures oid widget toastItem =
    [ Procedure.race
        [ sleep toastTimeout widget
        , Procedure.await toastItem <|
            \event _ ->
                case event of
                    CloseItem ->
                        [ Procedure.none
                        ]

        ]
    , Procedure.modify toastItem <|
        \m -> { m | isHidden = True }
    , sleep toastDisappearingDuration widget
    , Procedure.modify widget <|
        \(Memory m) -> Memory
            { m |
                items = List.filter (\(id, _) -> id /= oid) m.items
            }
    ]


sleep :
    Float
    -> Observer m e Memory Event
    -> Procedure (Command e) m e
sleep msec widget =
    Procedure.protected widget <|
        \priv ->
            [ Procedure.push priv <|
                \_ issue -> Sleep msec (issue WakeUp)
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


{-| -}
pushHttpError :
    Http.Error
    -> Observer m e Memory Event
    -> Procedure (Command e) m e
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
view :
    VPack e Memory Event
    -> Html (Msg e)
view widget =
    let
        (Memory param) = VPack.memory widget
    in
    Html.keyed "div"
        [ localClass "toast"
        , Mixin.style "--zindex" <| String.fromInt ZIndex.toast
        ]
        ( List.map
            (VPack.child widget ToastItemEvent toastItemView)
            param.items
        )


toastItemView :
    ObserverId
    -> VPack e ToastItemMemory ToastItemEvent
    -> (String, Html (Msg e))
toastItemView oid toastItem =
    let
        param = VPack.memory toastItem
    in
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
            , Events.onClick (VPack.issue toastItem CloseItem)
            ]
            [ Html.text "×"
            ]
        ]
    )


-- Helper functions

localClass : String -> Mixin msg
localClass name =
    Mixin.class ("widget_ toast--" ++ name)
