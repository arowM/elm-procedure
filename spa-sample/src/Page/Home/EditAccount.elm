module Page.Home.EditAccount exposing
    ( request
    , EditAccount
    , Response
    , Command(..)
    , mapCommand
    , runCommand
    , Form
    , initForm
    , FormError(..)
    , displayFormError
    , fromForm
    , toFormErrors
    )

{-| Module about login request.


# Request

@docs request
@docs EditAccount
@docs Response
@docs Command
@docs mapCommand
@docs runCommand


# Form decoding

If you are not familiar with the concept of _form decoding_, see [blog post](https://arow.info/posts/2019/form-decoding/).

@docs Form
@docs initForm
@docs FormError
@docs displayFormError
@docs fromForm
@docs toFormErrors

-}

import App.Session exposing (Session)
import Form.Decoder as FD
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Procedure.Advanced as Procedure exposing (Msg)
import Procedure.Observer exposing (Observer)
import Url.Builder as Url



-- Request


{-| Request server for login.
-}
request :
    EditAccount
    -> Observer m m1
    -> Request cmd m e1 (Command e1) (Result Http.Error Response)
request editAccount =
    Procedure.request <|
        \_ ->
            RequestEditAccount editAccount


{-| Validated request-ready data.
-}
type EditAccount
    = EditAccount EditAccount_


type alias EditAccount_ =
    { id : String
    }


{-| Response type for `request`.
-}
type alias Response =
    { session : Session
    }


{-| -}
type Command e
    = RequestEditAccount EditAccount (Result Http.Error Response -> Msg e)


{-| -}
mapCommand : (e1 -> e0) -> Command e1 -> Command e0
mapCommand f cmd =
    case cmd of
        RequestEditAccount editAccount toMsg ->
            RequestEditAccount editAccount <|
                Procedure.mapMsg f
                    << toMsg


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        RequestEditAccount login msg ->
            requestEditAccount login msg


requestEditAccount : EditAccount -> (Result Http.Error Response -> Msg e) -> Cmd (Msg e)
requestEditAccount (EditAccount editAccount) toEvent =
    let
        decoder : JD.Decoder Response
        decoder =
            JD.succeed Response
                |> JDP.required "profile" sessionDecoder

        sessionDecoder : JD.Decoder Session
        sessionDecoder =
            JD.succeed Session
                |> JDP.required "id" JD.string
    in
    Http.post
        { url =
            Url.absolute
                [ "api"
                , "account"
                ]
                []
        , body =
            Http.jsonBody <|
                JE.object
                    [ ( "id", JE.string editAccount.id )
                    ]
        , expect =
            Http.expectJson toEvent decoder
        }



-- Form decoding


{-| Represents current form status, which can be invalid.
-}
type alias Form =
    { id : String
    }


{-| Initial value.
-}
initForm : Form
initForm =
    { id = ""
    }


{-| Form validation errors.
-}
type FormError
    = IdRequired


{-| Format error to display UI.
-}
displayFormError : FormError -> String
displayFormError error =
    case error of
        IdRequired ->
            "ID is required."


{-| Decode form.
-}
fromForm : Form -> Result (List FormError) EditAccount
fromForm form =
    FD.run formDecoder form


{-|

    sample1 : Form
    sample1 =
        { id = ""
        }

    toFormErrors sample1
    --> [ IdRequired ]

    sample2 : Form
    sample2 =
        { id = "a"
        }

    toFormErrors sample2
    --> []

-}
toFormErrors : Form -> List FormError
toFormErrors form =
    case fromForm form of
        Ok _ ->
            []

        Err errs ->
            errs


formDecoder : FD.Decoder Form FormError EditAccount
formDecoder =
    FD.top EditAccount_
        |> FD.field (FD.lift .id formIdDecoder)
        |> FD.map EditAccount


formIdDecoder : FD.Decoder String FormError String
formIdDecoder =
    FD.identity
        |> FD.assert (FD.minLength IdRequired 1)
