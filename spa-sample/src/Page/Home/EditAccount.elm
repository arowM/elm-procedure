module Page.Home.EditAccount exposing
    ( request
    , EditAccount
    , Response
    , Command(..)
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
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer exposing (Observer)
import Url.Builder as Url



-- Request


{-| Request server for login.
-}
request :
    EditAccount
    -> (Result Http.Error Response -> e1)
    -> Observer m e m1 e1
    -> Procedure (Command e) m e
request login toEvent observer =
    Procedure.push observer <|
        \_ issue ->
            RequestEditAccount login (issue << toEvent)


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
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        RequestEditAccount login msg ->
            requestEditAccount login msg


requestEditAccount : EditAccount -> (Result Http.Error Response -> Msg e) -> Cmd (Msg e)
requestEditAccount (EditAccount login) toEvent =
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
                    [ ( "id", JE.string login.id )
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
