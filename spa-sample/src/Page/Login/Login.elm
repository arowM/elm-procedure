module Page.Login.Login exposing
    ( request
    , Login
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
@docs Login
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
import Procedure.Advanced as Procedure exposing (Msg, Observer, Request)
import Url.Builder as Url



-- Request


{-| Request server for login.
-}
request :
    Login
    -> Observer m m1
    -> Request cmd m e1 (Command e1) (Result Http.Error Response)
request login =
    Procedure.request <|
        \_ -> RequestLogin login


{-| Validated request-ready data.
-}
type Login
    = Login Login_


type alias Login_ =
    { id : String
    , pass : String
    }


{-| Response type for `request`.
-}
type alias Response =
    { session : Session
    }


{-| -}
type Command e
    = RequestLogin Login (Result Http.Error Response -> Msg e)


{-| -}
mapCommand : (e1 -> e0) -> Command e1 -> Command e0
mapCommand f cmd =
    case cmd of
        RequestLogin login toMsg ->
            RequestLogin login
                <| Procedure.mapMsg f << toMsg


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        RequestLogin login msg ->
            requestLogin login msg


requestLogin : Login -> (Result Http.Error Response -> Msg e) -> Cmd (Msg e)
requestLogin (Login login) toEvent =
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
                , "login"
                ]
                []
        , body =
            Http.jsonBody <|
                JE.object
                    [ ( "id", JE.string login.id )
                    , ( "pass", JE.string login.pass )
                    ]
        , expect =
            Http.expectJson toEvent decoder
        }



-- Form decoding


{-| Represents current form status, which can be invalid.
-}
type alias Form =
    { id : String
    , pass : String
    }


{-| Initial value.
-}
initForm : Form
initForm =
    { id = ""
    , pass = ""
    }


{-| Form validation errors.
-}
type FormError
    = IdRequired
    | PassRequired


{-| Format error to display UI.
-}
displayFormError : FormError -> String
displayFormError error =
    case error of
        IdRequired ->
            "ID is required."

        PassRequired ->
            "Password is required."


{-| Decode form.
-}
fromForm : Form -> Result (List FormError) Login
fromForm form =
    FD.run formDecoder form


{-|

    sample1 : Form
    sample1 =
        { id = ""
        , pass = "foo"
        }

    toFormErrors sample1
    --> [ IdRequired ]

    sample2 : Form
    sample2 =
        { id = "foo"
        , pass = ""
        }

    toFormErrors sample2
    --> [ PassRequired ]

    sample3 : Form
    sample3 =
        { id = "a"
        , pass = "2"
        }

    toFormErrors sample3
    --> []

-}
toFormErrors : Form -> List FormError
toFormErrors form =
    case fromForm form of
        Ok _ ->
            []

        Err errs ->
            errs


formDecoder : FD.Decoder Form FormError Login
formDecoder =
    FD.top Login_
        |> FD.field (FD.lift .id formIdDecoder)
        |> FD.field (FD.lift .pass formPassDecoder)
        |> FD.map Login


formIdDecoder : FD.Decoder String FormError String
formIdDecoder =
    FD.identity
        |> FD.assert (FD.minLength IdRequired 1)


formPassDecoder : FD.Decoder String FormError String
formPassDecoder =
    FD.identity
        |> FD.assert (FD.minLength PassRequired 1)
