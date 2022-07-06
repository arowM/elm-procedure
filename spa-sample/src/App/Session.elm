module App.Session exposing
    ( Session
    , fetch
    , Command(..)
    , runCommand
    )

{-|

@docs Session
@docs fetch
@docs Command
@docs runCommand

-}

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Procedure.Advanced as Procedure exposing (Modifier, Msg, Request)
import Url.Builder as Url


{-| Data for logged in user.
-}
type alias Session =
    { id : String
    }


{-| Fetch user information from the server.
-}
fetch :
    Modifier m m1
    -> Request cmd m e1 (Command e1) (Result Http.Error Session)
fetch =
    Procedure.request <|
        \_ publish ->
            FetchSession publish


{-| -}
type Command e
    = FetchSession (Result Http.Error Session -> Msg e)


{-| -}
runCommand : Command e -> Cmd (Msg e)
runCommand cmd =
    case cmd of
        FetchSession msg ->
            fetchSession msg


fetchSession : (Result Http.Error Session -> Msg e) -> Cmd (Msg e)
fetchSession toEvent =
    let
        decoder : JD.Decoder Session
        decoder =
            JD.succeed identity
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
                , "profile"
                ]
                []
        , body =
            Http.jsonBody <|
                JE.object
                    []
        , expect =
            Http.expectJson toEvent decoder
        }
