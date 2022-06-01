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
import Procedure.Advanced as Procedure exposing (Msg, Procedure)
import Procedure.Observer exposing (Observer)
import Url.Builder as Url


{-| -}
type alias Session =
    { id : String
    }


{-| -}
fetch :
    (Result Http.Error Session -> e1)
    -> Observer m e m1 e1
    -> Procedure (Command e) m e
fetch toEvent observer =
    Procedure.push observer <|
        \_ issue ->
            FetchSession (issue << toEvent)


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
                    [
                    ]
        , expect =
            Http.expectJson toEvent decoder
        }
