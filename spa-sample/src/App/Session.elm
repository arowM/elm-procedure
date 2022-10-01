module App.Session exposing
    ( Session
    , fetch
    )

{-|

@docs Session
@docs fetch

-}

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Url.Builder as Url


{-| Data for logged in user.
-}
type alias Session =
    { id : String
    }


{-| Fetch user information from the server.
-}
fetch : (Result Http.Error Session -> msg) -> Cmd msg
fetch toMsg =
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
            Http.expectJson toMsg decoder
        }
