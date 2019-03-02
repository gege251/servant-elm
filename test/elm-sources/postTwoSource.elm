module PostTwoSource exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Encode
import Url.Builder


postTwo : (Result Http.Error (Maybe (Int)) -> msg) -> String -> Cmd msg
postTwo toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "two"
                ]
                []
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectJson toMsg (nullable int)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
