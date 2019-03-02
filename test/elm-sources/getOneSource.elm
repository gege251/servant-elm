module GetOneSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder


getOne : (Result Http.Error (Int) -> msg) -> Cmd msg
getOne toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "one"
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg int
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
