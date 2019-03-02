module GetWithAResponseHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder


getWitharesponseheader : (Result Http.Error (String) -> msg) -> Cmd msg
getWitharesponseheader toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "with-a-response-header"
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg string
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
