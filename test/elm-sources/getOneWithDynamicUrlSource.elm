module GetOneWithDynamicUrlSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder


getOne : (Result Http.Error (Int) -> msg) -> String -> Cmd msg
getOne toMsg urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin urlBase
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
