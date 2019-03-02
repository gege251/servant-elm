module GetNothingSource exposing (..)

import Http
import Url.Builder


getNothing : (Result Http.Error (NoContent) -> msg) -> Cmd msg
getNothing toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "nothing"
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\response ->
                    case response of
                        Http.GoodStatus_ _ "" ->
                            Ok NoContent
                        _ ->
                            Err (Http.BadBody "Expected the response body to be empty")
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
