module PutNothingSource exposing (..)

import Http
import Url.Builder


putNothing : (Result Http.Error (()) -> msg) -> Cmd msg
putNothing toMsg =
    Http.request
        { method =
            "PUT"
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
                            Ok ()
                        _ ->
                            Err (Http.BadBody "Expected the response body to be empty")
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
