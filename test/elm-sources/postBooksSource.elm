module PostBooksSource exposing (..)

import Http
import Url.Builder


postBooks : (Result Http.Error (NoContent) -> msg) -> Book -> Cmd msg
postBooks toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "books"
                ]
                []
        , body =
            Http.jsonBody (encodeBook body)
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
