module GetBooksByTitleSource exposing (..)

import Http
import Url.Builder


getBooksByTitle : (Result Http.Error (Book) -> msg) -> String -> Cmd msg
getBooksByTitle toMsg capture_title =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "books"
                , capture_title
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg decodeBook
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
