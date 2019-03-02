module GetBooksByIdSource exposing (..)

import Http
import String
import Url.Builder


getBooksById : (Result Http.Error (Book) -> msg) -> Int -> Cmd msg
getBooksById toMsg capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "books"
                , String.fromInt <| capture_id
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
