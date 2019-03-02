module Generated.BooksApi exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String
import Url.Builder


type alias Book =
    { name : String
    }

decodeBook : Decoder Book
decodeBook =
    Json.Decode.succeed Book
        |> required "name" string

encodeBook : Book -> Json.Encode.Value
encodeBook x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        ]

postBooks : (Result Http.Error (Book) -> msg) -> Book -> Cmd msg
postBooks toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8000"
                [ "books"
                ]
                []
        , body =
            Http.jsonBody (encodeBook body)
        , expect =
            Http.expectJson toMsg decodeBook
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getBooks : (Result Http.Error (List (Book)) -> msg) -> Cmd msg
getBooks toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8000"
                [ "books"
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (list decodeBook)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getBooksByBookId : (Result Http.Error (Book) -> msg) -> Int -> Cmd msg
getBooksByBookId toMsg capture_bookId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8000"
                [ "books"
                , String.fromInt <| capture_bookId
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