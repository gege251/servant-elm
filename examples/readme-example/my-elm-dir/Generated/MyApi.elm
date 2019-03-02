module Generated.MyApi exposing (..)

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

getBooksByBookId : (Result Http.Error (Book) -> msg) -> Int -> Cmd msg
getBooksByBookId toMsg capture_bookId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
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