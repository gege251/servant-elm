module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder


getBooks : (Result Http.Error (List (Book)) -> msg) -> Bool -> Maybe (String) -> Maybe (Int) -> String -> List (Bool) -> Cmd msg
getBooks toMsg query_published query_sort query_year query_category query_filters =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "books"
                ]
                (List.concat
                    [ [Url.Builder.string "published" query_published]
                    , [Url.Builder.string "sort" (query_sort |> identity |> Maybe.withDefault "")]
                    , [Url.Builder.string "year" (query_year |> String.fromInt |> Maybe.withDefault "")]
                    , [Url.Builder.string "category" query_category]
                    , List.map (Url.Builder.string "filters") (query_filters |> (\v -> if v then "True" else "False"))
                    ])
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (list decodeBook)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
