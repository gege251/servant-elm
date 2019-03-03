module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)
import Maybe.Extra
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
                    [ query_published
                        |> List.singleton
                        |> List.map (\v -> if v then "True" else "False")
                        |> List.map (Url.Builder.string "published")
                    , query_sort
                        |> Maybe.Extra.toList
                        |> List.map identity
                        |> List.map (Url.Builder.string "sort")
                    , query_year
                        |> Maybe.Extra.toList
                        |> List.map String.fromInt
                        |> List.map (Url.Builder.string "year")
                    , query_category
                        |> List.singleton
                        |> List.map (Url.Builder.string "category")
                    , query_filters
                        |> List.map (\v -> if v then "True" else "False")
                        |> List.map (Url.Builder.string "filters")
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
