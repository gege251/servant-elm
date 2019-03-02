module GetWithAHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)
import Url.Builder


getWithaheader : (Result Http.Error (String) -> msg) -> Maybe (String) -> Maybe (Int) -> String -> Int -> Cmd msg
getWithaheader toMsg header_myStringHeader header_MyIntHeader header_MyRequiredStringHeader header_MyRequiredIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (identity >> Http.header "myStringHeader") header_myStringHeader
                , Maybe.map (String.fromInt >> Http.header "MyIntHeader") header_MyIntHeader
                , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                , Maybe.map (String.fromInt >> Http.header "MyRequiredIntHeader") (Just header_MyRequiredIntHeader)
                ]
        , url =
            Url.Builder.crossOrigin ""
                [ "with-a-header"
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg string
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
