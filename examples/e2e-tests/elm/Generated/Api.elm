module Generated.Api exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String
import Url.Builder


type alias Response =
    { origin : String
    }

decodeResponse : Decoder Response
decodeResponse =
    Json.Decode.succeed Response
        |> required "origin" string

type NoContent
    = NoContent

type alias MessageBody =
    { message : String
    }

encodeMessageBody : MessageBody -> Json.Encode.Value
encodeMessageBody x =
    Json.Encode.object
        [ ( "message", Json.Encode.string x.message )
        ]

decodeMessageBody : Decoder MessageBody
decodeMessageBody =
    Json.Decode.succeed MessageBody
        |> required "message" string

type alias ResponseWithJson =
    { json : MessageBody
    }

decodeResponseWithJson : Decoder ResponseWithJson
decodeResponseWithJson =
    Json.Decode.succeed ResponseWithJson
        |> required "json" decodeMessageBody

type alias QueryArgs =
    { q : String
    }

decodeQueryArgs : Decoder QueryArgs
decodeQueryArgs =
    Json.Decode.succeed QueryArgs
        |> required "q" string

type alias ResponseWithArgs =
    { args : QueryArgs
    }

decodeResponseWithArgs : Decoder ResponseWithArgs
decodeResponseWithArgs =
    Json.Decode.succeed ResponseWithArgs
        |> required "args" decodeQueryArgs

getIp : (Result Http.Error (Response) -> msg) -> Cmd msg
getIp toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "https://httpbin.org"
                [ "ip"
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg decodeResponse
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getStatus204 : (Result Http.Error (NoContent) -> msg) -> Cmd msg
getStatus204 toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "https://httpbin.org"
                [ "status"
                , "204"
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

postPost : (Result Http.Error (ResponseWithJson) -> msg) -> MessageBody -> Cmd msg
postPost toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "https://httpbin.org"
                [ "post"
                ]
                []
        , body =
            Http.jsonBody (encodeMessageBody body)
        , expect =
            Http.expectJson toMsg decodeResponseWithJson
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getGet : (Result Http.Error (ResponseWithArgs) -> msg) -> Maybe (String) -> Cmd msg
getGet toMsg query_q =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "https://httpbin.org"
                [ "get"
                ]
                (List.concat
                    [ [Url.Builder.string "q" (query_q |> Maybe.map identity |> Maybe.withDefault "")]
                    ])
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg decodeResponseWithArgs
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getByPath : (Result Http.Error (Response) -> msg) -> String -> Cmd msg
getByPath toMsg capture_path =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "https://httpbin.org"
                [ capture_path
                ]
                []
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg decodeResponse
        , timeout =
            Nothing
        , tracker =
            Nothing
        }