module Generated.GiphyApi exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String
import Url.Builder


type alias Gif =
    { data : GifData
    }

type alias GifData =
    { image_url : String
    }

decodeGif : Decoder Gif
decodeGif =
    Json.Decode.succeed Gif
        |> required "data" decodeGifData

decodeGifData : Decoder GifData
decodeGifData =
    Json.Decode.succeed GifData
        |> required "image_url" string

getRandom : (Result Http.Error (Gif) -> msg) -> Maybe (String) -> Maybe (String) -> Cmd msg
getRandom toMsg query_api_key query_tag =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://api.giphy.com/v1/gifs"
                [ "random"
                ]
                (List.concat
                    [ [Url.Builder.string "api_key" (query_api_key |> Maybe.map identity |> Maybe.withDefault "")]
                    , [Url.Builder.string "tag" (query_tag |> Maybe.map identity |> Maybe.withDefault "")]
                    ])
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg decodeGif
        , timeout =
            Nothing
        , tracker =
            Nothing
        }