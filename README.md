# Servant Elm

[![Build Status](https://travis-ci.org/gege251/servant-elm.svg?branch=master)](https://travis-ci.org/gege251/servant-elm)

Generate Elm functions to query your Servant API!

Elm type generation coutesy of [krisajenkins/elm-export](https://github.com/krisajenkins/elm-export).

## Installation

Servant Elm is [available on Hackage](http://hackage.haskell.org/package/servant-elm).

## Example

First, some language pragmas and imports.

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI)
```

We have some Haskell-defined types and our Servant API.

```haskell
data Book = Book
    { name :: String
    } deriving (Generic)

instance ElmType Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book
```

Now we can generate Elm functions to query the API:

```haskell
spec :: Spec
spec = Spec ["Generated", "MyApi"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Book)
             : toElmDecoderSource (Proxy :: Proxy Book)
             : generateElmForAPI  (Proxy :: Proxy BooksApi))

main :: IO ()
main = specsToDir [spec] "my-elm-dir"
```

Let's save this as `example.hs` and run it:

```
$ stack runghc example.hs
Writing: my-elm-dir/Generated/MyApi.elm
$
```

Here's what was generated:

```elm
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
```

See [`examples`](examples) for a complete usage example, or take a look at
[mattjbray/servant-elm-example-app](https://github.com/mattjbray/servant-elm-example-app)
for an example project using this library.

## Development

```
$ git clone https://github.com/mattjbray/servant-elm.git
$ cd servant-elm
$ stack test
$ stack test --flag servant-elm:integration
```

To build all examples:

```
$ make examples
```

To run an example:

```
$ cd examples/e2e-tests
$ elm-reactor
# Open http://localhost:8000/elm/Main.elm
```
