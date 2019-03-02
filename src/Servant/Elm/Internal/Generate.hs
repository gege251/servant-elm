{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Elm.Internal.Generate where

import           Prelude                 hiding ( (<$>) )
import           Control.Lens                   ( to
                                                , (^.)
                                                )
import           Data.List                      ( nub )
import           Data.Maybe                     ( catMaybes )
import           Data.Proxy                     ( Proxy )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as L
import qualified Data.Text.Encoding            as T
import           Elm                            ( ElmDatatype(..)
                                                , ElmPrimitive(..)
                                                )
import qualified Elm
import           Servant.API                    ( NoContent(..) )
import           Servant.Elm.Internal.Foreign   ( LangElm
                                                , getEndpoints
                                                )
import           Servant.Elm.Internal.Orphans   ( )
import qualified Servant.Foreign               as F
import           Text.PrettyPrint.Leijen.Text


{-|
Options to configure how code is generated.
-}
data ElmOptions = ElmOptions
  { {- | The protocol, host and any path prefix to be used as the base for all
    requests.

    Example: @Static "https://mydomain.com/api/v1"@

    When @Dynamic@, the generated Elm functions take the base URL as the first
    argument.
    -}
    urlPrefix             :: UrlPrefix
  , elmExportOptions      :: Elm.Options
    -- ^ Options to pass to elm-export
  , emptyResponseElmTypes :: [ElmDatatype]
    -- ^ Types that represent an empty Http response.
  , stringElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a String.
  , intElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Int.
  , floatElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Float.
  , boolElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Bool.
  , charElmTypes        :: [ElmDatatype]
    -- ^ Types that represent a Char.
  }

data UrlPrefix
  = Static T.Text
  | Dynamic


{-|
Default options for generating Elm code.

The default options are:

> { urlPrefix =
>     Static ""
> , elmExportOptions =
>     Elm.defaultOptions
> , emptyResponseElmTypes =
>     [ toElmType NoContent ]
> , stringElmTypes =
>     [ toElmType "" ]
> , intElmTypes =
>     [ toElmType 0 ]
> , floatElmTypes =
>     [ toElmType 0 ]
> , boolElmTypes =
>     [ toElmType True ]
> , charElmTypes =
>     [ toElmType '' ]
> }
-}
defElmOptions :: ElmOptions
defElmOptions = ElmOptions
  { urlPrefix             = Static ""
  , elmExportOptions      = Elm.defaultOptions
  , emptyResponseElmTypes = [Elm.toElmType NoContent, Elm.toElmType ()]
  , stringElmTypes        = [ Elm.toElmType ("" :: String)
                            , Elm.toElmType ("" :: T.Text)
                            ]
  , intElmTypes           = [Elm.toElmType (0 :: Int)]
  , floatElmTypes         = [Elm.toElmType (0 :: Float)]
  , boolElmTypes          = [Elm.toElmType (False :: Bool)]
  , charElmTypes          = [Elm.toElmType (' ' :: Char)]
  }


{-|
Default imports required by generated Elm code.

You probably want to include this at the top of your generated Elm module.

The default required imports are:

> import Http
> import Json.Decode exposing (..)
> import Json.Decode.Pipeline exposing (..)
> import Json.Encode
> import String
> import Url.Builder
-}
defElmImports :: Text
defElmImports = T.unlines
  [ "import Http"
  , "import Json.Decode exposing (..)"
  , "import Json.Decode.Pipeline exposing (..)"
  , "import Json.Encode"
  , "import String"
  , "import Url.Builder"
  ]


{-|
Generate Elm code for the API with default options.

Returns a list of Elm functions to query your Servant API from Elm.

You could spit these out to a file and call them from your Elm code, but you
would be better off creating a 'Spec' with the result and using 'specsToDir',
which handles the module name for you.
-}
generateElmForAPI
  :: ( F.HasForeign LangElm ElmDatatype api
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api)
     )
  => Proxy api
  -> [Text]
generateElmForAPI = generateElmForAPIWith defElmOptions


{-|
Generate Elm code for the API with custom options.
-}
generateElmForAPIWith
  :: ( F.HasForeign LangElm ElmDatatype api
     , F.GenerateList ElmDatatype (F.Foreign ElmDatatype api)
     )
  => ElmOptions
  -> Proxy api
  -> [Text]
generateElmForAPIWith opts =
  nub . map docToText . map (generateElmForRequest opts) . getEndpoints

i :: Int
i = 4

{-|
Generate an Elm function for one endpoint.
-}
generateElmForRequest :: ElmOptions -> F.Req ElmDatatype -> Doc
generateElmForRequest opts request = funcDef
 where
  funcDef = vsep
    [ fnName <+> ":" <+> typeSignature
    , fnName <+> args <+> equals
    , indent i elmRequest
    ]

  fnName =
    request ^. F.reqFuncName . to (T.replace "-" "" . F.camelCase) . to stext

  typeSignature = mkTypeSignature opts request

  args          = mkArgs opts request

  elmRequest    = mkRequest opts request


mkTypeSignature :: ElmOptions -> F.Req ElmDatatype -> Doc
mkTypeSignature opts request = (hsep . punctuate " ->" . concat)
  [ toMsgType
  , catMaybes [urlPrefixType]
  , headerTypes
  , urlCaptureTypes
  , queryTypes
  , catMaybes [bodyType, returnType]
  ]
 where
  toMsgType :: [Doc]
  toMsgType =
    let maybeResult = fmap elmTypeRef $ request ^. F.reqReturnType
    in  case maybeResult of
          Just result ->
            [parens $ "Result Http.Error" <+> parens result <+> "-> msg"]
          Nothing -> [parens "Result Http.Error NoContent"]


  urlPrefixType :: Maybe Doc
  urlPrefixType = case (urlPrefix opts) of
    Dynamic  -> Just "String"
    Static _ -> Nothing

  elmTypeRef :: ElmDatatype -> Doc
  elmTypeRef eType = stext (Elm.toElmTypeRefWith (elmExportOptions opts) eType)

  headerTypes :: [Doc]
  headerTypes =
    [ header ^. F.headerArg . F.argType . to elmTypeRef
    | header <- request ^. F.reqHeaders
    , isNotCookie header
    ]

  urlCaptureTypes :: [Doc]
  urlCaptureTypes =
    [ F.captureArg capture ^. F.argType . to elmTypeRef
    | capture <- request ^. F.reqUrl . F.path
    , F.isCapture capture
    ]

  queryTypes :: [Doc]
  queryTypes =
    [ arg ^. F.queryArgName . F.argType . to elmTypeRef
    | arg <- request ^. F.reqUrl . F.queryStr
    ]

  bodyType :: Maybe Doc
  bodyType = fmap elmTypeRef $ request ^. F.reqBody

  returnType :: Maybe Doc
  returnType = pure "Cmd msg"


elmHeaderArg :: F.HeaderArg ElmDatatype -> Doc
elmHeaderArg header = "header_" <> header ^. F.headerArg . F.argName . to
  (stext . T.replace "-" "_" . F.unPathSegment)


elmCaptureArg :: F.Segment ElmDatatype -> Doc
elmCaptureArg segment =
  "capture_" <> F.captureArg segment ^. F.argName . to (stext . F.unPathSegment)


elmQueryArg :: F.QueryArg ElmDatatype -> Doc
elmQueryArg arg =
  "query_" <> arg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)


elmBodyArg :: Doc
elmBodyArg = "body"


isNotCookie :: F.HeaderArg f -> Bool
isNotCookie header = header ^. F.headerArg . F.argName . to
  ((/= "cookie") . T.toLower . F.unPathSegment)


mkArgs :: ElmOptions -> F.Req ElmDatatype -> Doc
mkArgs opts request =
  (hsep . concat)
    $ [ pure "toMsg"
      ,
     -- Dynamic url prefix
        case urlPrefix opts of
        Dynamic  -> ["urlBase"]
        Static _ -> []
      , -- Headers
        [ elmHeaderArg header
        | header <- request ^. F.reqHeaders
        , isNotCookie header
        ]
      , -- URL Captures
        [ elmCaptureArg segment
        | segment <- request ^. F.reqUrl . F.path
        , F.isCapture segment
        ]
      , -- Query params
        [ elmQueryArg arg | arg <- request ^. F.reqUrl . F.queryStr ]
      , -- Request body
        maybe [] (const [elmBodyArg]) (request ^. F.reqBody)
      ]


mkParams :: ElmOptions -> F.Req ElmDatatype -> Doc
mkParams opts request = if null (request ^. F.reqUrl . F.queryStr)
  then "[]"
  else parens $ "List.concat" <$> indent
    i
    (elmList $ map paramToDoc (request ^. F.reqUrl . F.queryStr))
 where
  paramToDoc :: F.QueryArg ElmDatatype -> Doc
  paramToDoc qarg = case qarg ^. F.queryArgType of
    F.Normal -> brackets
      ("Url.Builder.string" <+> dquotes name <+> if isMaybe
        then withDefault $ elmName <+> toStringSrc'
        else elmName <+> toStringSrc'
      )

    F.Flag -> brackets
      ("Url.Builder.string" <+> dquotes name <+> if isMaybe
        then withDefault $ parens $ elmName <+> toStringSrc'
        else parens $ elmName <+> toStringSrc'
      )

    F.List ->
      "List.map" <+> (parens $ "Url.Builder.string" <+> dquotes name) <+> parens
        (elmName <+> toStringSrc')
   where
    elmName = elmQueryArg qarg
    argType = qarg ^. F.queryArgName . F.argType
    toStringSrc'
      | isElmMaybeType argType = toStringSrcL "|> Maybe.map" opts argType
      | isElmListType argType  = toStringSrcL "|> List.map" opts argType
      | otherwise              = toStringSrcL "|>" opts argType
    isMaybe = isElmMaybeType argType
    name    = qarg ^. F.queryArgName . F.argName . to (stext . F.unPathSegment)
    withDefault value =
      parens $ value <+> "|> Maybe.withDefault" <+> dquotes empty


mkRequest :: ElmOptions -> F.Req ElmDatatype -> Doc
mkRequest opts request = "Http.request" <$> indent
  i
  (elmRecord
    [ "method =" <$> indent i (dquotes method)
    , "headers =" <$> indent i (elmListOfMaybes headers)
    , "url =" <$> indent i url
    , "body =" <$> indent i body
    , "expect =" <$> indent i expect
    , "timeout =" <$> indent i "Nothing"
    , "tracker =" <$> indent i "Nothing"
    ]
  )
 where
  method = request ^. F.reqMethod . to (stext . T.decodeUtf8)

  mkHeader header =
    let
      headerName =
        header ^. F.headerArg . F.argName . to (stext . F.unPathSegment)
      headerArgName = elmHeaderArg header
      argType       = header ^. F.headerArg . F.argType
      wrapped       = isElmMaybeType argType
      toStringSrc'  = toStringSrc ">>" opts argType
    in
      "Maybe.map"
      <+> parens (toStringSrc' <+> "Http.header" <+> dquotes headerName)
      <+> (if wrapped then headerArgName else parens ("Just" <+> headerArgName))

  headers =
    [ mkHeader header | header <- request ^. F.reqHeaders, isNotCookie header ]

  url  = mkUrl opts request

  body = case request ^. F.reqBody of
    Nothing -> "Http.emptyBody"

    Just elmTypeExpr ->
      let encoderName =
            Elm.toElmEncoderRefWith (elmExportOptions opts) elmTypeExpr
      in  "Http.jsonBody" <+> parens (stext encoderName <+> elmBodyArg)

  expect = case request ^. F.reqReturnType of
    Just elmTypeExpr
      | isEmptyType opts elmTypeExpr
      -> let elmConstructor =
               Elm.toElmTypeRefWith (elmExportOptions opts) elmTypeExpr
         in
           "Http.expectStringResponse toMsg" <$> indent
             i
             (parens
               (   backslash
               <>  "response"
               <+> "->"
               <$> indent
                     i
                     (   "case response of"
                     <$> indent i       "Http.GoodStatus_ _ \"\" ->"
                     <$> indent (2 * i) ("Ok" <+> stext elmConstructor)

                     <$> indent i       "_ ->"
                     <$> indent
                           (2 * i)
                           (   "Err"
                           <+> parens
                                 (   "Http.BadBody"
                                 <+> dquotes
                                       "Expected the response body to be empty"
                                 )
                           )
                     )
               <>  line
               )
             )


    Just elmTypeExpr -> "Http.expectJson toMsg"
      <+> stext (Elm.toElmDecoderRefWith (elmExportOptions opts) elmTypeExpr)

    Nothing -> error "mkHttpRequest: no reqReturnType?"


mkUrl :: ElmOptions -> F.Req ElmDatatype -> Doc
mkUrl opts request =
  "Url.Builder.crossOrigin"
    <+> urlBase
    <$> indent i segmentsDoc
    <$> indent i paramsDoc
 where
  urlBase :: Doc
  urlBase = case urlPrefix opts of
    Dynamic    -> "urlBase"
    Static url -> dquotes $ stext url

  paramsDoc :: Doc
  paramsDoc = mkParams opts request

  segmentsDoc :: Doc
  segmentsDoc = elmList $ map segmentToDoc (request ^. F.reqUrl . F.path)

  segmentToDoc :: F.Segment ElmDatatype -> Doc
  segmentToDoc s = case F.unSegment s of
    F.Static path -> dquotes (stext (F.unPathSegment path))
    F.Cap arg ->
      let
        -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
          toStringSrc' = toStringSrc "<|" opts (arg ^. F.argType)
      in  toStringSrc' <+> elmCaptureArg s




{- | Determines whether we construct an Elm function that expects an empty
response body.
-}
isEmptyType :: ElmOptions -> ElmDatatype -> Bool
isEmptyType opts elmTypeExpr = elmTypeExpr `elem` emptyResponseElmTypes opts

toStringSrc :: T.Text -> ElmOptions -> ElmDatatype -> Doc
toStringSrc operator opts argType
  |
  -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
    isElmStringType opts argType
  = stext ""
  | otherwise
  = stext $ toStringSrcTypes operator opts argType <> " " <> operator


{- | Same as toStringSrc but with operator on the left side.
-}
toStringSrcL :: T.Text -> ElmOptions -> ElmDatatype -> Doc
toStringSrcL operator opts argType
  |
  -- Don't use "toString" on Elm Strings, otherwise we get extraneous quotes.
    isElmStringType opts argType
  = stext ""
  | otherwise
  = stext $ operator <> " " <> toStringSrcTypes operator opts argType


{- | Determines whether we call `toString` on URL captures and query params of
this type in Elm.
-}
isElmStringType :: ElmOptions -> ElmDatatype -> Bool
isElmStringType _ (ElmPrimitive (EList (ElmPrimitive EChar))) = True
isElmStringType opts elmTypeExpr = elmTypeExpr `elem` stringElmTypes opts


toStringSrcTypes :: T.Text -> ElmOptions -> ElmDatatype -> T.Text
toStringSrcTypes operator opts (ElmPrimitive (EMaybe argType)) =
  toStringSrcTypes operator opts argType
toStringSrcTypes _ _ (ElmPrimitive (EList (ElmPrimitive EChar))) = "identity"
toStringSrcTypes operator opts (ElmPrimitive (EList argType)) =
  toStringSrcTypes operator opts argType
toStringSrcTypes _ opts argType
  | isElmStringType opts argType = "identity"
  | isElmIntType opts argType = "String.fromInt"
  | isElmFloatType opts argType = "String.fromFloat"
  | isElmBoolType opts argType = "(\\v -> if v then \"True\" else \"False\")"
  | -- We should change this to return `true`/`false` but this mimics the old behavior.
    isElmCharType opts argType = "String.fromChar"
  | otherwise = error
    ("Sorry, we don't support other types than `String`, `Int` and `Float` atm. "
    <> show argType
    )

{- | Determines whether we call `String.fromInt` on URL captures and query params of this type in Elm.
-}
isElmIntType :: ElmOptions -> ElmDatatype -> Bool
isElmIntType opts elmTypeExpr = elmTypeExpr `elem` intElmTypes opts


{- | Determines whether we call `String.fromFloat` on URL captures and query params of
this type in Elm.
-}
isElmFloatType :: ElmOptions -> ElmDatatype -> Bool
isElmFloatType opts elmTypeExpr = elmTypeExpr `elem` floatElmTypes opts


{- | Determines whether we convert to `true` or `false`
-}
isElmBoolType :: ElmOptions -> ElmDatatype -> Bool
isElmBoolType opts elmTypeExpr = elmTypeExpr `elem` boolElmTypes opts

{- | Determines whether we call `String.fromChar` on URL captures and query params of
this type in Elm.
-}
isElmCharType :: ElmOptions -> ElmDatatype -> Bool
isElmCharType opts elmTypeExpr = elmTypeExpr `elem` charElmTypes opts


isElmMaybeType :: ElmDatatype -> Bool
isElmMaybeType (ElmPrimitive (EMaybe _)) = True
isElmMaybeType _                         = False


{- | Determines whether we call `String.fromInt` on URL captures and query params of this type in Elm.
-}
isElmListType :: ElmDatatype -> Bool
isElmListType (ElmPrimitive (EList _)) = True
isElmListType _                        = False

-- Doc helpers


docToText :: Doc -> Text
docToText = L.toStrict . displayT . renderPretty 0.4 100

stext :: Text -> Doc
stext = text . L.fromStrict

elmRecord :: [Doc] -> Doc
elmRecord = encloseSep (lbrace <> space) (line <> rbrace) (comma <> space)

elmList :: [Doc] -> Doc
elmList [] = lbracket <> rbracket
elmList ds = lbracket <+> hsep (punctuate (line <> comma) ds) <$> rbracket

elmListOfMaybes :: [Doc] -> Doc
elmListOfMaybes [] = lbracket <> rbracket
elmListOfMaybes ds = "List.filterMap identity" <$> indent 4 (elmList ds)
