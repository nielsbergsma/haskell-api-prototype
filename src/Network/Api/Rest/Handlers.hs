{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Handlers (
    httpOk, httpOkRaw, httpBadRequest, internalServerErrror, notFound,

    HttpRequest, HttpResponse (..), HttpHandler
    ) where

import Network.Wai
import Data.Aeson 
import Control.Exception (SomeException)
import Network.Api.Rest.Context 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

type HttpRequest = Request

data HttpResponse
    = HttpOk BSL.ByteString
    | HttpOkFile BS.ByteString FilePath
    | HttpBadRequest BSL.ByteString
    | HttpUnsupportedMediaType
    | HttpUnauthorized
    | HttpForbidden
    | HttpNotFound
    | HttpInternalServerError

type HttpHandler = HttpContext -> HttpRequest -> IO HttpResponse

httpOk :: ToJSON a => a -> IO HttpResponse
httpOk o = return $ HttpOk $ encode o

httpOkRaw :: BSL.ByteString -> IO HttpResponse
httpOkRaw o = return $ HttpOk o

httpBadRequest :: ToJSON a => a -> IO HttpResponse
httpBadRequest e = return $ HttpBadRequest $ encode e

internalServerErrror :: SomeException -> IO HttpResponse
internalServerErrror _ = return $ HttpInternalServerError 

notFound :: HttpHandler
notFound _ _ = return HttpNotFound