{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Routing (
    route, respond, get, patch, 
    pathParameter, queryParameter,

    RouteHandler, PathSegment, Path
    ) where 

import Data.Aeson 
import Control.Exception (catch)
import Network.Wai
import Network.HTTP.Types
import Network.Api.Rest.Handlers
import Network.Api.Rest.Error
import Network.Api.Rest.Context

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

type RouteHandler = HttpRequest -> RouteResult

type PathSegment = T.Text

type Path = [PathSegment]

data RouteResult = RouteNoMatch | RouteOk HttpHandler

route :: HttpContext -> [RouteHandler] -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
route c [] r s = (respond c r notFound) >>= s
route c (h:hs) r s = case h r of
  (RouteOk rh)   -> (respond c r rh) >>= s
  (RouteNoMatch) -> route c hs r s 

respond :: HttpContext -> HttpRequest -> HttpHandler -> IO Response
respond ctx req h = send <$> 
    catch (h ctx req) internalServerErrror
    where send (HttpOk res)               = responseLBS  status200 [("Content-Type", "application/json;charset=utf-8")] res
          send (HttpOkFile ct p)          = responseFile status200 [("Content-Type", ct)] p Nothing
          send (HttpBadRequest res)       = responseLBS  status400 [("Content-Type", "application/json;charset=utf-8")] res
          send (HttpUnauthorized)         = responseLBS  status401 [("Content-Type", "application/json;charset=utf-8")] (encode $ CommonError "authorization" "unauthorized")
          send (HttpForbidden)            = responseLBS  status403 [("Content-Type", "application/json;charset=utf-8")] (encode $ CommonError "authorization" "forbidden")
          send (HttpNotFound)             = responseLBS  status404 [("Content-Type", "application/json;charset=utf-8")] (encode $ CommonError "routing" "not_found")
          send (HttpUnsupportedMediaType) = responseLBS  status415 [("Content-Type", "application/json;charset=utf-8")] (encode $ CommonError "routing" "unsupported_media_type")
          send (HttpInternalServerError)  = responseLBS  status500 [("Content-Type", "application/json;charset=utf-8")] (encode $ CommonError "unknown" "unexpected_error")

get :: Path -> HttpHandler -> RouteHandler
get p h r
  | verbOk && pathOk = RouteOk h
  | otherwise = RouteNoMatch
  where verbOk = "GET" == requestMethod r
        pathOk = pathEq (pathInfo r) p

patch :: Path -> HttpHandler -> RouteHandler
patch p h r
  | verbOk && pathOk = RouteOk h
  | otherwise = RouteNoMatch
  where verbOk = "PATCH" == requestMethod r
        pathOk = pathEq (pathInfo r) p

pathEq :: Path -> Path -> Bool
pathEq [] []         = True
pathEq [] _          = False
pathEq _  []         = False
pathEq (l:ls) (r:rs) = (isParam r || l == r) && pathEq ls rs
  where isParam n = T.isPrefixOf ":" n

pathParameter :: HttpRequest -> Int -> Maybe PathSegment
pathParameter r s 
  | s < length path = Just $ path !! s
  | otherwise       = Nothing
  where path = pathInfo r

queryParameter :: HttpRequest -> T.Text -> Maybe T.Text 
queryParameter r n = case lookup (TE.encodeUtf8 n) query of
  (Just (Just v))   -> Just $ TE.decodeUtf8 v
  (Just (Nothing))  -> Just ""
  (Nothing)         -> Nothing
  where query = queryString r