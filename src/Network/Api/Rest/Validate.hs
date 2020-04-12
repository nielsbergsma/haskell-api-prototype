{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Validate (
    hasRoleAny, hasParameters, hasBody,
    validate1, validate2, validate3
  ) where

import Data.Aeson
import Network.Wai
import Network.HTTP.Types
import Network.Api.Rest.Handlers
import Network.Api.Rest.Error
import Network.Api.Rest.Context

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Api.Rest.Authorization.Jwt as Jwt
import qualified Network.Api.Rest.Validation.JsonPath as Json
import qualified Network.Api.Rest.Validation.JsonValidator as JV

-- Request 
hasRoleAny :: [T.Text] -> HttpContext -> HttpRequest -> IO (Either AuthorizationError ())
hasRoleAny xs c r = case BS.split ' ' <$> header r "Authorization" of 
  (Just ["Bearer", bearerToken]) -> do
    jwtToken <- Jwt.parseToken settings jwks (BSL.fromStrict bearerToken)
    return $ case authenticates <$> jwtToken of 
      (Right True) -> Right ()
      (_)          -> Left Forbidden
    where authenticates = Jwt.rolesMatch xs
          settings  = jwtValidationSettings (security c)
          jwks = jwkSet (security c)
  (_) -> return $ Left Unauthorized

hasParameters :: (HttpRequest -> Either a b) -> HttpRequest -> IO (Either a b)
hasParameters v r = return $ v r

hasBody :: FromJSON a => JV.Validator -> HttpRequest -> IO (Either RequestBodyError a)
hasBody v r = do 
  body <- lazyRequestBody r
  return $ contentOk isJson body
    where contentOk False _       = Left UnsupportedMediaType
          contentOk True b        = parsedOk (eitherDecode b)
          parsedOk (Left _)       = Left CorruptBody
          parsedOk (Right c)      = validatedOk (v Json.rootPath $ pure c) c
          validatedOk (Left l) _  = Left (InvalidBody l)
          validatedOk (Right _) b = decodedOk (fromJSON b)
          decodedOk (Error _)     = Left UnmarshableBody
          decodedOk (Success s)   = Right s
          isJson                  = header r "Content-Type" == Just "application/json;charset=utf-8"

-- Helpers
validate1 :: HttpErrorable l1 => IO (Either l1 r1) -> (r1 -> IO HttpResponse) -> IO HttpResponse
validate1 c1 n = do
  c1' <- c1 
  case c1' of 
    (Left l1)  -> mapToError l1
    (Right r1) -> n r1

validate2 :: HttpErrorable l1 => HttpErrorable l2 => IO (Either l1 r1) -> IO (Either l2 r2) -> (r1 -> r2 -> IO HttpResponse) -> IO HttpResponse
validate2 c1 c2 n = do 
  c1' <- c1
  c2' <- c2
  case (c1', c2') of 
    (Left l1, _) -> mapToError l1
    (_, Left l2) -> mapToError l2
    (Right r1, Right r2) -> n r1 r2

validate3 :: HttpErrorable l1 => HttpErrorable l2 => HttpErrorable l3 => IO (Either l1 r1) -> IO (Either l2 r2) -> IO (Either l3 r3) -> (r1 -> r2 -> r3 -> IO HttpResponse) -> IO HttpResponse
validate3 c1 c2 c3 n = do 
  c1' <- c1
  c2' <- c2
  c3' <- c3
  case (c1', c2', c3') of 
    (Left l1, _, _) -> mapToError l1
    (_, Left l2, _) -> mapToError l2
    (_, _, Left l3) -> mapToError l3
    (Right r1, Right r2, Right r3) -> n r1 r2 r3


-- Other
header :: HttpRequest -> HeaderName -> Maybe BS.ByteString
header r n = lookup n (requestHeaders r)