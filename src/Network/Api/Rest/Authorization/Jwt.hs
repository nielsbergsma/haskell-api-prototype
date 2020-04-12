{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Authorization.Jwt (
      audienceFromString
    , settings
    , readJwksFile
    , parseToken
    , rolesMatch

    , Audience
    , JWKSet
    , JWTError
    , JWTValidationSettings
    ) where

import Data.Maybe (fromJust)
import Data.Aeson (eitherDecode, Value(..))
import Control.Monad.Except (runExceptT)
import Control.Lens (preview, view)
import Crypto.JWT (StringOrURI, JWTValidationSettings, JWKSet, JWTError, ClaimsSet, defaultJWTValidationSettings, decodeCompact, verifyClaims, unregisteredClaims, stringOrUri)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as DHL
import qualified Data.Vector as DV
import qualified Data.ByteString.Lazy as BSL

type Audience = StringOrURI

audienceFromString :: String -> Audience
audienceFromString a = fromJust $ preview stringOrUri a

settings :: [StringOrURI] -> JWTValidationSettings
settings as = defaultJWTValidationSettings (`elem` as)

readJwksFile :: FilePath -> IO (Either String JWKSet)
readJwksFile path = eitherDecode <$> BSL.readFile path :: IO (Either String JWKSet)

parseToken :: JWTValidationSettings -> JWKSet -> BSL.ByteString -> IO (Either JWTError ClaimsSet)
parseToken cfg jwks jwtRaw = do
  jwt <- runExceptT $ decodeCompact jwtRaw
  case jwt of 
    (Left l)  -> pure $ Left l
    (Right r) -> runExceptT $ verifyClaims cfg jwks r

roles :: ClaimsSet -> [T.Text]
roles cs = case cognitoGroups $ claims cs of 
    (Just gs) -> unpackArray gs
    (Nothing) -> []
    where claims                  = view unregisteredClaims
          cognitoGroups           = DHL.lookup ("cognito:groups" :: T.Text)
          unpackArray (Array a)   = DV.toList $ fmap unpackString a
          unpackArray _           = []
          unpackString (String s) = s
          unpackString _          = ""

rolesMatch :: [T.Text] -> ClaimsSet -> Bool
rolesMatch []   _    = True
rolesMatch want have = any (`elem` want) (roles have)