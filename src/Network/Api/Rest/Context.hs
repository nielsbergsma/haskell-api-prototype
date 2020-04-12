{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Context (
    mkHttpContext, security, storage, jwkSet, jwtValidationSettings, primary,
    
    HttpContext, SecuritySettings, StorageSettings
    ) where 

import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.Api.Rest.Authorization.Jwt as Jwt

data HttpContext = HttpContext 
  { security :: SecuritySettings
  , storage :: StorageSettings
  }

data SecuritySettings = SecuritySettings
  { jwkSet :: Jwt.JWKSet
  , jwtValidationSettings :: Jwt.JWTValidationSettings
  }

data StorageSettings = StorageSettings 
  { primary :: DP.Pool PG.Connection
  }

mkHttpContext :: Jwt.JWKSet -> [Jwt.Audience] -> DP.Pool PG.Connection -> HttpContext
mkHttpContext jwks auds ps = HttpContext
  { security = SecuritySettings 
    { jwkSet = jwks
    , jwtValidationSettings = Jwt.settings auds     
    }
  , storage = StorageSettings
    { primary = ps
    }  
  }