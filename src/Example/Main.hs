{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.AddHeaders
import Network.Api.Rest.Routing
import Network.Api.Rest.Context
import System.Environment (getEnv)

import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Pool as DP
import qualified Data.ByteString.UTF8 as BSU
import qualified Network.Api.Rest.Authorization.Jwt as Jwt

import Example.Api.Resources.Home 
import Example.Api.Resources.Index
import Example.Api.Resources.User 
import Example.Api.Resources.Person
import Example.Api.Resources.People

rightOrError :: Either String r -> IO r
rightOrError (Left l) = error l
rightOrError (Right r) = return r

resources :: [RouteHandler]
resources = 
  [ get   [] index
  , get   ["home"] home
  , patch ["users", ":user_id"] user
  , get   ["people"] people
  , get   ["people", ":person_id"] person
  ]

-- Main
main :: IO ()
main = do 
  jwksFilePath     <- getEnv "JWKS_FILE_PATH"
  jwtAudience      <- getEnv "JWT_AUDIENCE"
  connectionString <- getEnv "CONNECTION_STRING"
  jwks             <- Jwt.readJwksFile jwksFilePath >>= rightOrError
  connectionPool   <- DP.createPool (PG.connectPostgreSQL $ BSU.fromString connectionString) PG.close 2 60 10
  let context = mkHttpContext jwks [Jwt.audienceFromString jwtAudience] connectionPool

  run 3000 
    $ addHeaders [("Server", "your name here")] 
    $ gzip def { gzipFiles = GzipCompress }
    $ staticPolicy (addBase "static")
    $ route context resources
