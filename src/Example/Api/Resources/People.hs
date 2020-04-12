{-# LANGUAGE OverloadedStrings #-}

module Example.Api.Resources.People (
    people
 ) where

import Network.Api.Rest.Handlers

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Database.PostgreSQL.Simple as PG

import Example.Api.Common

-- Handler
people :: HttpHandler
people c _ =  do
  result <- runQuery c (queryPersons "test")
  httpOkRaw $ writeRawJsonArray result

-- Queries 
queryPersons :: T.Text -> PG.Connection -> IO ([BSL.ByteString])
queryPersons tenant connect = do
    let firstColumn = \(PG.Only c) -> c 
    let statement = "select data_raw from people where tenant = ?"

    results <- PG.query connect statement (PG.Only (tenant :: T.Text)) :: IO [PG.Only BSL.ByteString]
    return (firstColumn <$> results)