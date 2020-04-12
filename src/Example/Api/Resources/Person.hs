{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Example.Api.Resources.Person (
    person
 ) where

import Data.Maybe
import Network.Api.Rest.Handlers
import Network.Api.Rest.Routing
import Network.Api.Rest.Validate

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.Api.Rest.Validation.ParameterValidator as PV

import Example.Api.Common

-- Parameters 
data PersonParameters = PersonParameters 
  { personId :: T.Text
  }

-- Transformers & validators
readPersonParameters :: HttpRequest -> Either PV.ValidationFailure PersonParameters
readPersonParameters r = do
  personIdP  <- PV.required "person_id" (pathParameter r 1) (PV.string [PV.minLength 1, PV.maxLength 6])
  return PersonParameters 
    { personId  = PV.readText personIdP
    }

-- Queries
queryPerson :: T.Text -> T.Text -> PG.Connection -> IO (Maybe BSL.ByteString)
queryPerson tenant id' connect = do
    let firstRow = listToMaybe
    let firstColumn = \(PG.Only c) -> c 
    let statement = "select data_raw from people where tenant = ? and id = ? limit 1"

    results <- PG.query connect statement (tenant, id') 
    return (firstRow results >>= firstColumn)

-- Handlers 
person :: HttpHandler
person c r = validate1 
  (hasParameters readPersonParameters r)
  \p -> person' p c r
    
person' :: PersonParameters -> HttpHandler
person' p c _ = do 
  result <- runQuery c (queryPerson "test" (personId p))
  case result of
    Just ps -> httpOkRaw ps
    Nothing -> return HttpNotFound
