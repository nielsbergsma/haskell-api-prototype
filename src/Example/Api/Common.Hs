{-# LANGUAGE OverloadedStrings #-}

module Example.Api.Common (
    runQuery, writeRawJsonArray
 ) where

import Control.Monad.Trans (liftIO)
import Network.Api.Rest.Context

import qualified Data.ByteString.Lazy as BSL
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Pool as DP

runQuery :: HttpContext -> (PG.Connection -> IO a) -> IO a
runQuery c q = withPool (primary $ storage c) (($) q)
  where withPool p r = do liftIO . DP.withResource p $ r

writeRawJsonArray :: [BSL.ByteString] -> BSL.ByteString
writeRawJsonArray xs = BSL.concat ["[", BSL.intercalate "," xs, "]"]