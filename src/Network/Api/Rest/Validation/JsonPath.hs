{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Rest.Validation.JsonPath (
  rootPath,
  pathWithMember, pathWithElement,
  Path, PathSegment(..)
  ) where

import qualified Data.Text as T

data Path 
    = Path [PathSegment] 
    deriving (Eq, Ord)

instance Show Path where
  show (Path p) = foldl (++) "" (map show p)

data PathSegment
    = PathRoot
    | PathMember T.Text
    | PathElement Int
    deriving (Eq, Ord)

instance Show PathSegment where
  show (PathRoot)       = "$"
  show (PathMember pm)  = "." ++ (T.unpack pm)
  show (PathElement pe) = "[" ++ show pe ++ "]"

rootPath :: Path
rootPath = Path [PathRoot]

pathWithMember :: Path -> T.Text -> Path
pathWithMember (Path ps) m = Path (ps ++ [PathMember m])

pathWithElement :: Path -> Int -> Path
pathWithElement (Path ps) e = Path (ps ++ [PathElement e])