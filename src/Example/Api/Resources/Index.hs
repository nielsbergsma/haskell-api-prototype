{-# LANGUAGE OverloadedStrings #-}

module Example.Api.Resources.Index (
    index
 ) where

import Network.Api.Rest.Handlers

index :: HttpHandler
index _ _ = return $ HttpOkFile "text/html" "static/index.html"
