{-# LANGUAGE OverloadedStrings #-}

module Example.Api.Resources.Home (
    home
    ) where

import Network.Api.Rest.Handlers

home :: HttpHandler
home _ _ = httpOkRaw "this is home"