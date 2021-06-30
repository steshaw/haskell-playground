#!/usr/bin/env stack
{-
  stack script --resolver lts-18.0
    --package bytestring
    --package http-types
    --package wai
    --package warp
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (runEnv)

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

app :: Application
app req respond =
  respond $ case (requestMethod req, pathInfo req) of
    ("GET", []) ->
      html status200 "<h1>Welcome</h1>"
    ("GET", ["login"]) ->
      html status200 "<h1>Please login</h1>"
    _ -> 
      html status404 "<h1>Not found</h1>"

html :: Status -> ByteString -> Response
html status body =
  responseLBS status [("Content-Type", "text/html")] body

main :: IO ()
main = runEnv 3000 app
