#!/usr/bin/env stack
{-
  stack script --resolver lts-8.8
    --package http-types
    --package wai
    --package warp
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket_)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp (runEnv)

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

app :: Application
app req respond = bracket_
  (putStrLn "Allocating scarce resource")
  (putStrLn "Cleaning up")
  (respond $ responseLBS status200 [] "Hello World")

main :: IO ()
main = runEnv 3000 app
