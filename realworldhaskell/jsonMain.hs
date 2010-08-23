module Main () where

import Json
import Prettify
import Control.Monad (forM_)

getJsonObject (JsonObject o) = o

main =
  do
    printEm $ JsonNull
    printEm $ JsonString "hi"
    printEm $ JsonString "This\nis\tfun"
    printEm $ JsonNumber 3.14
    printEm $ JsonBool True
    printEm $ JsonBool False
    let json1 = JsonObject [("foo", JsonNumber 1), ("bar", JsonBool False)]
    printEm json1
    let json2 = JsonArray $ [JsonNull, JsonString "hi"] ++ (map JsonNumber [10,20..50])
    printEm json2
    let json3 = JsonObject $ ("oh", json2):getJsonObject json1
    printEm json3
  where
    printEm json = do
      putStrLn $ replicate 80 '-'
      putStrLn $ "           Raw: " ++ show json
      putStrLn $ "        Render: " ++ renderJson json
      putStrLn $ "Pretty compact: " ++ compact (prettyJson json)
      forM_ [15, 30, 60] $ \ width ->
        putStrLn $ "     Pretty " ++ (show width) ++ ": " ++ pretty width (prettyJson json)
