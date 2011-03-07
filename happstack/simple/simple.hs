module Main (main)
where

import Happstack.Server

main = simpleHTTP nullConf $ ok "Hello, World!"
