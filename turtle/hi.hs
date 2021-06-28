#!/usr/bin/env stack
-- stack --resolver lts-18.0 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = echo "Hello World!"
