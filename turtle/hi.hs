#!/usr/bin/env stack
-- stack --resolver lts-8.5 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = echo "Hello World!"
