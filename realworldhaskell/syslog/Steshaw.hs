module Steshaw where

forever f = f >> forever f
