#!/usr/bin/env stack
{-
  stack script
    --resolver lts-18.0
    --package template-haskell
    --
    -Wall -fwarn-tabs -ddump-splices
-}

-- Example of "meta meta" TH.
-- http://www.yesodweb.com/blog/2010/09/yo-dawg-template-haskell

{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax

main = $(return $(return $ InfixE
    (Just $
        (ConE $ mkName "VarE") `AppE`
        ((VarE $ mkName "mkName") `AppE` (LitE $ StringL "putStrLn")))
    (ConE $ mkName "AppE")
    (Just $
        (ConE $ mkName "LitE") `AppE`
        ((ConE $ mkName "StringL") `AppE` (LitE $ StringL "Yo dawg")))))
