module Main where

import MadLibs
import Control.Monad
import Control.Monad.Reader

data AppConfig = AppConfig {
  name :: Name
 ,verb :: Verb
 ,noun :: Noun
} deriving (Show)

appConfig = AppConfig (Name "Steve") (Verb "flew") (Noun "Statue of Liberty")

joke' :: Reader AppConfig String
joke' = do
  appConfig <- ask
  return $ joke (name appConfig) (verb appConfig) (noun appConfig)

-- Long hand way of doing liftM3.
joke'' :: Reader AppConfig Name -> Reader AppConfig Verb -> Reader AppConfig Noun -> Reader AppConfig String
joke'' a b c = do
  a' <- a
  b' <- b
  c' <- c
  return $ joke a' b' c'

joke''' :: Reader AppConfig Name -> Reader AppConfig Verb -> Reader AppConfig Noun -> Reader AppConfig String
joke''' = liftM3 joke

--x = joke''' (liftM name) (liftM verb) (liftM noun)

grabName = do
  appConfig <- ask
  return name appConfig

grabVerb = do
  appConfig <- ask
  return verb appConfig

grabNoun = do
  appConfig <- ask
  return noun appConfig

--ajoke = joke (runReader (liftM name) appConfig) (runReader (liftM verb) appConfig) (runReader (liftM noun) appConfig)

{-
foo = do
  name <- ask
  verb <- ask
  noun <- ask
  return joke name verb noun
-}

main = do
    print appConfig
    putStrLn $ joke (Name "Fred") (Verb "swam") (Noun "Wall Street")
    putStrLn $ runReader joke' appConfig
--    aJoke <- joke''
--    putStrLn aJoke

{-
myName step = do
  name <- ask
  return $ step ++ "). I am " ++ name

eg1 :: Reader String [String]
eg1 = do
  a <- myName "1"
  b <- local (const "Fred") (myName "2")
  c <- myName "3"
  return [a,b,c]

eg2 = mapM_ print $ runReader eg1 "Steve"
-}
