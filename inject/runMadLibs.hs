module Main where

import MadLibs
import Control.Monad (liftM3)
import Control.Monad.Reader

data AppConfig = AppConfig {
  name :: Name
 ,verb :: Verb
 ,noun :: Noun
} deriving (Show)

appConfig = AppConfig (Name "Steve") (Verb "flew") (Noun "Statue of Liberty")

--jokeMaker :: Reader AppConfig () -> String
jokeMaker = do
  appConfig <- ask
  return $ joke (name appConfig) (verb appConfig) (noun appConfig)

{-
joke' :: Reader AppConfig Name -> Reader AppConfig Verb -> Reader AppConfig Noun -> Reader AppConfig String
joke' = liftM3 joke
-}

{-
grabName = do
  appConfig <- ask
  return name appConfig

grabVerb = do
  appConfig <- ask
  return verb appConfig

grabNoun = do
  appConfig <- ask
  return noun appConfig

ajoke = joke (runReader grabName appConfig) (runReader grabVerb appConfig) (runReader grabNoun appConfig)
-}

main = do
    print appConfig
    putStrLn $ joke (Name "Fred") (Verb "swam") (Noun "Wall Street")
    putStrLn $ runReader jokeMaker appConfig
--    putStrLn ajoke

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
