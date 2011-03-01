module Main (main) where

import MadLibs
import Control.Monad
import Control.Monad.Reader

data AppConfig = AppConfig {
  name :: String
 ,verb :: String
 ,noun :: String
} deriving (Show)

appConfig = AppConfig "Steve" "flew" "Statue of Liberty"

-- Apply joke longhand.
longApplyJoke1 :: Reader AppConfig String
longApplyJoke1 = do
  name <- asks name
  verb <- asks verb
  noun <- asks noun
  return $ joke name verb noun

-- Alternative method to apply joke by longhand.
longApplyJoke2 :: Reader AppConfig String
longApplyJoke2 = do
  appConfig <- ask
  return $ joke (name appConfig) (verb appConfig) (noun appConfig)

-- Lifed joke longhand.
liftJoke1 :: Reader AppConfig String -> Reader AppConfig String -> Reader AppConfig String -> Reader AppConfig String
liftJoke1 a b c = do
  a' <- a
  b' <- b
  c' <- c
  return $ joke a' b' c'

-- Lift joke.
liftJoke2 :: Reader AppConfig String -> Reader AppConfig String -> Reader AppConfig String -> Reader AppConfig String
liftJoke2 = liftM3 joke

-- Apply lifted jokes.
-- XXX: Doesn't seem that magical. What am I missing?

applyJoke1 :: Reader AppConfig String
applyJoke1 = liftJoke1 (asks name) (asks verb) (asks noun)

applyJoke2 :: Reader AppConfig String
applyJoke2 = liftJoke2 (asks name) (asks verb) (asks noun)

applyJoke3 :: Reader AppConfig String
applyJoke3 = (liftM3 joke) (asks name) (asks verb) (asks noun)

main = do
    print appConfig
    putStrLn $ joke "Marco Polo" "flew" "clock"
    putStrLn $ runReader longApplyJoke1 appConfig
    putStrLn $ runReader longApplyJoke2 appConfig
    putStrLn $ runReader applyJoke1 appConfig
    putStrLn $ runReader applyJoke2 appConfig
    putStrLn $ runReader applyJoke3 appConfig
