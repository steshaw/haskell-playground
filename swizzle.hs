--
-- Inspired by http://blog.tmorris.net/i-have-found-that/
--

import Control.Monad.Instances ()

sequence' :: [t -> a] -> t -> [a]
sequence' = sequence

type Swizzle a = Int -> a

print0 :: Swizzle (IO ())
print0 n = print n

print1 :: Swizzle (IO ())
print1 n = print $ "1" ++ (show n)

print2 :: Int -> IO ()
print2 n = print $ "2" ++ (show n)

asString :: Swizzle String
asString n = show n

formattedMessage :: Swizzle String
formattedMessage n = "You have found a " ++ (show n)

shortMessage :: Swizzle String
shortMessage n = "A " ++ (show n)

actions :: [IO ()]
actions = sequence' [print0, print1, print2] 3

results :: [String]
results = sequence' [asString, shortMessage, formattedMessage] 3

main :: IO ()
main = do
  print results
  mapM_ putStrLn results
  sequence_ actions
