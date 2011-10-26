module FizzBuzz where

(|>) = flip ($)

divisible by n = n `mod` by == 0

div3 = divisible 3
div5 = divisible 5

main = [1..100]
  |> map (\n -> if div3 n && div5 n then "FizzBuzz" else if div3 n then "Fizz" else if div5 n then "Buzz" else show n)
  |> mapM_ putStrLn
