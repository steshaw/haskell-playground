--
-- Motivated by post to scala-melbourne https://groups.google.com/d/topic/scala-melb/r-zYEUjhxuA/discussion
--
-- See http://en.wikipedia.org/wiki/Happy_number
--

import Data.List ((\\))

(|>) = flip ($)

isHappy n = case sumSquares (digits n) of
    1 -> True
    4 -> False -- all unhappy numbers go through 4. In fact this sequence: 4, 16, 37, 58, 89, 145, 42, 20, 4, ….
    n -> isHappy n
  where
    base = 10
    square n = n * n
    sumSquares = sum . map square
    -- Only works correctly with postive numbers.
    -- Digits are (most efficiently) provided in reverse order.
    digits n = case quotRem n base of
      (0, d) -> [d]
      (r, d) -> d : (digits r)

happys =
  [ 1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97, 100, 103, 109, 129, 130, 133, 139, 167
  , 176, 188, 190, 192, 193, 203, 208, 219, 226, 230, 236, 239, 262, 263, 280, 291, 293, 301, 302, 310, 313, 319, 320
  , 326, 329, 331, 338, 356, 362, 365, 367, 368, 376, 379, 383, 386, 391, 392, 397, 404, 409, 440, 446, 464, 469, 478
  , 487, 490, 496
  ]
truths = happys |> map isHappy

unhappys = enumFromTo 2 500 \\ happys

falsehoods = unhappys |> map isHappy

happies = filter isHappy [1..]

main = mapM_ print [truths, falsehoods]
