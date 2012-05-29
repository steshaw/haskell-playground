import Control.Monad
import Data.Maybe (fromMaybe)

--
-- Original strip quotes
--
stripQuotes0 :: String -> String
stripQuotes0 s@[_]                       = s
stripQuotes0 ('"' : s)  | last s == '"'  = init s
stripQuotes0 ('\'' : s) | last s == '\'' = init s
stripQuotes0 s                           = s

--
-- First attempt. Extract common functionality.
--
stripSurrounding1 :: Char -> String -> String
stripSurrounding1 _ s@[_]                            = s
stripSurrounding1 c (f : s)  | f == c && last s == c = init s
stripSurrounding1 _ s                                = s

-- but this gets ugly and I must compare the strings...
stripQuotes1 :: String -> String
stripQuotes1 s =
  let s0 = stripSurrounding1 '"' s
  in if s == s0
     then stripSurrounding1 '\'' s
     else s0

--
-- Attempt 2. Use Maybe.
--
stripSurrounding2 :: Char -> String -> Maybe String
stripSurrounding2 _ [_]                             = Nothing
stripSurrounding2 c (f : s) | f == c && last s == c = Just (init s)
stripSurrounding2 _ _                               = Nothing

-- Not the easiest to follow here...
stripQuotes2 :: String -> String
stripQuotes2 s = fromMaybe s $ (stripSurrounding2 '"' s) `mplus` (stripSurrounding2 '\'' s)

--
-- Attempt 3.
--
(|||) :: MonadPlus m => m a -> m a -> m a
(|||) = mplus

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

stripQuotes3 :: String -> String
stripQuotes3 s =
      stripSurrounding2 '"' s
  ||| stripSurrounding2 '\'' s
  |> fromMaybe s
