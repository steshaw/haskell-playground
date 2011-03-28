--
-- Some code posted by Don Stewart as a comment on the following blog.
--  http://moonmaster9000.tumblr.com/post/514172391/ruby-v-haskell-continued-a-script-for-converting
--

import System.Environment

main = do
    args <- getArgs
    case args of
      [inp,outp] -> mapFile (unlines . splitLines) inp outp
      _          -> error "You must provide two filenames."

mapFile f i o = do
  s <- readFile i
  writeFile o (f s)

splitLines [] = []
splitLines s  = x : xs
  where
    (x, rest) = break (\c -> c `elem` "\r\n") s
    xs        = case rest of
        ('\r':'\n':t) -> splitLines t
        ('\r':t)      -> splitLines t
        ('\n':t)      -> splitLines t
        _             -> []
