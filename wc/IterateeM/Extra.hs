module IterateeM.Extra where

import IterateeM

-- Count NL in a stream of characters.
count_nl :: Iteratee Char IO Integer
count_nl = ie_cont $ step 0
  where
    step acc (Chunk str)  = ie_contM (step $! acc + count str)
    step acc stream       = ie_doneM acc stream
    count [] = 0
    count ('\n':str) = succ $! count str
    count (_:str) = count str

-- Count the stream. This could have been in the IterateeM library
stream_count :: Monad m => Iteratee el m Int
stream_count = ie_cont $ step 0
  where
    step acc (Chunk [])  = ie_contM (step acc)
    step acc (Chunk [_]) = ie_contM (step $! succ acc)
    step acc (Chunk ls)  = ie_contM (step $! acc + length ls)
    step acc stream      = ie_doneM acc stream
