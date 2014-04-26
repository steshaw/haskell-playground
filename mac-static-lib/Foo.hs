module Foo where

import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Data.List

foreign export ccall comma :: CString -> IO CString

comma :: CString -> IO CString
comma x = do
  s <- peekCString x
  newCString $ intercalate ", " $ words s
