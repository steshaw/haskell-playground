--
-- Evil global variables in Haskell using unsafePerformIO (MVars in this case).
--
-- Inspired by http://stackoverflow.com/questions/6076129/a-way-to-avoid-a-common-use-of-unsafeperformio
--

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

data Options = Options { optionsHostname :: String, optionsPort :: Integer }

options :: MVar Options
options = unsafePerformIO $ newEmptyMVar

url :: Integer -> String
url port = unsafePerformIO $ do
  opt <- readMVar options
  return (url' opt)
  where
    url' opt = (optionsHostname opt) ++ ":" ++ (show port)

main :: IO ()
main = do
  putMVar options (Options {optionsHostname = "localhost", optionsPort = 5000})
  putStrLn (url 80)
