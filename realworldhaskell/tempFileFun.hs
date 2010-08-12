import System.IO
import System.Directory
import Control.Exception(finally)

main :: IO ()
main = withTempFile "temp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempPath tempFile = do
  putStrLn "Welcome to tempFileFun.hs!"
  putStrLn $ "I have a temporary file at " ++ tempPath
  pos <- hTell tempFile
  putStrLn $ "My initial position is " ++ show pos

  let tempData = show [1..10]
  putStrLn $ "Writing one line containing " ++
    show (length tempData) ++ " bytes: " ++ tempData
  hPutStr tempFile tempData

  pos <- hTell tempFile
  putStrLn $ "After writing, my new position is " ++ show pos

{-
  hSeek tempFile RelativeSeek (-1)
  pos <- hTell tempFile
  putStrLn $ "Content at " ++ show pos ++ " is:"
  c <- hGetContents tempFile
  putStrLn c
-}

  putStrLn $ "The file content is: "
  hSeek tempFile AbsoluteSeek 0
  c <- hGetContents tempFile
  putStrLn c

  putStrLn $ "Which could be expressed as this Haskell literal:"
  print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
  tempDir <- catch getTemporaryDirectory (\_ -> return ".")
  (tempPath, tempFile) <- openTempFile tempDir pattern
  finally (func tempPath tempFile) $ do 
    hClose tempFile
    removeFile tempPath
