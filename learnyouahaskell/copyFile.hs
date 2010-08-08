
import System.Environment
import qualified Data.ByteString.Lazy as LBS

main = do
  (fileName1:fileName2:_) <- getArgs
  copyFile fileName1 fileName2 -- even though System.Directory.copyFile exists

copyFile src dest = do
  contents <- LBS.readFile src
  LBS.writeFile dest contents
