
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified Data.ByteString as BS

numChunks LBSI.Empty = 0
numChunks (LBSI.Chunk _ lbs) = 1 + numChunks lbs
