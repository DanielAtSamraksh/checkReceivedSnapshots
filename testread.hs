import Prelude hiding ( readFile )
import Data.ByteString.Lazy ( readFile )
import Data.Binary.Get ( isEmpty, runGet )
import qualified Data.Binary as B

type Chars = [B.Word8]

printChars :: Chars -> IO()
printChars cs = mapM_ print cs

getChars = do
  e <- isEmpty
  if e then return []
  else do
    c <- B.get
    cs <- getChars
    return (c:cs)

main :: IO()
main = do
  input <- readFile "chars"
  printChars $ runGet getChars input
  


