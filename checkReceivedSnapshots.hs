import System.Environment ( getArgs )
import Prelude hiding (readFile)
import ReceivedSnapshots (Snapshot, readFile)

main = do
     [nodeId_string, filename] <- getArgs
     let nid = read $ nodeId_string
     readFile nid filename :: IO [Snapshot]
