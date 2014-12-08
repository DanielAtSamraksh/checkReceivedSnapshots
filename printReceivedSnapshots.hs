
import System.Environment ( getArgs )
import Prelude hiding (readFile)
import ReceivedSnapshots (Snapshot, readFile, printSnapshots)

main = do
     [nodeId_string, filename] <- getArgs
     let nid = read $ nodeId_string
     snapshots <- readFile nid filename :: IO [Snapshot]
     printSnapshots snapshots