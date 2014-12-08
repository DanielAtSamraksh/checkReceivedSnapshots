import System.Environment ( getArgs )
import ReceivedSnapshots ( Snapshot, getContents, snapshots2stalenesses, putStalenesses )
import Prelude hiding (getContents)
main = do
     [nodeId_string] <- getArgs
     let nid = read $ nodeId_string
     snapshots <- getContents nid :: IO [Snapshot]
     putStalenesses $ snapshots2stalenesses nid snapshots



