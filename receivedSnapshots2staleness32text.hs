import System.Environment ( getArgs )
import ReceivedSnapshots ( Snapshot, getContents, snapshots2stalenesses32, printStalenesses32 )
import Prelude hiding (getContents)
main = do
     [nodeId_string] <- getArgs
     let nid = read $ nodeId_string
     snapshots <- getContents nid :: IO [Snapshot]
     printStalenesses32 $ snapshots2stalenesses32 nid snapshots



