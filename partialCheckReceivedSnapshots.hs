import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )

import Prelude hiding ( readFile, take )
import ReceivedSnapshots (Snapshot, check, getList)
import Data.ByteString.Lazy ( readFile, take )
import Data.Binary.Get ( isEmpty, runGet, getWord16host, getWord32host )

main = do
     args <- getArgs
     -- [nodeId_string, filename, len_string] <- getArgs
     let nid = read $ args !! 0
     let filename = args !! 1
     input <- readFile filename
     let input2 = if length args > 2 then take (read $ args !! 2) input else input
     let snapshots = runGet getList input2 :: [Snapshot]
     if check nid snapshots then
       exitSuccess
     else do
       print "FAIL"
       exitFailure

