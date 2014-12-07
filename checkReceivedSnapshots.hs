
import Control.Monad (when)
import qualified Data.Binary as B
import qualified Data.Map.Strict as Map
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )

import Prelude hiding ( readFile, take )
import ReceivedSnapshots ( readFile )

import ReceivedSnapshots

die :: String -> IO a
die err = do putStrLn err
             exitFailure
             
main = do
     [nodeId_string, filename, len_string] <- getArgs
     let nid = read $ nodeId_string
     snapshots <- readFile nid filename :: IO [Snapshot]
     printSnapshots snapshots
     exitSuccess
