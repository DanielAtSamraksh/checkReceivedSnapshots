

import qualified Data.Binary as B
import qualified Data.Map.Strict as Map
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )

import Prelude hiding ( readFile, take )
import Data.ByteString.Lazy ( readFile, take )
import Data.Binary.Get ( isEmpty, runGet, getWord16host, getWord32host )

-- see http://hackage.haskell.org/package/binary-0.4.1/docs/Data-Binary.html#t:Binary
type Hash = B.Word32
type NodeId = B.Word16
data Snapshot = Snapshot { now :: B.Word32
                         , nodeId :: NodeId
                         , hops :: B.Word16
                         , timestamp :: B.Word32
                         , x :: B.Word32
                         , y :: B.Word32
                         , checkSum :: Hash
                         } deriving (Show)

instance B.Binary Snapshot where
  put s = do
    B.put $ now s
    B.put $ nodeId s
    B.put $ hops s
    B.put $ timestamp s
    B.put $ x s
    B.put $ y s
    B.put $ checkSum s
  get = do
    r <- getWord32host
    n <- getWord16host
    h <- getWord16host
    t <- getWord32host
    x <- getWord32host
    y <- getWord32host
    c <- getWord32host
    return $ Snapshot { now = r
                      , nodeId = n
                      , hops = h
                      , timestamp = t
                      , x = x
                      , y = y
                      , checkSum = c
                      }
  
             
type CopTable = Map.Map B.Word16 Snapshot

type HashInternal = B.Word32
hash :: HashInternal -> HashInternal -> HashInternal
hash x y = ( a * ( x + y ) + c ) `mod` m
     where a = 1140671485
           c = 12820163
           m = 2 ^ 24

hashRow :: HashInternal -> Snapshot -> HashInternal
hashRow h r = h `hash` ( fromIntegral $ nodeId r )
                `hash` ( fromIntegral $ hops r )
                `hash` timestamp r
                `hash` x r
                `hash` y r


checkTable :: CopTable -> Hash
checkTable t = fromIntegral $ Map.foldl hashRow 0 t

updateRow :: NodeId -> Snapshot -> Snapshot -> Snapshot
updateRow myNodeId old new -- note that new has already had hops updated
       | timestamp old > timestamp new =        old
       | myNodeId == nodeId new =               new { hops = 0 }
       | timestamp old < timestamp new =        new
       | hops old > hops new =                  new
       | otherwise =                            old

check :: NodeId -> [ Snapshot ] -> Bool
check nid ss = f Map.empty ss
  where f _ [] = True
        f t (s:ss) = let n = nodeId s
                         s' = if n == nid then s else s{ hops=1+hops s }
                         t' = Map.insertWith (updateRow nid) n s' t
                     in if checkTable t' == checkSum s then f t' ss else False

getList = do
  e <- isEmpty
  if e then return []
  else do
    x <- B.get
    xs <- getList
    return (x:xs)

printSnapshots :: [Snapshot] -> IO()
printSnapshots ss = mapM_ print ss

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
     else
       exitFailure


