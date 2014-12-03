

import qualified Data.Binary as B
import qualified Data.Map.Strict as Map
import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )
-- see http://hackage.haskell.org/package/binary-0.4.1/docs/Data-Binary.html#t:Binary
type Hash = B.Word8
type NodeId = B.Word16
data Snapshot = Snapshot { nodeId :: NodeId
                         , timestamp :: B.Word32
                         , x :: B.Word32
                         , y :: B.Word32
                         , hops :: B.Word16
                         , checkSum :: Hash
                         } deriving (Show)

instance B.Binary Snapshot where
  put s = do
    B.put $ nodeId s
    B.put $ timestamp s
    B.put $ x s
    B.put $ y s
    B.put $ hops s
    B.put $ checkSum s
  get = do
    n <- B.get
    t <- B.get
    x <- B.get
    y <- B.get
    h <- B.get
    c <- B.get
    return $ Snapshot { nodeId = n
                      , timestamp = t
                      , x = x
                      , y = y
                      , hops = h
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
                `hash` timestamp r
                `hash` x r
                `hash` y r
                `hash` ( fromIntegral $ hops r )

checkTable :: CopTable -> Hash
checkTable t = fromIntegral $ Map.foldl hashRow 0 t

updateRow :: NodeId -> Snapshot -> Snapshot -> Snapshot
updateRow myNodeId old new
       | timestamp old > timestamp new =        old
       | myNodeId == nodeId new =               new { hops = 0 }
       | timestamp old < timestamp new =        new { hops = 1 + hops new }
       | hops old > hops new =                  new { hops = 1 + hops new }
       | otherwise =                            old

check :: NodeId -> [ Snapshot ] -> Bool
check nid ss = f Map.empty ss
  where f _ [] = True
        f t (s:ss) = let n = nodeId s
                     in let t' = Map.insertWith (updateRow n) n s t
                        in if checkTable t' == checkSum s then f t' ss else False

main = do
     args <- getArgs
     let nodeId = read $ args !! 0
     snapshots <- B.decodeFile $ args !! 1
     if check nodeId snapshots then exitSuccess else exitFailure
     print "hello"

