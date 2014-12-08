module ReceivedSnapshots where

import System.Exit ( exitFailure, exitSuccess )
import Data.ByteString.Lazy (ByteString)
import Prelude hiding ( readFile, getContents, putStr )
import qualified Data.ByteString.Lazy as BL ( readFile, getContents, putStr )
import Control.Monad (when)

import qualified Data.Binary as B
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( (!) )

import Data.Binary.Get ( isEmpty, runGet, getWord16host, getWord32host, getWord64host )

import Math.NumberTheory.Powers.Squares (integerSquareRoot)

-- see http://hackage.haskell.org/package/binary-0.4.1/docs/Data-Binary.html#t:Binary
type Hash = B.Word32
type NodeId = B.Word16
type Time = B.Word64
type Distance = B.Word32
type Hops = B.Word16

data Snapshot = Snapshot { now :: Time
                         , timestamp :: Time
                         , nodeId :: NodeId
                         , hops :: Hops
                         , x :: Distance
                         , y :: Distance
                         , checkSum :: Hash
                         } -- deriving (Show)

instance Show Snapshot where
  show s = "receivedTime=" ++ ( show $ now s )
    ++ ", " ++ "timestamp=" ++ ( show $ timestamp s )
    ++ ", " ++ "nodeId=" ++ ( show $ nodeId s )
    ++ ", " ++ "hops=" ++ ( show $ hops s )
    ++ ", " ++ "x=" ++ ( show $ x s )
    ++ ", " ++ "y=" ++ ( show $ y s )
    ++ ", " ++ "copTableHash=" ++ ( show $ checkSum s )
    
instance B.Binary Snapshot where
  put s = do
    B.put $ now s
    B.put $ timestamp s
    B.put $ nodeId s
    B.put $ hops s
    B.put $ x s
    B.put $ y s
    B.put $ checkSum s
  get = do
    r <- getWord64host
    t <- getWord64host
    n <- getWord16host
    h <- getWord16host
    x <- getWord32host
    y <- getWord32host
    c <- getWord32host
    return $ Snapshot { now = r
                      , timestamp = t
                      , nodeId = n
                      , hops = h
                      , x = x
                      , y = y
                      , checkSum = c
                      }

type CopTable = Map.Map B.Word16 Snapshot

type HashInternal = B.Word64
hash :: HashInternal -> HashInternal -> HashInternal
hash x y = ( a * ( x + y ) + c ) `mod` m
     where a = 1140671485
           c = 12820163
           m = 2 ^ 24

hashRow :: HashInternal -> Snapshot -> HashInternal
hashRow h r = h `hash` ( fromIntegral $ nodeId r )
                `hash` ( fromIntegral $ hops r )
                `hash` timestamp r
                `hash` ( fromIntegral $ x r )
                `hash` ( fromIntegral $ y r )


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

--getList :: B.Binary a => B.Get [ a ]
getList :: B.Get [Snapshot]
getList = do
  e <- isEmpty
  if e then return []
  else do
    x <- B.get
    xs <- getList
    return (x:xs)

die :: String -> IO a
die err = return $ error err
-- die err = do putStrLn err
--              exitFailure
             


printSnapshots :: [Snapshot] -> IO()
printSnapshots ss = mapM_ print ss

putSnapshots :: [Snapshot] -> IO()
putSnapshots ss = mapM_ (BL.putStr . B.encode) ss

readFile :: NodeId -> String -> IO [Snapshot]
readFile nid filename = do
  input <- BL.readFile filename
  let snapshots = runGet getList input :: [Snapshot]
  when (not $ check nid snapshots) $ die "copHashes don't match."
  return snapshots

getContents :: NodeId -> IO [Snapshot]
getContents nid = do
  input <- BL.getContents
  let snapshots = runGet getList input :: [Snapshot]
  when (not $ check nid snapshots) $ die "copHashes don't match."
  return snapshots

dist :: Snapshot -> Snapshot -> Distance
dist a b = integerSquareRoot $ (x a - x b) ^ 2 + ( y a - y b ) ^2

data Staleness = Staleness Time Distance
snapshots2stalenesses :: NodeId -> [Snapshot] -> [ Staleness ]
snapshots2stalenesses nid snapshots = ss
  where (ss, m ) = foldl f ([], Map.empty) snapshots -- ss=stalenesses, m=map, 
        f ( ss, m ) s
          | Map.notMember (nodeId s) m
            = ( ss, Map.insert (nodeId s) s m )
          | nid == nodeId s
            = ( ss, if ( timestamp $ m ! nid ) < timestamp s
                    then Map.insert nid s m
                    else m )
          | ( timestamp $ m ! nodeId s ) < timestamp s
            = ( (Staleness  (now s - (timestamp $ m ! nodeId s)) (dist s $ m ! nid)):ss,
                Map.insert (nodeId s) s m )
          | otherwise = ( ss, m )
       

instance B.Binary Staleness where
  put (Staleness t d) = do B.put t; B.put d
  get = do t <- getWord64host; d <- getWord32host; return $ Staleness t d

putStalenesses :: [Staleness] -> IO()
putStalenesses = BL.putStr . B.encode
          
