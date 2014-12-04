import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs )
import qualified Data.Binary as B

-- see http://hackage.haskell.org/package/binary-0.4.1/docs/Data-Binary.html#t:Binary
type Hash = B.Word32
type HashInternal = B.Word32
hash :: HashInternal -> HashInternal -> HashInternal
hash x y = ( a * ( x + y ) + c ) `mod` m
     where a = 1140671485
           c = 12820163
           m = 2 ^ 24

printHashes seed [] = print seed
printHashes seed (n:ns) = do print seed; printHashes (hash seed n) ns

main = do
     strings <- getArgs
     let nums = map read strings
     printHashes 0 nums

