

import qualified Data.Binary as B

-- see http://hackage.haskell.org/package/binary-0.4.1/docs/Data-Binary.html#t:Binary

data ReceivedSnapshotLogEntry = RSLE { nodeId :: B.Word16
                                     , timestamp :: B.Word32
                                     , x :: B.Word32
                                     , y :: B.Word32
                                     , hops :: B.Word16
                                     , hash :: B.Word8
                                     } deriving (Show)


main = print "hello"

