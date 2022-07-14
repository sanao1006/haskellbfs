import qualified Data.Sequence as Seq
import qualified Data.Array.MArray as MA
import Data.Array.ST
import Data.STRef


type Graph = A.Array Int [Int]
type Queue = STRef s(Seq.Seq Int)


bfs :: Int -> Int -> Graph -> UArray Int Int
bfs start n graph = runSTUArray $ do
  dist <- MA.newArray (0, n-1) (-1) :: ST s(STUArray s Int Int)
  queue <- newSTRef (Seq.singleton (start)) :: ST s Queue
  MA.writeArray dist start 0
  bfsInternal dist queue graph
  return dist

bfsInternal :: STUArray s Int Int -> Queue -> Graph -> ST s()
bfsInternal dist queue graph = do
  que <- readSTRef queue
  if(Seq.null que) then pure()
  else do
    v <- seqfront <$> Seq.viewl <$> readSTRef queue
    modifySTRef queue (seqpop <$> Seq.viewl)
    forM_ (graph A.! v) $ \v2 -> do
      distv2 <- MA.readArray dist v2
      when(distv2 == (-1)) $ do
        distv <- MA.readArray dist v
        MA.writeArray dist v2 (distv + 1)
        modifySTRef' queue(flip(Seq.|>) v2)
    bfsInternal dist queue graph
