bfs :: Int -> Int -> A.Array Int [Int] -> UArray Int Int
bfs start n graph = runSTUArray $ do
  dist <- MA.newArray (0, n-1) (-1) :: ST s(STUArray s Int Int)
  queue <- newSTRef (Seq.singleton (start)) :: ST s(STRef s(Seq.Seq Int))
  MA.writeArray dist start 0
  bfs' dist queue graph
  return dist
bfs' :: STUArray s Int Int -> STRef s(Seq.Seq Int) -> A.Array Int [Int] -> ST s()
bfs' dist queue graph = do
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
        modifySTRef queue(flip(Seq.|>) v2)
    bfs' dist queue graph
