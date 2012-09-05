module Main where

import           Control.Monad
import           Control.Parallel.MPI.Simple


main :: IO ()
main = mpiWorld $ mpiMergeSort $ reverse [3,7,1,9,100,23,45,2,31,48,100]

mpiMergeSort :: [Int] -> Int -> Rank -> IO ()
mpiMergeSort xs size rank = do
    localdata <-  case rank of
      0 -> scatterSend commWorld 0  $ divideMessage size (replicate size []) xs
      _ -> scatterRecv commWorld 0
    putStrLn $ "Data at " ++ show rank ++ " is " ++ show localdata
    let sortedData = mergeSort localdata
    --putStrLn $ show rank ++ ": " ++ show sortedData
    sorted <- sendPartner (fromRank rank) size (power2 $ size -1) sortedData
    when (rank == 0) $ print sorted
 where
   -- | Divides a list into len lists
   divideMessage _ ys [] = ys
   divideMessage len ys left = let (zs,rest) = splitAt len left
                                  in divideMessage len (attachMessage ys zs) rest
   attachMessage [] _ = []
   attachMessage ys [] = ys
   attachMessage (y:ys) (z:zs) = (z:y):attachMessage ys zs

   -- | Generates powers of 2 uplo l
   power2 :: Int -> [Int]
   power2 l = map (2^) ([0..(floor $ logBase 2 (fromIntegral l))])

   -- | Appropriately send data to a partner or receive data from a parter.
   sendPartner _ _ [] sdata = return $ sdata
   sendPartner rank size (sendMap:rest) sdata = do
     if (rank `div` sendMap) `rem` 2 == 1
       then do --putStrLn $ "sending from" ++ show rank ++ " to "++ show (rank-sendMap) ++ " data " ++ show sdata
               send commWorld (toRank $ rank-sendMap) unitTag sdata
               return sdata
       else if (rank+sendMap < size)
               then do (msg,_status) <- recv commWorld (toRank $ rank+sendMap) unitTag
                       putStrLn $ "receiving at " ++ show rank ++ " from" ++ show (rank+sendMap) ++ " data " ++ show msg
                       sendPartner rank size rest $ merge sdata msg
               else sendPartner rank size rest sdata


-- | Performs the merge sorting
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = merge (mergeSort fs) (mergeSort sn)
    where
        (fs,sn) = splitAt (floor ((fromIntegral $ length xs)*0.5)) xs

-- | Merges two sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs) | a<b = a:merge as (b:bs)
                    | otherwise = b:merge (a:as) bs
