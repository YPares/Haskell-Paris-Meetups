{-# LANGUAGE BangPatterns #-}

module Main where

average1 :: [Double] -> Double
average1 list = ({-# SCC "foldl" #-} foldl (+) 0 list) /
                ({-# SCC "length" #-} fromIntegral $ length list)

average2 :: [Double] -> Double
average2 list = sum / length
  where (length, sum) = {-# SCC "foldl" #-} foldl (\(l, s) x -> (l+1, s+x))
                              (0, 0)
                              list

average3 :: [Double] -> Double
average3 list = sum / length
  where (length, sum) = {-# SCC "foldl" #-} foldl (\(!l, !s) x -> (l+1, s+x))
                              (0, 0)
                              list

myList = [1..10000000]

main = print $ average3 myList

