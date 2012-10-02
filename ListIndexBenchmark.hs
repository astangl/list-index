-- Benchmark list access using list-index versus using (!!) on
-- the underlying list, using Criterion.
--
--module Main where

import ListIndex
import Criterion.Main
import System.Random

a = [1 :: Int ..]
ia = [fromList a n | n <- [2..33]]

sumBackward :: Num a => (Int -> a) -> Int -> a
sumBackward f hi = sum [f n | n <- [hi,hi-1..0]]
sumForward :: Num a => (Int -> a) -> Int -> a
sumForward f hi = sum [f n | n <- [0..hi]]

randAccess :: Num a => (Int -> a) -> Int -> a
randAccess f hi =
             let seed = 12345813
                 g = mkStdGen seed
                 lst = [1,3..hi]
                 nIter = 10000
                 randR _ 0 = []
                 randR g n = let (a,g') = randomR (0, hi `div` 2 - 1) g
                                 n' = n - 1
                             in (f a) : randR g' n'
             in sum $ randR g nIter

main = let hi = 50000
       in defaultMain ([
            bench "sumForward raw list" (nf (sumForward (a!!)) hi),
            bench "sumBackward raw list" (nf (sumBackward (a!!)) hi),
            bench "randAccess raw list" (nf (randAccess (a!!)) hi)
            ]
            ++ [bench ("sumForward indexed list, fanout " ++ (show fo))
              (nf (sumForward (ia!!(fo-2)!)) hi) | fo <- [2..33]]
            ++ [bench ("sumBackward indexed list, fanout " ++ (show fo))
              (nf (sumBackward (ia!!(fo-2)!)) hi) | fo <- [2..33]]
            ++ [bench ("randAccess indexed list, fanout " ++ (show fo))
              (nf (randAccess (ia!!(fo-2)!)) hi) | fo <- [2..33]])

