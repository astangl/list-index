-- Simple tests to check efficiency of ListIndex vs. direct
-- list access for sequential access (via !!) and pseudorandom
-- access.
--
-- Alex Stangl

import System.Random
import ListIndex

a = [1..]
b = fromList a 4

testSequential = [(!) b n | n <- [1,3..100000]]
--testSequential = [a!!n | n <- [1,3..100000]]

randAccess = let seed = 12345813
                 g = mkStdGen seed
                 hi = 10000000
                 lst = [1,3..hi]
                 lst' = fromList lst 32
                 nIter = 1000
                 randR _ 0 = []
                 randR g n = let (a,g') = randomR (0, hi `div` 2 - 1) g 
                                 n' = n - 1
                             --in (lst!!a) : randR g' n'
                             in (lst'!a) : randR g' n'
             in sum $ randR g nIter
main = putStrLn $ show $ randAccess

