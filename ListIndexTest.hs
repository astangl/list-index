-- Simple unit tests to check correctness of list index, i.e.,
-- that it yields same elements as underlying list.
--
-- Alex Stangl

import Test.HUnit
import ListIndex

a = [1..]
b = fromList a 4

indexedToList il = [il!n | n <- [0..]]
indexedToBackwardList il mx = [il!n | n <- [mx,mx-1..0]]

test1 = TestCase (assertEqual "sequential list" (take 10000 a) (take 10000 $ indexedToList b))
test2 = TestCase (assertEqual "backwards list" (reverse $ take 10000 a) (indexedToBackwardList b 9999))
tests = TestList [test1, test2]

main = runTestTT tests

