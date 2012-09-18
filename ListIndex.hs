{- |
Module      : ListIndex
Description : list indexer, providing fast random list lookup
Copyright   : (c) Alex Stangl 2012
License     : BSD3

Maintainer  : alex@stangl.us
Stability   : unstable
Portability : portable

Wrap a list, providing faster access, like a skip-list. This was primarily
written to wrap an infinite list, lazily building the index as the list is
accessed. For finite lists, an array is probably a better choice.
Fanout controls how much each level of the index jumps, or "fans out" from
the previous level. 4, 8, 16, or 32 would be a good default choice for this,
but feel free to experiment. Memory usage should be roughly proportional to 
accessed list size / (fanout - 1)
-}

module ListIndex (fromList, (!), LI) where

-- | Type of index wrapping an underlying list
data LI a = LI Int [LInode a]
data LInode a = LiNonLeaf (LInode a) (LInode a) | LiLeaf (LInode a) [a]

-- | Constructs index from specified list and fanout
fromList :: [a] -> Int -> LI a
fromList l fo =
  let topLevel = mkTopLevelNode l
      mkTopLevelNode l = LiLeaf (mkTopLevelNode (drop fo l)) l
      mkLevel plv = let lv = mkMidLevelNode plv
                    in lv : mkLevel lv
      mkMidLevelNode l = LiNonLeaf (mkMidLevelNode (nodeDrop fo l)) l
  in LI fo (topLevel : mkLevel topLevel)

-- drop i nodes from a linear node stream
nodeDrop :: Int -> LInode a -> LInode a
nodeDrop 0 n = n
nodeDrop i n = let i' = i - 1
               in case n of
                    LiNonLeaf n' _ -> nodeDrop i' n'
                    LiLeaf    n' _ -> nodeDrop i' n'

-- | access specified element of underlying list using index to speed access
(!) :: LI a -> Int -> a
(!) (LI fo ns) i =
  let getLevel k (n : ns) = let (q,r) = k `quotRem` fo
                                l = if q == 0
                                      then n
                                      else parent $ getLevel q ns
                            in nodeDrop r l
      parent (LiNonLeaf _ p) = p
      (q, r) = i `quotRem` fo
      (LiLeaf _ l) = getLevel q ns
  in l !! r
