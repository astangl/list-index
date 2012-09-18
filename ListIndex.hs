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

module ListIndex ((!), fromList, LI) where

-- | Type of index wrapping an underlying list
data LI a = LI Int [LInode a]
data LInode a = FullNode (LInode a) (LInode a) | LeafNode [a] (LInode a)

-- | Constructs index from specified list and fanout
fromList :: [a] -> Int -> LI a
fromList l fo =
  let mkLeafNode l = LeafNode l (mkLeafNode (drop fo l))
      mkFullNode l = FullNode l (mkFullNode (nodeDrop fo l))
  in LI fo (iterate mkFullNode (mkLeafNode l))

-- drop i nodes from a linear node stream
nodeDrop :: Int -> LInode a -> LInode a
nodeDrop 0 n = n
nodeDrop i n = let i' = i - 1
               in case n of
                    FullNode _ n' -> nodeDrop i' n'
                    LeafNode _ n' -> nodeDrop i' n'

-- | access specified element of underlying list using index to speed access
(!) :: LI a -> Int -> a
(!) (LI fo ns) i =
  let getLevel k (n : ns) = let (q,r) = k `quotRem` fo
                                l = if q == 0
                                      then n
                                      else parent $ getLevel q ns
                            in nodeDrop r l
      parent (FullNode p _) = p
      (q, r) = i `quotRem` fo
      (LeafNode l _) = getLevel q ns
  in l !! r
