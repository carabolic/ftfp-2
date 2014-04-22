module Data.PriorityQueue where

import qualified Data.SkewHeap as SH

class PriorityQueue q where
    extractMin :: Ord a => q a -> Maybe (a, q a)

    singleton :: Ord a => a -> q a

    insert :: Ord a => a -> q a -> q a
    insert x heap = singleton x `union` heap

    union :: Ord a => q a -> q a -> q a

instance PriorityQueue SH.SkewHeap where
    extractMin = SH.extractMin
    singleton = SH.singleton
    union = SH.union
