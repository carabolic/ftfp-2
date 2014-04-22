module Data.SkewHeap (
      SkewHeap
    , empty
    , singleton
    , insert
    , delete
    , extractMin
    , union
    ) where

data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) 
                  deriving (Show)

empty :: SkewHeap a
empty = Empty

singleton :: a -> SkewHeap a
singleton x = SkewNode x Empty Empty

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x heap = singleton x `union` heap

delete :: a -> SkewHeap a -> SkewHeap a
delete = undefined

extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty = Nothing
extractMin (SkewNode x l r) = Just (x, l `union` r)

union :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
union heap1@(SkewNode x1 l1 r1) heap2@(SkewNode x2 l2 r2)
    | x1 <= x2 = SkewNode x1 (heap2 `union` r1) l1
    | otherwise = SkewNode x2 (heap1 `union` r2) l2
union Empty heap = heap
union heap Empty = heap