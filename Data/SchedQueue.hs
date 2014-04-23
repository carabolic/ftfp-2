module Data.SchedQueue (
      SchedQueue
    , empty
    , singleton
    , null
    , add
    , head
    , tail  
    ) where

import Prelude hiding (null, head, tail)
import qualified Data.List as L (null, head, tail)

data SchedQueue a = SQ [a] [a] [a]
                    deriving (Show)

empty :: SchedQueue a
empty = SQ [] [] []

singleton :: a -> SchedQueue a
singleton e = SQ [e] [e] []

null :: SchedQueue a -> Bool
null (SQ left _ _) = L.null left

add :: a -> SchedQueue a -> SchedQueue a
add e (SQ left sched right) = norm $ SQ left sched (e : right)

head :: SchedQueue a -> a
head (SQ []   _ _) = error "empty queue"
head (SQ left _ _) = L.head left

tail :: SchedQueue a -> SchedQueue a
tail (SQ []   _     _)     = error "empty queue"
tail (SQ left sched right) = norm $ SQ (L.tail left) sched right 

-- | Check invariant
norm :: SchedQueue a -> SchedQueue a
norm (SQ left []    right) = SQ l' l' []
    where
        l' = rotate left [] right
norm (SQ left sched right) = SQ left (L.tail sched) right

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] sched right = L.head right : sched
rotate (x:xs) sched (z:zs) = x : rotate xs (z : sched) zs
