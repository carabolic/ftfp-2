module Data.SimpleQueue (
      SimpleQueue
    , empty
    , singleton
    , null
    , add
    , head
    , tail
    ) where

import Prelude hiding (null, head, tail)

data SimpleQueue a = SQ [a] [a]
                     deriving (Show) 

empty :: SimpleQueue a
empty = SQ [] []

singleton :: a -> SimpleQueue a
singleton e = SQ [e] []

null :: SimpleQueue a -> Bool
null (SQ [] []) = True
null _          = False

add :: a -> SimpleQueue a -> SimpleQueue a
add e (SQ left right) = norm $ SQ left (e:right)

head :: SimpleQueue a -> a
head (SQ [] _)    = error "empty queue"
head (SQ (x:_) _) = x

tail :: SimpleQueue a -> SimpleQueue a
tail (SQ [] _)         = error "empty queue"
tail (SQ (_:xs) right) = norm $ SQ xs right

-- | Invariant check.
norm :: SimpleQueue a -> SimpleQueue a
norm q@(SQ left right)
    | length left >= length right = q
    | otherwise = SQ (left ++ reverse right) []
