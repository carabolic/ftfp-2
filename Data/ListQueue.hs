module Data.ListQueue where

import Prelude hiding (null, head, tail)

type ListQueue a = [a]

empty :: ListQueue a
empty = []

singleton :: a -> ListQueue a
singleton e = [e]

null :: ListQueue a -> Bool
null [] = True
null _  = False

add :: a -> ListQueue a -> ListQueue a
add e q = q ++ [e]

head :: ListQueue a -> a
head []    = error "empty queue"
head (x:_) = x

tail :: ListQueue a -> ListQueue a
tail []     = error "empty queue"
tail (_:xs) = xs