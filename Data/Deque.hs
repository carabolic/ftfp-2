module Data.Deque (
      Deque
    , empty
    , null
    , addFront
    , addEnd
    , head
    , last
    , tail
    , init
    ) where

import Prelude hiding (null, head, tail, init, last)
import qualified Data.List as L (null)

-- | Size factor
c = 2 :: Int

data Deque a = Deque{
                left::[a],
                lenLeft::Int,
                right::[a],
                lenRight::Int
            }
               deriving (Show)

empty :: Deque a
empty = Deque [] 0 [] 0

null :: Deque a -> Bool
null Deque{lenLeft=ll, lenRight=lr} = ll + lr == 0

addFront :: a -> Deque a -> Deque a
addFront e d@Deque{left=l, lenLeft=ll} = norm d{left=e:l, lenLeft=ll + 1}

addEnd :: a -> Deque a -> Deque a
addEnd e d@Deque{right=r, lenRight=lr} = norm d{right=e:r, lenRight=lr + 1}

head :: Deque a -> a
head Deque{left=e:_} = e

last :: Deque a -> a
last Deque{right=e:_} = e

tail :: Deque a -> Deque a
tail d@Deque{left=_:l', lenLeft=ll} = norm d{left=l', lenLeft=ll - 1}

init :: Deque a -> Deque a
init d@Deque{right=_:r', lenRight=lr} = norm d{right=r', lenRight=lr - 1}

-- Check invariant. If one list is longer than the other (by a constant factor c)
-- 
norm :: Deque a -> Deque a
norm d@Deque{left=l, lenLeft=ll, right=r, lenRight=lr}
    -- left list is to long
    | length l > c * length r + 1 = Deque (take i l) i (r ++ reverse (drop i l)) j
    -- right list is to long
    | length r > c * length l + 1 = Deque (l ++ reverse (drop j r)) i (take j r) j
    | otherwise = d
    where
        len = ll + lr
        i = len `div` 2
        j = len - i
