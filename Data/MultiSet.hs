module Data.MultiSet (MultiSet (..)) where

import Prelude hiding (null)
import qualified Prelude as L (null)

class MultiSet s where
    empty :: s a

    singleton :: a -> s a

    null :: s a -> Bool

    size :: Eq a => s a -> Int
    --size s = 1 + size s'
    --    where
    --        s' = delete (arb s) s

    -- | Number of occurences of the element in the MultiSet
    occur :: Eq a => a -> s a -> Int

    member :: Eq a => a -> s a -> Bool

    -- | Inserts one occurence to the MultiSet
    insert :: a -> s a -> s a

    -- | Deletes one occurence of the element from the MultiSet
    delete :: Eq a => a -> s a -> s a

    arb :: s a -> a

    union :: Eq a => s a -> s a -> s a
    union = union' empty
        where
            union' result first second
                | null first && null second = result
                | null first                = union' result' first  second'
                | null second               = union' result' first' second
                | otherwise                 = union' result' first' second'
                where
                    v = arb (if null first then second else first)
                    result' = insert v result
                    first' = delete v first
                    second' = delete v second

    -- | Returns all elements contained in both MultiSets S ∩ T := {x : x ∈ S ∧ x ∈ T}
    intersect :: Eq a => s a -> s a -> s a
    intersect = intersect' empty
        where
            intersect' result first second
                | null first || null second             = result
                | v `member` first && v `member` second = intersect' result' first' second'
                | otherwise                             = intersect' result first' second'
                where
                    v = arb first
                    first' = delete v first
                    second' = delete v second
                    result' = insert v result

instance MultiSet [] where
    empty = []
    singleton a = [a]
    null = L.null
    size = length
    occur a = length . filter (== a)
    member = elem
    insert = (:)
    delete _ [] = []
    delete e (x:xs)
        | e == x    = xs
        | otherwise = x : delete e xs
    arb []     = error "Using arb on empty list"
    arb (x:_)  = x
    --union = union' []
    --    where
    --        union' result [] ys       = result ++ ys
    --        union' result xs []       = result ++ xs
    --        union' result (x:xs) (ys) = union' (x:result) xs (delete x ys)
    --intersect = intersect' []
    --    where
    --        intersect' result [] _ = result
    --        intersect' result _ [] = result
    --        intersect' result (x:xs) ys
    --            | x `member` ys = intersect' (x:result) xs (delete x ys)
    --            | otherwise     = intersect' result xs ys
