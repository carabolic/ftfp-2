{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Set where

import qualified Data.MultiSet as M

class Set s where
    empty :: s a

    singleton :: a -> s a

    member :: Eq a=> a -> s a -> Bool

    arb :: s a -> a

    insert :: Eq a => a -> s a -> s a

    delete :: Eq a => a -> s a -> s a

    union :: Eq a => s a -> s a -> s a

    intersect :: Eq a => s a -> s a -> s a

instance (M.MultiSet s) => Set s where
    empty = M.empty
    singleton = M.singleton
    member = M.member
    arb = M.arb
    insert e s = if e `M.member` s then s else M.insert e s
    delete = M.delete
    union = M.union
    intersect = M.intersect
