{-# LANGUAGE InstanceSigs #-}

module Data.CPO where

import Data.POrd
import Data.Maybe
import Data.Word
import qualified Data.Set as S
import qualified Data.Trie as T

class (POrd a) => CPO a where
    -- | Bottom element. Defaults to `undefined`.
    --
    -- prop> bottom `le` a
    bottom :: a
    bottom = undefined

    -- | Least upper bound.
    -- For 
    --
    -- Laws:
    --
    -- prop> a `le` (a `lub` b) && b `le` (a `lub` b)
    -- prop> a `le` c && b `le` c && (a `lub` b) `le` c
    --
    -- Note: The default implementation @lub a b = if a <= b then b else a@
    -- does not work since @a@ and @b@ might not be comparable. Hence returning
    -- @b@ would violate the contract.
    lub :: a -> a -> a

instance CPO Int where
    bottom = minBound
    lub = max

instance CPO Word where
    bottom = minBound
    lub = max

instance (CPO a) => CPO (Maybe a) where
    bottom = Nothing

    lub Nothing y         = y
    lub x Nothing         = x
    lub (Just x) (Just y) = Just (x `lub` y)

instance (Ord a) => CPO (S.Set a) where
    bottom = S.empty
    lub = S.union

instance (CPO b) =>CPO (T.Trie b) where
    bottom = T.empty
    lub m1 m2 = foldr merge T.empty dom'
        where
            -- list of unique values (from both domains)
            dom' = S.toList $ T.dom m1 `S.union` T.dom m2
            -- lookup for key -> value, return bottom if none found
            lkup' k m = fromMaybe bottom $ T.lkup k m
            merge k = T.insert k (lkup' k m1 `lub` lkup' k m2)

instance (Eq a) => CPO [a] where
    bottom = []

    -- | This is a partial function. It is only defined if:
    --
    -- @l1 `le` l2 || l2 `le` l1@
    lub l1 l2
        | l1 `le` l2 = l2
        | l2 `le` l1 = l1
        | otherwise  = undefined
