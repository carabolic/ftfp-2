{-# LANGUAGE CPP #-}

module Data.POrd where

import Data.List
import Data.Maybe
import Data.Word
import qualified Data.Set as S
import qualified Data.Trie as T

#if __GLASGOW_HASKELL__ < 708
-- | Minimal complete definition: @le@.
#endif
class (Eq a) => POrd a where
    compare :: a -> a -> Maybe Ordering
    compare a b = case (le a b, le b a) of
        (True,  True )  -> Just EQ
        (True,  False)  -> Just LT
        (False, True )  -> Just GT
        _               -> Nothing

    -- | Check whether @a <= b@.
    --
    -- prop> a `le` a == True
    -- prop> a `le` b && b `le` a == (a == b)
    -- prop> a `le` b && b `le` c == a `le` c
    le :: a -> a -> Bool

    eq :: a -> a -> Bool
    x `eq` y = x `le` y && y `le` x

    ge :: a -> a -> Bool
    x `ge` y = y `le` x

#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL le #-}
#endif

-- TODO make Ord a superclass of POrd (somehow?)

instance POrd Word where
    le = (<=)

instance POrd Int where
    le = (<=)

instance POrd Integer where
    le = (<=)

instance POrd Bool where
    le = (<=)

instance POrd Float where
    le = (<=)

instance POrd Double where
    le = (<=)

instance (POrd a) => POrd (Maybe a) where
    le Nothing _       = True
    le _       Nothing = False
    le (Just a) (Just b) = a `le` b

-- | Partial order over lists.
--
-- prop> (p1 ++ xs) `le` (p2 ++ ys) == (p1 == p2)
instance (Eq a) => POrd [a] where
    le = isPrefixOf

-- | Partial order over sets.
--
-- prop> s1 `le` s2 == s1 isSubsetOf s2
instance (Ord a) => POrd (S.Set a) where
    le = S.isSubsetOf

instance (POrd b) => POrd (T.Trie b) where
    le m1 m2 = T.dom m1 `S.isSubsetOf` T.dom m2
               && T.foldTK
                  (\key val acc -> acc && val `le` fromJust (T.lkup key m2))
                  True m2
