module Solver where

import Control.Fixpoint
import Data.CPO
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Trie as T

type Solution a = T.Trie a
-- Trie (Trie (Set Char) -> Set Char)
type Eqs a = T.Trie (Solution a -> a)

(!) :: (CPO a) => T.Trie a -> String -> a
sol ! var = fromMaybe bottom (T.lkup var sol)

x :: T.Trie (S.Set Char) -> S.Set Char
x s = (s!"x") `S.union` S.singleton 'a' `S.union` (s!"y")

y :: T.Trie (S.Set Char) -> S.Set Char
y s = (s!"y") `S.union` S.singleton 'c'

z :: T.Trie (S.Set Char) -> S.Set Char
z s = (s!"z") `S.union` S.singleton 'd' `S.union` (s!"x") `S.union` (s!"y")

eqs :: Eqs (S.Set Char)
eqs = T.fromList (zip ["x", "y", "z"] [x, y, z])

solve :: (CPO a) => Eqs a -> Solution a
solve eq = fixpoint step
    where
        step sol = T.mapT (\f -> f sol) eq
