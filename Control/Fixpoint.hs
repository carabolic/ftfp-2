module Control.Fixpoint where

import Prelude hiding (iterate)
--import Data.POrd
import Data.CPO
import Data.Incremental
import Data.Word
import qualified Data.Set as S

condFixpoint :: (CPO a) => (a -> a) -> a -> a
condFixpoint f ws = if ws' == ws then ws else condFixpoint f ws'
    where
        ws' = f ws

fixpoint :: (CPO a) => (a -> a) -> a
fixpoint f = condFixpoint f bottom

fixpoint' :: (Incremental a) => (a -> a) -> a -> a
fixpoint' f ws = iterate f ws (workset ws (f ws)) 

iterate :: (Incremental a) => (a -> a) -> a -> S.Set b -> a
iterate f s ws | S.null ws = s
               | otherwise = iterate f s' (workset s' (f s'))
    where
        d = S.findMin ws
        s' = increment s d

-- Test function copied from
-- Pepper, Hofstedt "Funktionale Programmierung" p. 192

-- | testFunction1 is continous function computing @1^n@.
testFunction1 :: Word -> Word
testFunction1 n = 1 ^ n

testFunction3 :: Int -> Int
testFunction3 x = x ^ (2 :: Int)

testFunction4 :: S.Set Int -> S.Set Int
testFunction4 = S.union (S.fromList [1, 7])

type Start a b = a -> (a, S.Set b)
type More a b = (a, S.Set b) -> Bool
type Step a b = (a, S.Set b) -> (a, S.Set b)

condFixpointFD :: Start a b -> More a b -> Step a b -> a -> a
condFixpointFD start more step s = iter (start s)
    where
        iter ws = if more ws
                    then iter (step ws)
                    else fst ws
