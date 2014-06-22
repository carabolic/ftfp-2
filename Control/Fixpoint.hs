module Control.Fixpoint where

import Data.POrd
import Data.CPO
import Data.Word
import qualified Data.Set as S

condFixpoint :: (CPO a) => (a -> a) -> a -> a
condFixpoint f ws = if ws' == ws then ws else condFixpoint f ws'
    where
        ws' = f ws

fixpoint :: (CPO a) => (a -> a) -> a
fixpoint f = condFixpoint f bottom

-- Test function copied from
-- Pepper, Hofstedt "Funktionale Programmierung" p. 192

-- | testFunction1 is continous function computing @1^n@.
testFunction1 :: Word -> Word
testFunction1 n = 1 ^ n

testFunction3 :: Int -> Int
testFunction3 x = x ^ (2 :: Int)

testFunction4 :: S.Set Int -> S.Set Int
testFunction4 = S.union (S.fromList [1, 7])
