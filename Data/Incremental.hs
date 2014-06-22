module Data.Incremental where

import Data.CPO

import qualified Data.Set as S

class (CPO a) => Incremental a where
    workset :: a -> a -> S.Set b
    increment :: a -> b -> a
