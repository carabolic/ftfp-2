module Data.AssociativeArray where

import qualified Data.Trie as T

class AssociativeArray a where
    insert :: String -> e -> a e -> a e

    delete :: String -> a e -> a e

    get :: String -> a e -> Maybe e

instance AssociativeArray T.Trie where
    insert = T.insert
    get = T.lkup
    delete = T.remove