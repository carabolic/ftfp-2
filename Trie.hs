module Trie
    (
      Trie  
    , empty
    , insert
    , lkup
    , remove 
    ) where

import Data.Maybe (fromMaybe, fromJust)
import qualified AList as A

data Trie a = Node (Maybe a) (A.AList (Trie a))
            deriving (Show)

empty :: Trie a
empty = Node Nothing []

insert :: String -> a -> Trie a -> Trie a
insert []     value (Node _ children) = Node (Just value) children
insert (c:cs) value (Node v children) = Node v (A.put c (insert cs value newChild) children)
    where
        child = A.get c children
        newChild = fromMaybe empty child

lkup :: String -> Trie a -> Maybe a
lkup []     (Node value _)    = value
lkup (c:cs) (Node _ children) = maybe Nothing (lkup cs) $ A.get c children

-- | Removes the given string.
remove :: String -> Trie a -> Trie a
remove []     (Node _ children)   = Node Nothing children
remove (c:cs) t@(Node value children) = if c `A.isIn` children
                                          then newNode
                                          else t
    where
        subTrie = remove cs $ fromJust $ A.get c children
        newNode = Node value (if isLeaf subTrie
                      then A.del c children
                      else A.put c subTrie children)

isLeaf :: Trie a -> Bool
isLeaf (Node _ []) = True
isLeaf _           = False
