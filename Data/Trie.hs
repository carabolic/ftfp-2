module Data.Trie
    (
      Trie  
    , empty
    , insert
    , lkup
    , remove
    , mapT
    , mapTK
    , filterT
    , filterTK
    , foldT
    , foldTK
    , insertTrie
    , asList
    , fromList
    ) where

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.AList as A

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

mapT :: (a -> b) -> Trie a -> Trie b
mapT func = mapTK (\_ v -> func v)

mapTK :: (String -> a -> b) -> Trie a -> Trie b
mapTK func = mapTK' ""
  where
    mapTK' key (Node value children) =
      Node (fmap (func key) value)
           (map (\(c, v) -> (c, mapTK' (key ++ [c]) v)) children)

filterT :: (a -> Bool) -> Trie a -> Trie a
filterT p = filterTK (\_ v -> p v) 

filterTK :: (String -> a -> Bool) -> Trie a -> Trie a
filterTK = filterTK' ""
  where
    filterTK' key p (Node (Just value) children) = Node (if p key value then Just value else Nothing) (map (\(c, v) -> (c, filterTK' (key ++ [c]) p v)) children)
    filterTK' key p (Node Nothing children) = Node Nothing (map (\(c, v) -> (c, filterTK' (key ++ [c]) p v)) children)

foldT :: (a -> b -> b) -> b -> Trie a -> b
foldT func = foldTK (\_ v -> func v)

foldTK :: (String -> a -> b -> b) -> b -> Trie a -> b
foldTK = foldTK' ""
  where
    foldTK' key func acc (Node value children) = foldl (\a (c, v) -> foldTK' (key ++ [c]) func a v) (maybe acc (\v -> func key v acc) value) children

insertTrie :: Trie a -> Trie a -> Trie a
insertTrie = foldTK insert

asList :: Trie a -> [(String, a)]
asList = foldTK (\key value list -> (key, value):list) []

fromList :: [(String, a)] -> Trie a
fromList = foldl (\trie (key, val) -> insert key val trie) empty
