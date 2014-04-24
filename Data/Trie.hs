module Data.Trie
    (
      Trie  
    , empty
    , singleton
    , null
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
    , member
    , dom
    , ran
    , update
    , removeSet
    , restrict
    , pointwise
    , pointwiseDom
    , pointwiseRan
    , mutually
    , mutuallyDom
    , mutuallyRan
    ) where

import Prelude hiding (null)
import Data.Maybe (fromMaybe, fromJust, isJust)
import qualified Data.Set as S
import qualified Data.AList as A

data Trie a = Node (Maybe a) (A.AList (Trie a))
            deriving (Show)

empty :: Trie a
empty = Node Nothing []

singleton :: String -> a -> Trie a
singleton key value = insert key value empty

null :: Trie a -> Bool
null (Node _ []) = True
null _           = False

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

member :: String -> Trie a -> Bool
member key trie = isJust (lkup key trie)

dom :: Trie a -> S.Set String
dom = foldTK (\key _ set -> S.insert key set) S.empty

ran :: Ord a => Trie a -> S.Set a
ran = foldT S.insert S.empty

update :: Trie a -> Trie a -> Trie a
update = insertTrie

removeSet :: S.Set String -> Trie a -> Trie a
removeSet keys trie = S.foldr remove trie keys

restrict :: Trie a -> S.Set String -> Trie a
restrict trie keys = filterTK (\k _ -> k `S.member` keys) trie

pointwise :: ((String, a) -> Bool) -> Trie a -> Bool
pointwise p trie = foldT (&&) True $ mapTK (curry p) trie

pointwiseDom :: (String -> Bool) -> Trie a -> Bool
pointwiseDom p = pointwise (\(k, _) -> p k)

pointwiseRan :: (a -> Bool) -> Trie a -> Bool
pointwiseRan p  = pointwise (\(_, v) -> p v)

mutually :: Eq a => ((String, a) -> (String, a) -> Bool) -> Trie a -> Bool
mutually p trie = and [p a1 a2 | a1 <- asList trie, a2 <- asList trie, a1 /= a2]

mutuallyDom :: Eq a => (String -> String -> Bool) -> Trie a -> Bool
mutuallyDom p = mutually (\(k1, _) (k2, _) -> p k1 k2)

mutuallyRan :: Eq a => (a -> a -> Bool) -> Trie a -> Bool
mutuallyRan p = mutually (\(_, v1) (_, v2) -> p v1 v2)
