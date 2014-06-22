module ShortestPath where

import Data.Maybe (fromMaybe)
import Data.POrd
import Data.CPO
import Data.Word
import Data.Graph
import Control.Fixpoint

import qualified Data.Set as S
import qualified Data.Trie as T

data Dist = Unreachable | Dist Word deriving (Eq, Show)

instance Ord Dist where
    Dist a      <= Dist b      = a <= b
    _           <= Unreachable = True
    Unreachable <= Dist _      = False

instance POrd Dist where
    le (Dist a) (Dist b)     = a `le` b
    le Unreachable _         = True
    le _         Unreachable = False

instance CPO Dist where
    bottom = Unreachable
    lub (Dist a)    (Dist b)    = Dist (lub a b)
    lub Unreachable b           = b
    lub a           Unreachable = a

addOne :: Dist -> Dist
addOne (Dist n)    = Dist (n + 1)
addOne Unreachable = Unreachable

shortestPath :: Graph -> Vertex -> T.Trie Dist
shortestPath g s = condFixpoint (next g s) (initialize g s)

next :: Graph -> Vertex -> T.Trie Dist -> T.Trie Dist
next g s sp = S.foldl' lub sp updateDist
    where
        -- all vertices except the start vertex
        v' = s `S.delete` vertices g
        -- returns all vertices @u@ where @(u,v) `elem` edges@
        ins = inSet g
        increment v = addOne $ lkupUnsafe sp v
        -- returns the minimum distance or @Unreachable@
        dist = S.findMin . S.union (S.singleton Unreachable) . S.map increment . ins
        updateDist = S.map (\v -> T.singleton v (dist v)) v'

initialize :: Graph -> Vertex -> T.Trie Dist
initialize g s = knownDists `lub` unknownDists
    where
        knownDists = T.singleton s (Dist 0)
        unknownDists = buildTrie (const Unreachable) (vertices g)

buildTrie :: (String -> a) -> S.Set String -> T.Trie a
buildTrie f = S.foldl' (\t e -> T.insert e (f e) t) T.empty

lkupUnsafe :: T.Trie a -> String -> a
lkupUnsafe t k = fromMaybe (error ("Unable to find " ++ k)) $ T.lkup k t

testGraph :: Graph
testGraph = fromList 
    [
      ("a", "d"), ("a", "b")
    , ("b", "c")
    , ("c", "b")
    , ("d", "e")
    , ("e", "b"), ("e", "c")
    ]
