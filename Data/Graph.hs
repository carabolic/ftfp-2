module Data.Graph (
      Graph
    , Vertex
    , Edge
    , empty
    , singleton
    , vertices
    , edges
    , addEdge
    , addVertex
    , fromList
    , neighbours
    , inSet
    , outSet
    ) where

--import Data.List (foldr')
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Trie as T

type Vertex = String
type Edge = (Vertex, Vertex)
type Graph = T.Trie (S.Set Edge)

empty :: Graph
empty = T.empty

singleton :: Vertex -> Graph
singleton v = T.singleton v S.empty

vertices :: Graph -> S.Set Vertex
vertices = T.dom

edges :: Graph -> S.Set Edge
edges g = S.foldl' S.union S.empty $ T.ran g

addEdge :: Graph -> Edge -> Graph
addEdge g e@(v, w) 
    | not (null v) && null w       = addVertex g v
    | null v       && not (null w) = addVertex g w
    | not (null v) && not (null w) = addVertex (T.insert v es' g) w
    | otherwise                    = g
    where
        es = fromMaybe S.empty (T.lkup v g)
        es' = S.insert e es 

addVertex :: Graph -> Vertex -> Graph
addVertex g v = if g `contains` v then g else T.insert v S.empty g

contains t k = isJust $ T.lkup k t


fromList :: [Edge] -> Graph
fromList = foldr (flip addEdge) empty

neighbours :: Graph -> Vertex -> [Vertex]
neighbours g v = filter (/= v) $ map snd ns
    where
        ns = S.toList $ fromMaybe S.empty (T.lkup v g)

inSet :: Graph -> Vertex -> S.Set Vertex
inSet g v = S.map fst $ S.filter (\(_, v') -> v == v') $ edges g

outSet :: Graph -> Vertex -> S.Set Vertex
outSet g v = S.map snd $ S.filter (\(v', _) -> v == v') $ edges g
